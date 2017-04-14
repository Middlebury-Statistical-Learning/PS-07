library(tidyverse)
library(broom)
library(okcupiddata)
library(ROCR)
library(plotly)

data("profiles")

# Data cleaning:
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height, age) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

# Exploratory plot. It seems there might be predictive power in height. But how
# much?
ggplot(profiles, aes(x=sex, y=height)) +
  geom_boxplot() +
  coord_flip()

## Train and Test Sets

profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# 1) Random Model 

profiles_train_f <- profiles_train %>% 
  sample_frac(0.5) %>% 
  mutate(y_hat = 1) 

profiles_train_f <- profiles_train_f %>% 
  mutate(p_hat = runif(nrow(profiles_train_f), 
                       min = 0.51,
                       max = 0.99))

profiles_train_m <- profiles_train %>%
  anti_join(profiles_train_f, by="ID") %>% 
  mutate(y_hat = 0)

profiles_train_m <- profiles_train_m %>% 
  mutate(p_hat = runif(nrow(profiles_train_m), 
                       min = 0.01,
                       max = 0.49))

profiles_train_guess <- rbind(profiles_train_f, profiles_train_m)

## Gen ROC curve for random guesses 

# This bit of code computes the ROC curve
pred_1 <- prediction(predictions = profiles_train_guess$p_hat, labels = profiles_train_guess$y)
perf_1 <- performance(pred_1, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_1 <- as.numeric(performance(pred_1,"auc")@y.values)
auc_1

# This bit of code prints it
plot(perf_1, main=paste("Area Under the Curve =", round(auc_1, 3)))
abline(c(0, 1), lty=2)

## Now, make perfect predictions 

profiles_train_perfect <- profiles_train %>% 
  mutate(y_hat = y) 
  
profiles_train_perfect <- profiles_train_perfect %>% 
  mutate(p_hat = ifelse(y_hat == 1, 0.99, 0.01))
                    
# This bit of code computes the ROC curve
pred_2 <- prediction(predictions = profiles_train_perfect$p_hat, labels = profiles_train_perfect$y)
perf_2 <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_2 <- as.numeric(performance(pred_2,"auc")@y.values)
auc_2

# This bit of code prints it
plot(perf_2, main=paste("Area Under the Curve =", round(auc_2, 3)))
abline(c(0, 1), lty=2)

# Logit Model

model_logit <- glm(data = profiles_train, y ~ height + age, family = "binomial")

profiles_test_logit <- profiles_test %>% 
  mutate(
    p_hat = predict(model_logit, type="response"),
    p_hat = round(p_hat, 3)
  ) 

# This bit of code computes the ROC curve
pred_3 <- prediction(predictions = profiles_test_logit$p_hat, labels = profiles_test_logit$y)
perf_3 <- performance(pred_3, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_3 <- as.numeric(performance(pred_3,"auc")@y.values)
auc_3

# This bit of code prints it
plot(perf_3, main=paste("Area Under the Curve =", round(auc_3, 3)))
abline(c(0, 1), lty=2)

## choosing P* to make it to conservatively predict females

hist_p_hat <- ggplot(data = profiles_test_logit, aes(p_hat))+
  geom_histogram()
hist_p_hat

## make a contingency table for given p_star
## incorrect male classifications are twice as bad

threshold_1 <- 0.5

table_1 <- profiles_test_logit %>% 
  arrange(desc(p_hat)) %>% 
  mutate(y_hat = ifelse(p_hat > threshold_1, 1, 0))

table_1 <- table_1 %>% 
  mutate(class = ifelse(y_hat == 0 & y == 0, "TN",
                    ifelse(y_hat == 1 & y == 0, "FP",
                    ifelse(y_hat == 0 & y == 1, "FN",
                    ifelse(y_hat == 1 & y == 1, "TP", "NA")
                            )
                        )
                    )
         )

table_1 <- table_1 %>% 
  group_by(class) %>% 
  tally() 
table_1

## think about how to score this and then incorporate the weights in the score

## use the prop incorrect (FP + FN)/N, but with weights

TP <- subset(table_1, table_1$class=="TP")
TP$n

FP <- subset(table_1, table_1$class=="FP")
FP$n

TN <- subset(table_1, table_1$class=="TN")
TN$n

FN <- subset(table_1, table_1$class=="FN")
FN$n

weighted_prop_incorrect = (0.67*FN$n + 0.33*FP$n)/(TP$n + TN$n + FP$n + FN$n)
weighted_prop_incorrect

## Cross Validate to optimize p_star-----------------


thresholds <- c(seq(.01, .99, by = .01))
length(thresholds)
scores <- c(rep(0, 99))

for(i in 1:length(thresholds)){
  c_table <- profiles_test_logit %>% 
    arrange(desc(p_hat)) %>% 
    mutate(y_hat = ifelse(p_hat > thresholds[i], 1, 0))

  c_table <- c_table %>% 
    mutate(class = ifelse(y_hat == 0 & y == 0, "TN",
                        ifelse(y_hat == 1 & y == 0, "FP",
                        ifelse(y_hat == 0 & y == 1, "FN",
                        ifelse(y_hat == 1 & y == 1, "TP", 
                               "NA")
                               )
                        )
                  )
              )

  c_table <- c_table %>% 
    group_by(class) %>% 
    tally() 
  c_table

  TP <- subset(c_table, c_table$class=="TP")
  TP$n

  FP <- subset(c_table, c_table$class=="FP")
  FP$n

  TN <- subset(c_table, c_table$class=="TN")
  TN$n

  FN <- subset(c_table, c_table$class=="FN")
  FN$n

  weighted_prop_incorrect = (0.67*FN$n + 0.33*FP$n)/(TP$n + TN$n + FP$n + FN$n)
  scores[i] <- weighted_prop_incorrect
}

p_stars <- cbind(thresholds, scores) %>% 
  tidy() %>% 
  tbl_df()

p_star_plot <- ggplot(data = p_stars, aes(x = thresholds, y = scores))+
  geom_point()
ggplotly(p_star_plot)

## I would choose around .34 for my threshold 

props_sex <- profiles %>% 
  arrange(sex) %>% 
  group_by(sex) %>% 
  tally()
props_sex

## my graph doesn't make much sense. 
## I would expect a "U" shape
