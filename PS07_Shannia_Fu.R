library(tidyverse)
library(broom)
library(okcupiddata)
library(ROCR)
data("profiles")

##### RANDOM GUESSES #####

profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>% 
  mutate(ID = 1:n()) %>%
  mutate(p_rand_guess = runif(nrow(profiles), 0.0, 1.0))

# ROC curve
pred <- prediction(predictions = profiles$p_rand_guess, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# Plot
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


##### PERFECT GUESSES #####
profiles <- profiles %>% 
  mutate(p_perfect_guess = y)
# ROC curve
pred <- prediction(predictions = profiles$p_perfect_guess, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# Plot
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


##### AGE AND HEIGHT #####
profiles <- profiles %>% 
  select(ID, sex, y, height, age, p_rand_guess, p_perfect_guess) %>%
  filter(height>50) %>% 
  na.omit

model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles, family="binomial")
profiles <- profiles %>% 
  mutate(p_age_height_guess = round(predict(model_logistic, type="response"), 3))


pred <- prediction(predictions = profiles$p_age_height_guess, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# Plot
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)




##### AGE AND HEIGHT #####
# set min error as highest possible so comparatively the actual min error will be
# lower
min_error <- Inf

# list of p* values to test
p_star_vals <-  seq(0.001, 0.999, 0.001)

# set optimal p_star as 0 (a value that will isn't even hit)
p_star <- 0

# this was used for a different method--storing all p_stars and their
# outcomes and then finding the min from that dataframe
# ratios <- c()
# errors <- c()


# test all possible p* values
for (i in p_star_vals){
  # mutate using probabilities > p*
  profiles <- profiles %>% 
    mutate(age_height_gender=ifelse(p_age_height_guess > i, 1, 0))
  
  # create contingency table
  table <- profiles %>% 
    select(y, age_height_gender) %>% 
    group_by(y, age_height_gender) %>% 
    summarise(num = n())
  
  # calculate false neg/false pos
  false_neg <- table$num[3]/(table$num[3] + table$num[4])
  false_pos <- table$num[2]/(table$num[1] + table$num[2])
  
  # used for the different method -- more storage
  # ratios <- c(ratios, false_neg/false_pos)
  # errors <- c(errors, (table$num[2] +table$num[3])/sum(table$num))
  
  if(false_neg/false_pos < 0.5 & ((table$num[2] +table$num[3])/sum(table$num)) < min_error){
    min_error = false_neg + false_pos
    p_star = i
  }
}

# find optimal p*
p_star

# I got ~ 0.341 as p*

# see minimum from a dataframe
# df <- data_frame(p_star_vals, ratios, errors)
# 
# p_star_outcomes <- df %>% filter(ratios < 0.5)


