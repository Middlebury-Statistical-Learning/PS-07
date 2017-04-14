library(tidyverse)
library(broom)
library(ROCR)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")

# Data cleaning:
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, height, age, y) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

##Random Guesses 
profiles_train_random <- profiles
random_predictions <- rep(0, times=nrow(profiles_train_random))
for(i in 1:nrow(profiles_train_random)) {
  random_predictions[i] <- sample(0:1000,1)/1000
}
profiles_train_random <- profiles_train_random %>%
  mutate(
    p_hat = random_predictions
  ) %>%
  select(y, p_hat)

# ROC Curve for random guesses 

# This bit of code computes the ROC curve
pred_random <- prediction(predictions = profiles_train_random$p_hat, labels = profiles_train_random$y)
perf_random <- performance(pred_random, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_random <- as.numeric(performance(pred_random,"auc")@y.values)
auc_random

# This bit of code prints it
plot(perf_random, main=paste("Area Under the Curve =", round(auc_random, 3)))
abline(c(0, 1), lty=2)


##Perfect Guesses
profiles_train_perfect <- profiles %>% 
  mutate(
    p_hat = y
  ) %>%
  select(y, p_hat)

# ROC Curve for perfect guesses 

# This bit of code computes the ROC curve
pred_perfect <- prediction(predictions = profiles_train_perfect$p_hat, labels = profiles_train_perfect$y)
perf_perfect <- performance(pred_perfect, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_perfect<- as.numeric(performance(pred_perfect,"auc")@y.values)
auc_perfect

# This bit of code prints it
plot(perf_perfect, main=paste("Area Under the Curve =", round(auc_perfect, 3)))
abline(c(0, 1), lty=2)


## Cost function question: cost = 2(false negative) + (false positive)

# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")
profiles_model <- profiles_test %>% 
  mutate(p_hat=predict(model_logistic, newdata = profiles_test, type="response"))

## make a vector to test p*
n_test_values <- 100 
p_star_values<- rep(0,n_test_values)
for(i in 1:n_test_values){
  p_star_values[i]<-i/n_test_values
}

## make cost vector to store total cost for for each p*
costs<-rep(0,n_test_values)
for(i in 1:n_test_values){
  total_cost<-profiles_model %>% 
  mutate(y_hat=ifelse(p_hat<p_star_values[i],0,1)) %>% 
  mutate(cost=ifelse(y==y_hat,0, ifelse(y<y_hat,1,2))) %>% 
  summarise(total_cost=sum(cost))
  costs[i]<-total_cost$total_cost
}

optimize_p_star <- tbl_df(costs) %>% 
  mutate(p_star_values=p_star_values) %>% 
  rename(cost=value)

##if we want to minimize cost then p*=0.29





