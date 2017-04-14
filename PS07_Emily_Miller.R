# PS07 Emily Miller

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(okcupiddata)
setwd("C:/Users/Emily Miller/Documents/Stat_learning")

data("profiles")

# Part 1: Random
mf_var <- c(0, 1)

# Data cleaning: Create random y variable
profiles <- profiles %>%
  tbl_df() %>%
   # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

profiles$y <- NA
nrows <- nrow(profiles)

# Create Random y
for (i in 1:nrows){
  rand_guess <- sample(mf_var, 1)
  profiles$y[i] <- rand_guess
}

# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~sex)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df()

# Again we can convert the fitted values to probabilities p_hat by using the
# inverse logit function:
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(profiles_train)


library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)








# Part 2: Perfect
rm(list = ls())
data("profiles")

# Part 1: Random
profiles$y <- NA

# Data cleaning: Create random y variable
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)


# Make Tran and Test models --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~sex)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df()

# Again we can convert the fitted values to probabilities p_hat by using the
# inverse logit function:
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(profiles_train)


library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# Part 3
# Roughly, 0.5 should be the threshold p??? should you use in 
# the decision rule: "Predict a user to be female if p^>p???"