library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(Quandl)
library(forcats)
library(tidyverse)
library(norm)
library(knitr)
library(ROCR)

library(okcupiddata)
data("profiles")

profiles <- profiles %>% 
  filter(!is.na(height)) %>% 
  mutate(is_female=ifelse(sex=='f', 1, 0)) %>% 
  tibble::rownames_to_column(var="ID") %>%
  select(ID, age, is_female, height)

#split into train and test
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")


#ROC curve: random guesses
n_folds <- 2
random_train <- profiles_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  mutate(p_hat = ifelse(fold==1, 1, 0)) %>%
#  mutate(p_hat = ifelse(is_female=1 & fold==2, 0, 1)) 
  select(is_female, p_hat)

rand <- prediction(predictions = random_train$p_hat, labels = random_train$is_female)
rando <- performance(rand, "tpr","fpr")

random <- as.numeric(performance(rand,"auc")@y.values)

plot(rando, main=paste("Area Under the Curve =", round(random, 3)))
abline(c(0, 1), lty=2)


#ROC curve perfect guesses
predict_train <- profiles_train %>%
  mutate(
    p_hat = is_female
  ) %>%
  select(is_female, p_hat)

predi <- prediction(predictions = predict_train$p_hat, labels = predict_train$is_female)
perfi <- performance(predi, "tpr","fpr")

auci <- as.numeric(performance(predi,"auc")@y.values)

plot(perfi, main=paste("Area Under the Curve =", round(auci, 3)))
abline(c(0, 1), lty=2)


#Part 3
#build the model
model_formula <- as.formula(is_female~height+age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

#test the model
predictions<- model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )

#how did the model do?
prophecy <- predictions %>%
  mutate(guess = ifelse(p_hat>=.346, 1, 0))

errors <- prophecy %>%
  mutate(correct = ifelse(guess==is_female, 1, 0))

sexpropwrong <- errors %>%
  group_by(is_female) %>%
  summarize(prop_wrong=1-mean(correct))
kable(sexpropwrong)

#using guess and check, p_hat>=.346 predicts females wrong about half as often as males.

