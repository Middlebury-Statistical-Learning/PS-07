# Load Packages
library(tidyverse)
library(broom)
library(ROCR)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")

profiles <- profiles %>%
  tbl_df() %>%
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>% 
  na.omit()

# Question 1
# (Generate a vector of random guesses, and plot the ROC.)
nrows <- as.numeric(count(profiles))
random_guesses <- as.vector(runif(nrows,0,1))
pred <- prediction(predictions = random_guesses, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)
                          

# Question 2
# (Generate a vector of the correct p hats, and plot the ROC.)
nrows <- as.numeric(count(profiles))
perfect_guesses <- as.vector(profiles$y)
pred <- prediction(predictions = perfect_guesses, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# Question 3
model_logistic <- glm(y~ age+height, data=profiles, family="binomial")
profiles <- profiles %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  )
pred <- prediction(predictions = profiles$p_hat, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# Figure out the lowest p_hat value associated with y=1
min(profiles %>% filter(y==1) %>% select(p_hat))

# Since the minimum p-hat value associated with any of the females is 0,
# we should make the p-star value 0, because it is cheaper to incorrectly predict
# a male as female, than to predict a female as male.
