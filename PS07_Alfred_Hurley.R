library(tidyverse)
library(broom)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")

#rand_p_hat is a vector full of random probabilities that wil be used for the random guesses
rand_p_hat <- rep(0, length(profiles$ID))

for (i in 1:length(profiles$ID)) {
  p_hat <- runif(1,0,1)
  rand_p_hat[i] <- p_hat
}
rand_p_hat <- rand_p_hat %>% round(3)

# Data cleaning:
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  mutate(p_hat = rand_p_hat) %>%
  select(ID, sex, p_hat, y) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)


library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles$p_hat, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#Area under curve is .5, precisely what we wanted as the true postive and false positive rates should be equal until the end with 50/50 odds)

#true_y is simply the actual y's
true_y <- profiles$y

profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  mutate(p_hat = true_y) %>%
  select(ID, sex, p_hat, y) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles$p_hat, labels = profiles$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#Area under curve is 1 (which it is expected to be as we get everything correct, i.e. 100% true positive rate)

# Data cleaning:
library(okcupiddata)
data("profiles")

# Data cleaning:
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height, age) %>%
  # Remove all rows with NA missing values:
  na.omit()

# Split into train and test --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

#create algorithm
model_formula <- as.formula(y~height + age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)


library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

# False Positive, False Negative, and P_Star respectively:
fp <- unlist(pred@fp)
fn <- unlist(pred@fn)
p_star <- unname(unlist(pred@cutoffs))

#create min_cost, which will be our goal, we want to minimize the size of cost
#which is the sum of twice the false negative and the false postive rate
#we multiply the false negative rate by two because of how it is twice as costly
min_cost <- 2*fn[1] + fp[1]
optimal_p_star <- p_star[1]

for(i in 2:length(fn)) {
  cost <- 2*fn[i] + fp[i]
  if(cost < min_cost) {
    min_cost = cost
    optimal_p_star = p_star[i]
  }
}

#optimal_p_star is around .32
optimal_p_star


