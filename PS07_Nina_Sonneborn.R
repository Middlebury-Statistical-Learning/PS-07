# Problem Set 7 Info -----
# 1. Create the ROC curve corresponding to a model that makes random guesses as reflected in p hat. 
# What is the area under the ROC curve?
# 2. Create the ROC curve corresponding to a model that makes perfect guesses as reflected in p hat. 
# What is the area under the ROC curve?
# 3. Say you are trying to predict sex == female using height and age as predictor variables
# for the purposes of advertisement targeting. Marketing feels that it is twice as costly
# to incorrectly predict a user to be male when they are really female than to 
# incorrectly predict a user to be female when they are really male. 
# Roughly what threshold p∗p∗ should you use in the decision rule:
# “Predict a user to be female if p_hat > p∗”?

# Load libraries and data --------------------------------------
library(tidyverse)
library(broom)
library(ROCR)
library(okcupiddata)
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


# 1. Random guesses model ----------------
# Create the ROC curve corresponding to a model that makes random guesses as reflected in p hat. 
# What is the area under the ROC curve?


random_predictions <- rep(0, times = nrow(profiles))
for(i in 1:nrow(profiles)){
  random_predictions[i] <- sample(0:1000, 1)/1000
}

# ROC Curve
# This code is modified from logistic_regression.R

profiles_random <- profiles %>%
  mutate(
    p_hat = random_predictions
  ) %>%
  select(y, p_hat)
View(profiles_random)

# This bit of code computes the ROC curve
pred_random <- prediction(predictions = profiles_random$p_hat, labels = profiles_random$y)
perf_random <- performance(pred_random, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_random <- as.numeric(performance(pred_random,"auc")@y.values)
auc_random

# This bit of code prints it
plot(perf_random, main=paste("Area Under the Curve =", round(auc_random, 3)))
abline(c(0, 1), lty=2)



# 2. Perfect guesses model ----------------
# Create the ROC curve corresponding to a model that makes perfect guesses as reflected in p hat. 
# What is the area under the ROC curve?

profiles_perfect <- profiles %>%
  mutate(
    p_hat = y
  ) %>%
  select(y, p_hat)
View(profiles_train_perfect)

# This bit of code computes the ROC curve
pred_perfect <- prediction(predictions = profiles_perfect$p_hat, labels = profiles_perfect$y)
perf_perfect <- performance(pred_perfect, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_perfect <- as.numeric(performance(pred_perfect,"auc")@y.values)
auc_perfect

# This bit of code prints it
plot(perf_perfect, main=paste("Area Under the Curve =", round(auc_perfect, 3)))
abline(c(0, 1), lty=2)


# 3. Hypothetical p_hat----------------------
# Say you are trying to predict sex == female using height and age as predictor variables
# for the purposes of advertisement targeting. Marketing feels that it is twice as costly
# to incorrectly predict a user to be male when they are really female than to 
# incorrectly predict a user to be female when they are really male. 
# Roughly what threshold p∗ should you use in the decision rule:
# “Predict a user to be female if p_hat > p∗”?

# First, create training and test data
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Create model that predicts sex based on age and height
model_logistic <- glm(y~age+height, data=profiles_train, family="binomial")

# Make a vector of p* values to test
n_test_values <- 10000
p_star_tests <- rep(0, n_test_values)
for(i in 1:n_test_values){
  p_star_tests[i] <- i/n_test_values
}

# Get predictions
profiles_model <- profiles_test %>%
  mutate(p_hat = predict(model_logistic, newdata = profiles_test, type = "response"))

# Make vector to save costs for each p*
costs <- rep(0, n_test_values)

# Loop finds the total cost for each p_star
for(i in 1:n_test_values){ # for each value of p*
    total_cost <- 
      profiles_model %>%
      # make predictions based on p*
      mutate(y_hat = ifelse(p_hat > p_star_tests[i], 1, 0)) %>%
      # calculate the cost of each prediction
      mutate(cost = ifelse(y == y_hat, 0, ifelse(y < y_hat, 1, 2))) %>%
      # sum up the costs
      summarise(total_cost = sum(cost))
    # save the total cost for this value of p*
    costs[i] <- total_cost$total_cost
}

# Merge p* vector and cost vector
optimize_cost <- p_star_tests %>% 
  tbl_df() %>% 
  mutate(cost = costs) %>%
  rename(p_star = value)

# Plot cost as a function of p*
ggplot(optimize_cost, aes(x = p_star, y = cost)) + geom_point()

# Find minimum cost
optimize_cost %>% arrange(cost) %>% slice(1)
# p_star =  0.2961 to 0.2980 (same cost)

