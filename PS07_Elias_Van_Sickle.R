library(tidyverse)
library(broom)
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
  select(ID, sex, y, height) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

#Split into train and test data
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

#Question #1. //////////////////////RANDOM GUESSES////////////////////////////////////

# Create vector to store random numbers, which will be the p_hat values
length_train <- length(profiles_train$ID)
randVector <- rep(0, length_train)

for(i in 1:length_train) {
  randNum <- runif(1,0,1) 
  randVector[i] = randNum %>% 
  round(3)
}

# Add p_hat column to profiles dataframe
profiles_train <- profiles_train %>% 
  mutate(rand_p_hat = randVector) %>% 
  select(y, rand_p_hat)

# ROC Curve --------------------------------------------------------------------
library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$rand_p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


#WHAT IS THE AREA UNDER THE CURVE IN THE RANDOM CASE?
#In this case the area under the curve = approx 0.5, which makes sense given that it was totally random

#Question #2. //////////////////////PERFECT GUESSES////////////////////////////////////
perfectVector <- rep(0, length_train)

for(i in 1:length_train) {
  if(profiles_train$y[i] == 1) {
    perfectVector[i] <- 1
  }
  else {
    perfectVector[i] = 0
  }
}


profiles_train <- profiles_train %>% 
  mutate(perfect_p_hat = perfectVector)

# ROC Curve --------------------------------------------------------------------
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$perfect_p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#WHAT IS THE AREA UNDER THE CURVE IN THE RANDOM CASE?
#In this case the area under the curve = 1, which makes sense given that it was perfect guesses
#I.e. there are no false positives.  There are only true positives




#//////////////////////////////////Question #3//////////////////////////////////////

#If it is twice as costly to incorrectly predict that a user is male when they are actually female 
#than to incorrectly predict that a user is female when they are actually male
#I.e. False positive is twice as costly as False negative


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
View(profiles)

# Split into train and test --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~height + age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")


# 1. Predictions using predict() ---------------------------------------------
# predict() for logistic regression returns fitted values in log-odds space
# i.e. values from (-infty, infty)

#predict(model_logistic, newdata = profiles_test, type="response")[1:10] %>% round(3)



# ROC Curve --------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

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

# False Positive, False Negative, and P_Star respectively:
fp <- unlist(pred@fp)
fn <- unlist(pred@fn)
p_star <- unname(unlist(pred@cutoffs))


#Initialize Min Cost to the first value in fn/fp
minCost <- 2*fp[1] + fn[1]
optimal_p_star <- p_star[1]

for(i in 2:length(fn)) {
  cost <- 2*fp[i] + fn[i]
  if(cost < minCost) {
    minCost = cost
    optimal_p_star = p_star[i]
  }
}

#Optimal p* roughly equal to 0.677
optimal_p_star



