library(tidyverse)
library(broom)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")
library(ROCR)

# Data cleaning:
profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height) %>%
  # Remove all rows with NA missing values:
  na.omit() 
  

#PART I:
profiles_random_guesses <- profiles %>%
  mutate(p_hat = runif(n(), 0, 1)) %>%
  select(y,p_hat)

# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_random_guesses$p_hat, labels = profiles_random_guesses$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)



#PART II:
profiles_perfect_guesses <- profiles %>%
  mutate(p_hat = y) %>% 
  select(y,p_hat)

# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_perfect_guesses$p_hat, labels = profiles_perfect_guesses$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#PART III:

# Data cleaning:
data("profiles")

profiles <- profiles %>%
  tbl_df() %>%
  # Create binary outcome variable y:
  mutate(y=ifelse(sex=="f", 1, 0)) %>%
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height,age) %>%
  # Remove all rows with NA missing values:
  na.omit() 

# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command. I added age
model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")




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

#find p* 
#The value of p* should be around the value of true positive rate 0.6 and the false positive rate corresponding to that FPR. 
#Using those values, we would be able to calculate the FPR




