#####################################################
# Problem Set 7 - Math 218
# Author: Marcos Barrozo
#####################################################
#setting up environment and cleaning data as in logistic_regression.R
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
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height, age) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)


# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")


# ROC Curve -"Random model"-------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

profiles_train_random <- profiles_train %>%
  mutate(
    p_hat = 0.5  #a coin toss, a person is 50/50 likely to be male/female
  ) %>%
  select(y, p_hat)

# Compute ROC
pred_random <- prediction(predictions = profiles_train_random$p_hat, labels = profiles_train_random$y)
perf_random <- performance(pred_random, "tpr","fpr")
# Compute area under curve
auc_random <- as.numeric(performance(pred_random,"auc")@y.values)
auc
#Print it!
plot(perf_random, main=paste("Area Under the Curve =", round(auc_random, 3)))
abline(c(0, 1), lty=2) #area under curve is 0.5, follows the diagonal line


# ROC Curve -"Perfect model"-------------------------------------------------------------------
# Train logistic regression - similar to logistic_regression.R, but with age as an additional control
model_formula <- as.formula(y~height + age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

#Creating the curve
profiles_train_perf <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
# Compute ROC
pred_perf <- prediction(predictions = profiles_train_perf$p_hat, labels = profiles_train_perf$y)
perf_perf <- performance(pred_perf, "tpr","fpr")
# Compute area under curve
auc_perf <- as.numeric(performance(pred_perf,"auc")@y.values)
auc
#Print it!
plot(perf_perf, main=paste("Area Under the Curve =", round(auc_perf, 3)))
abline(c(0, 1), lty=2) #area under curve is 0.5, follows the diagonal line


#PART 3 - Hypothetical situation where a false negative is twice as bad as a false positive
#Here, the marketing company's "indifference curve" would lie more or less
# where the false negative rate is half that of the false positive rate
# With that in mind, the value of p* would value depending on the proportion
# of females in the sample
# but it would be around .333 in a balanced dataset.
#In other words, if p>.333, predict female

#In this sample:
profiles %>% summarize(prop=mean(y))
#females comprise about 40% of the data,
# that results in a greater rate of false negatives (there are more males out there)
# at any p* level, compared to a more balanced sample

#This implies a slightly greater optimal p*, something closer to p*=.4