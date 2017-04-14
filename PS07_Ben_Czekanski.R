library(tidyverse)

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
  select(ID, sex, y, height) %>%
  # Remove all rows with NA missing values:
  na.omit()

profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

profiles_train <- profiles_train %>%
  mutate(p_hat_rand = sample(c(0, 1), nrow(profiles_train),replace=TRUE, prob = c(.5, .5))) %>%
  mutate(p_hat_correct = y)

library(ROCR)
# This bit of code computes the ROC curve
pred_rand <- prediction(predictions = profiles_train$p_hat_rand, labels = profiles_train$y)
perf_rand <- performance(pred_rand, "tpr","fpr")

pred_correct <- prediction(predictions = profiles_train$p_hat_correct, labels = profiles_train$y)
perf_correct <- performance(pred_correct, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_rand <- as.numeric(performance(pred_rand,"auc")@y.values)

auc_correct <- as.numeric(performance(pred_correct,"auc")@y.values)

# This bit of code prints it

plot(perf_rand, main=paste("Area Under the Curve Random =", round(auc_rand, 3)))

plot(perf_correct, main = paste("Area Under the Curve Random =", round(auc_correct, 3)))


# If it were twice as costly to wrongly predict male as was to wrongly predict female then I would
# want to err on the side of predicting male to the point where I wrongly predict female twice as 
# many times as I wrongly predict male. This corresponds to a p* of 2/3.

