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
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

# Exploratory plot. It seems there might be predictive power in height. But how
# much?
ggplot(profiles, aes(x=sex, y=height)) +
  geom_boxplot() +
  coord_flip()



# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~height)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

# Recall there have been generally two ways in this class to obtain predictions
# based on a model: using predict(), or using augment(). Let's look at the first
# 20:





# 1. Predictions using predict() ---------------------------------------------
# predict() for logistic regression returns fitted values in log-odds space
# i.e. values from (-infty, infty)
predict(model_logistic, newdata = profiles_test)[1:10] %>% round(3)

# Recall from class notes that we fit the regression in log-odds space first,
# then convert back to probability space [0,1] using the inverse-logit function.
# These values below are now probabilities!
p_hat <- 1/(1+exp(-predict(model_logistic, newdata = profiles_test)[1:10]))
p_hat <- round(p_hat, 3)
p_hat

# You can do this automatically by setting type="response". Note how these two
# sets of values are identical:
predict(model_logistic, newdata = profiles_test, type="response")[1:10] %>% round(3)
p_hat





# 2. Predictions using augment() ---------------------------------------------
# The .fitted column below is the same as the output of predict(model_logistic)
# above: in log-odds space (-infty, infty)
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





# ROC Curve --------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

profiles_train_random <- profiles_train %>% 
  mutate(
    p_hat = sample(c(1,0), nrow(profiles_train), replace =TRUE)
  ) %>% 
  select(y, p_hat)
View(profiles_train_random)

profiles_train_perfect <- profiles_train %>%
  mutate(
    p_hat = y
  ) %>%
  select(y, p_hat)
View(profiles_train_perfect)



library(ROCR)
# This bit of code computes the ROC curve
pred_random <- prediction(predictions = profiles_train_random$p_hat, labels = profiles_train$y)
perf_random <- performance(pred, "tpr","fpr")

pred_perfect <- prediction(predictions = profiles_train_perfect$p_hat, labels = profiles_train$y)
perf_perfect <- performance(pred, "tpr","fpr")


# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred_random,"auc")@y.values)
auc

auc <- as.numeric(performance(pred_perfect,"auc")@y.values)
auc

# This bit of code prints it
plot(perf_random, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

plot(perf_perfect, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)







# 3) The p* will be closer to 0.33-0.35 because we're going to be measuring the p hat value to the p* value.
#in that sense, if 0 == male and 1 == female, this would help make sure that
#it wouldn't be twice as costly to incorrectly predict a user to be male when
#they are actually female, so false positive should be twice as high as false negative rates


