#AUC for random guesses is .895
#AUC for perfect model is .503
# p* = 0.282. Cost = 0.2379410
# weeeeeeee I did this almost all by myself except Brenda helped me a little at line 330

library(tidyverse)
library(broom)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")
View(profiles)
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

random_profiles <- profiles %>% mutate(random=(sample(0:1, 59914, replace=T)))

# Exploratory plot. It seems there might be predictive power in height. But how
# much?
ggplot(profiles, aes(x=sex, y=height)) +
  geom_boxplot() +
  coord_flip()


#RANDOM
# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- random_profiles %>%
  sample_frac(0.5)

profiles_test <- random_profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(random~height)
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





# Random ROC Curve --------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(profiles_train)
#phat = probability

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

#AUC for random guesses is .895
#AUC for perfect model is .503

#Perfect
# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- random_profiles %>%
  sample_frac(0.5)

profiles_test <- random_profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
perfect_model_formula <- as.formula(sex~height)
perfect_model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

# Recall there have been generally two ways in this class to obtain predictions
# based on a model: using predict(), or using augment(). Let's look at the first
# 20:





# 1. Predictions using predict() ---------------------------------------------
# predict() for logistic regression returns fitted values in log-odds space
# i.e. values from (-infty, infty)
predict(perfect_model_logistic, newdata = profiles_test)[1:10] %>% round(3)

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


# Perfect ROC Curve --------------------------------------------------------------------
# Let's use the ROCR package to create our ROC curves. The data is unfortunately
# not in tidy format, so we can't plot using ggplot() or use tidyverse tools:

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(profiles_train)
#phat = probability

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

#AUC for perfect model is .503



#question 3
library(tidyverse)
library(broom)
# We'll predict the sex (binary) of 60K OkCupid users using only their height
# https://github.com/rudeboybert/okcupiddata
library(okcupiddata)
data("profiles")
View(profiles)
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

# Exploratory plot. It seems there might be predictive power in height. But how
# much?
ggplot(profiles, aes(x=sex, y=age)) +
  geom_boxplot() +
  coord_flip()



# Example of Training Logistic Model and Making Predictions --------------------
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~height + age)
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

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(profiles_train)


p_star_values <- seq(.001, .999, 0.001)
p_star_values


costs <- rep(0, length(p_star_values))

for(j in 1:length(p_star_values)){

p2 <- profiles_train %>% mutate(y_hat =ifelse(p_hat>p_star_values[j], 1, 0)) %>% 
  mutate(cost=0) %>% 
    mutate(cost =ifelse(y==1 & y_hat==0, 2, cost)) %>% 
       mutate(cost =ifelse(y==0 & y_hat==1, 1, cost)) 
 
p3 <- as.numeric(summarize(p2, mean = mean(cost, na.rm = TRUE)))

costs[j]<-p3 #I didn't know how to make a vector to store things until Brenda showed me.
}

results <- data_frame(
  Cost = costs,
  p_star = p_star_values)













