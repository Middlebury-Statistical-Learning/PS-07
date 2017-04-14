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
  select(ID, sex, y, height, age) %>%
  # Remove all rows with NA missing values:
  na.omit()
View(profiles)

# Exploratory plot. It seems there might be predictive power in height. But how
# much?
ggplot(profiles, aes(x=sex, y=height)) +
  geom_boxplot() +
  coord_flip()


#--random predictions--


profiles_random <- profiles %>%
  mutate(p_hat = runif(n(), 0, 1))
glimpse(profiles_random)

#make the ROC curve
pred <- prediction(predictions = profiles_random$p_hat, labels = profiles_random$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


#--perfect--
profiles_perfect <- profiles %>%
  mutate(p_hat = y)
glimpse(profiles_perfect)


#make the ROC curve
pred <- prediction(predictions = profiles_perfect$p_hat, labels = profiles_perfect$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)



#--part 3 --
#visualize problem 
profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

#add age into formula
model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

profiles_train <- profiles_train %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)

# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train$p_hat, labels = profiles_train$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


#So if its worse to have a woman identified as a male, we should make it easier to predict female. As a result
#we should lower the threshold, which will predict more females.