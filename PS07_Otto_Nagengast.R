########################################################################################
# Question 1                                                                           #
# Generating predictions randomly (i.e. without any information about the predictors). #
# Note: the results will be the same if you set p_hat = 0 or p_hat = 1 or p_hat = 0.5  #
########################################################################################

profiles_train_random <- profiles_train %>% 
 mutate(
   p_hat = 0.5 
  ) %>%  
  select(y, p_hat)
View(profiles_train_random)

library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train_random$p_hat, labels = profiles_train_random$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


## The area under the curve = 0.5. ##

##############################################
# Question 2                                 #
# Generating completely correct predictions. #
##############################################

profiles_train_correct <- profiles_train %>% 
  mutate(
    p_hat = y
  ) %>%  
  select(y, p_hat)
View(profiles_train_correct)

library(ROCR)
# This bit of code computes the ROC curve
pred <- prediction(predictions = profiles_train_correct$p_hat, labels = profiles_train_correct$y)
perf <- performance(pred, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

## The area under the curve = 1. ##

##############
# Question 3 #
##############

## Approach 1 ##
# Goal: find the total cost associated with each p_star #

# Generating predictions  
profiles <- profiles %>%
  mutate(
    p_hat = predict(model_logistic, newdata=profiles, type="response"),
    p_hat = round(p_hat, 3)
  )

# Assigning values for p_star  
p_star <- seq(from=0, to=1, by=0.01)

# Creating a vector to store the cost at each p_star 
cost <- rep(0, 101)

# Writing a for loop to find y_hat for each value of p_star 
for (i in 1:length(p_star)) {
  # Generate y_hat   
  profiles <- profiles %>% 
    mutate(y_hat = ifelse(p_hat > p_star[i], 1, 0)) 
  
  # Generate indicator for positives 
  profiles <- profiles %>% 
    mutate(positive = ifelse(y==1, 1, 0)) 
  
  # Generate indicator for false positives 
 profiles <- profiles %>% 
    mutate(false_positive = ifelse(y_hat==1 & y==0, 1, 0)) 
  
  # Generate indicator for false negatives 
  profiles <- profiles %>% 
    mutate(false_negative = ifelse(y_hat==0 & y==1, 1, 0)) 
  
  # Calculate costs 
  profiles <- profiles %>% 
    mutate(costs = ifelse(false_negative==1, 2, 0)) %>% 
    mutate(costs = replace(costs, false_positive == 1, 1))
  
  # Storing the total cost 
  cost[i] <- profiles %>% 
    summarise(sum(costs))
}

cost 

# The lowest cost is achieved when p_star is around .3. # 




