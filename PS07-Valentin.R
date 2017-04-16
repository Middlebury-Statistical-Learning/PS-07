library(tidyverse)
library(broom)
library(okcupiddata)
library(ROCR)
data("profiles")


# Male = 0 | Female = 1
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


#Length of the Profiles datafram
size <- 59914

#Either 0 or 1
sample_space <- c(0, 1)
probability <- 0.5

coin_flips <- sample(sample_space, size = size, replace=TRUE, 
                     prob = c(probability, 1 - probability)) 


#random sex guesses
random_sex_guess <- profiles %>% mutate(p_hat = coin_flips)

#perf guesses based off the data set
perf_sex_guess <- profiles %>% mutate(p_hat = ifelse(profiles$sex == 'm', 1, 0))

#ROC Curve - Random
pred_guess <- prediction(predictions = random_sex_guess$p_hat, labels = profiles$y)
perf_guess <- performance(pred_guess, "tpr","fpr")


# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#ROC Curve - Perfect
pred_perf <- prediction(predictions = perf_sex_guess$p_hat, labels = profiles$y)
perf_perf <- performance(pred_perf, "tpr","fpr")


# This bit of code computes the Area Under the Curve
auc <- as.numeric(performance(pred,"auc")@y.values)

# This bit of code prints it
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)

#The thresholds we are testing
p_attempts_vector = seq(from = 0.05, to = 0.5, by=0.025)

#Calculating the TPR & FPR - for guess
tpr_guess <- pred_guess@tp[[1]]/max(pred_guess@tp[[1]])
fpr_guess <- pred_guess@fp[[1]]/max(pred_guess@fp[[1]])

#Calculating the TPR & FPR - for perf
tpr_guess <- pred_perf@tp[[1]]/max(pred_perf@tp[[1]])
fpr_guess <- pred_perf@fp[[1]]/max(pred_perf@fp[[1]])

#“Predict a user to be female if p^>p∗”?
# Male - 0 | Female - 1 therefore 1 is a prediction
# Lower the number of Male predictions on average, so we lower the p* value
# to a lower threshold than 0.5, since we want more female i.e.
# (want to decrease the false negative rate/false positives)
for (i in p_attempts_vector) {
  # plug in i values and check the tpr and fpr each time and see if the values 
  # increased or decreased from the predictive model
  # if we reach a point where fp rates reaches a point where we are achieving 
  # roughly the same number of correct predictions without an increase in fpr. 
}


