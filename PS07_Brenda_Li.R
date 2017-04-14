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
  select(ID, sex, y, height,age) %>%
  # Remove all rows with NA missing values:
  na.omit()


# Part 1: Random Predictions
profiles <- profiles %>% 
  mutate(pRandom=runif(nrow(profiles),min=0,max=1))

predRandom <- prediction(predictions = profiles$pRandom, labels = profiles$y)
perfRandom <- performance(predRandom, "tpr","fpr")

aucRandom <- as.numeric(performance(predRandom,"auc")@y.values)
aucRandom

# The AUC for my random predictions model is 0.496 (or approximately 0.5)



# Part 2: Perfect Predictions
profiles <- profiles %>% mutate(p_hat_perfect=y) 

predPerfect <- prediction(predictions = profiles$p_hat_perfect, labels = profiles$y)
perfPerfect <- performance(predPerfect, "tpr","fpr")

aucPerfect <- as.numeric(performance(predPerfect,"auc")@y.values)
aucPerfect

# The AUC for my perfect predictions model is 1


# Part 3: False Negatives Twice as Costly as False Positives

model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles, family="binomial")

profiles <- profiles %>%
  mutate(
    p_hat_log = predict(model_logistic, type="response"),
    p_hat_log = round(p_hat_log, 3)
  ) 


p_star_values<-seq(0,1,0.001) #vector of different p_star_values
fn_to_fp_ratio<-numeric(length(p_star_values)) #vector to store ratios of false negative rate to false positive rate
error_rate<-numeric(length(p_star_values)) #vector to store error rates of each p_star_values


for (i in 1:length(p_star_values)){
  profiles<-profiles %>% 
    mutate(pred=ifelse(p_hat_log>p_star_values[i],1,0)) #make predictions given p_star_values
  
  contingency_table<-profiles %>%  #making a contingency table
    group_by(y,pred) %>% 
    summarise(n=n()) 
  
  false_neg_rate<-contingency_table$n[3]/(contingency_table$n[3]+contingency_table$n[4])
  false_pos_rate<-contingency_table$n[2]/(contingency_table$n[1]+contingency_table$n[2])
  fn_to_fp_ratio[i]<-false_neg_rate/false_pos_rate
  error_rate[i]<-(contingency_table$n[3]+contingency_table$n[2])/(sum(contingency_table$n))
}


df<-data.frame(p_star_values,fn_to_fp_ratio,error_rate) #putting p_star_values, ratios, and errors together
df<-filter(df,fn_to_fp_ratio<0.5) %>% arrange(error_rate) #taking out only p_star values where false negative rate is less than half the false positive rate
p_star_optimal<-df$p_star_values[1] #taking first p_star_value (now that dataframe is sorted from lowest error to highest)
p_star_optimal

# You would want to use a p* of around 0.339 because that is the value p* that generates the overall lowest error
# while guarenteeing that the false negative rate is less than half the false postive rate

