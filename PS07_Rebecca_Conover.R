library(tidyverse)
library(broom)
library(okcupiddata)
data("profiles")
library(ROCR)  

#Prepare data------

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


#Random Model------

random_predict<-profiles 
random_predict["p_hat"]=runif(nrow(profiles))
random_predict<-random_predict %>% 
  select(y,p_hat)

# This bit of code computes the ROC curve
pred_random <- prediction(predictions = random_predict$p_hat, labels = random_predict$y)
perf_random <- performance(pred_random, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_random <- as.numeric(performance(pred_random,"auc")@y.values)
auc_random

plot(perf_random, main=paste("Area Under the Curve =", round(auc_random, 3)))
abline(c(0, 1), lty=2)

#Perfect Model-----


perfect_predict<-profiles 
perfect_predict["p_hat"]=profiles["y"]
perfect_predict<-perfect_predict %>% 
  select(y,p_hat)

pred_perfect <- prediction(predictions = perfect_predict$p_hat, labels = perfect_predict$y)
perf_perfect <- performance(pred_perfect, "tpr","fpr")

# This bit of code computes the Area Under the Curve
auc_perfect <- as.numeric(performance(pred_perfect,"auc")@y.values)
auc_perfect

plot(perf_perfect, main=paste("Area Under the Curve =", round(auc_perfect, 3)))
abline(c(0, 1), lty=2)


#Choosing p*-------

#Marketing feels that it is twice as costly to incorrectly predict a user to be
#male when they are really female than to incorrectly predict a user to be
#female when they are really male. Roughly what threshold p∗ should you use in
#the decision rule: “Predict a user to be female if pˆ>p∗”?

#if female==1 , we want false negative rate to be half as large the false positive rate

model_formula <- as.formula(y~height+age)
model_logistic <- glm(model_formula, data=profiles, family="binomial")

profiles_log <- profiles %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)

pred_log <- prediction(predictions = profiles_log$p_hat, labels = profiles_log$y)
perf_log <- performance(pred_log, "tpr","fpr")


auc_log <- as.numeric(performance(pred_log,"auc")@y.values)
auc_log


plot(perf_log, main=paste("Area Under the Curve =", round(auc_log, 3)))
abline(c(0, 1), lty=2)

false_n_over_false_p<-data.frame("fn_fp"=numeric(),"p_star"=numeric()) 

for(i in 1:400) {
  p_star<-i/1000+0.3
  prediction<-profiles_log %>% 
    mutate(p=ifelse(p_hat>p_star, 1,0))
    
  cont_table<-prediction %>% 
    group_by(y, p) %>% 
    summarize(count=n()) %>% 
    group_by(y) %>% 
    mutate(prop=count/sum(count)) %>% 
    filter(y!=p) %>% 
    ungroup() %>% 
    select(prop)
  
  fn_fp<-as.numeric(cont_table[2,1])/as.numeric(cont_table[1,1])
  
  fn_fp<-as.data.frame(fn_fp) %>% 
     mutate(p_star=p_star)
  
  false_n_over_false_p<-false_n_over_false_p %>% 
    rbind(fn_fp)
 
}



good_ps<-false_n_over_false_p %>% 
  filter(fn_fp > 0.48 & fn_fp < 0.52)
  



#these p_stars (around 0.34) coorespond to when the false negative rate is approximately half
#of the false negative rate: i.e. when the model predicts a female is a male
#half as often as it predects a male to be a female






