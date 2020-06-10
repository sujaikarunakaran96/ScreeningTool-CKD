---
title: "Prostrate cancer"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r}

library(readxl)
main_data = read.csv("/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/training_data copy.csv")


sub_data = read.csv("/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/sub_data.csv")

```


```{r}

str(main_data)


main_data[,!colnames(main_data) %in% c('id','t_score','n_score','m_score','stage','height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')]=lapply(main_data[,!colnames(main_data) %in% c('id','t_score','n_score','m_score','stage','height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')],as.factor)

levels(main_data$age) = list("Below 78" = c(32:78), "Above 78" = c(78:104))


```

```{r}

ggplot(main_data, aes(side,fill=survival_7_years)) + geom_bar()+ facet_grid(main_data$survival_7_years)

ggplot(sub_data, aes(race,fill=survival_7_years)) + geom_bar()+ facet_grid(sub_data$survival_7_years)

ggplot(main_data, aes(tumor_diagnosis,fill=survival_7_years)) + geom_bar()+ facet_grid(main_data$survival_7_years)

ggplot(main_data, aes(psa_diagnosis,fill=survival_7_years)) + geom_bar()+ facet_grid(main_data$survival_7_years)

ggplot(main_data, aes(gleason_score,fill=survival_7_years)) + geom_bar()+ facet_grid(main_data$survival_7_years)

table(sub_data$age, sub_data$survival_7_years)
chisq.test(table(sub_data$age, sub_data$survival_7_years))

```



```{r}
library(ggplot2)

table_tscore = table(sub_data$t_score, sub_data$survival_7_years)
chisq.test(table_tscore)

table_nscore = table(sub_data$n_score, sub_data$survival_7_years)
chisq.test(table_nscore)

table_mscore = table(sub_data$m_score, sub_data$survival_7_years)
chisq.test(table_mscore)

table_race = table(sub_data$race, sub_data$survival_7_years)
chisq.test(table_race)

table_previous = table(sub_data$previous_cancer, sub_data$survival_7_years)
chisq.test(table_previous)

#Smoker is not very significant
table_smoker = table(main_data$smoker, main_data$survival_7_years)
chisq.test(table_smoker)

table_rd = table(sub_data$rd_thrpy, sub_data$survival_7_years)
chisq.test(table_rd)

#Not significant
table_h = table(sub_data$h_thrpy, sub_data$survival_7_years)
chisq.test(table_h)

table_chm = table(main_data$chm_thrpy, main_data$survival_7_years)
chisq.test(table_chm)

table_cry = table(sub_data$cry_thrpy, sub_data$survival_7_years)
chisq.test(table_cry)

table_brch = table(sub_data$brch_thrpy, sub_data$survival_7_years)
chisq.test(table_brch)

#Not significant
table_rad = table(sub_data$rad_rem, sub_data$survival_7_years)
chisq.test(table_rad)

table_multi = table(main_data$multi_thrpy, main_data$survival_7_years)
chisq.test(table_multi)

table_1ys = table(main_data$survival_1_year, main_data$survival_7_years)
chisq.test(table_1ys)
#Side is not significant
table_side = table(main_data$side, main_data$survival_7_years)
chisq.test(table_side)

table_stage = table(main_data$stage, main_data$survival_7_years)
chisq.test(table_stage)


chisq.test(table(sub_data$U03, sub_data$survival_7_years))
chisq.test(table(sub_data$U01, sub_data$survival_7_years))
chisq.test(table(sub_data$U02, sub_data$survival_7_years))
#Sig
chisq.test(table(sub_data$U05, sub_data$survival_7_years))
chisq.test(table(sub_data$U06, sub_data$survival_7_years))
#sig
chisq.test(table(sub_data$P01, sub_data$survival_7_years))
#Sig
chisq.test(table(sub_data$P02, sub_data$survival_7_years))
#Sig
chisq.test(table(sub_data$P03, sub_data$survival_7_years))

chisq.test(table(sub_data$multi_thrpy, sub_data$survival_7_years))

#S04 - 0.0459, S07 - 2.578, S10 - 26.06, O1 - 57.63, O8 - 49.5, O9 - 36.07, O10 - 18.75, O11 - 0

correlation_data = subset(main_data, select = c(gleason_score, age, height, weight, family_history, first_degree_history, tumor_diagnosis, tumor_6_months, tumor_1_year, psa_diagnosis, psa_6_months, psa_1_year, tea))

cor(na.omit(correlation_data[,-14]))

write.csv(correlation_data,"/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/correlation_data.csv", row.names = FALSE)

glm_glea = glm(main_data$survival_7_years~main_data$gleason_score, data = main_data, family = "binomial")
summary(glm_glea)

glm_age = glm(main_data$survival_7_years~main_data$age, data = main_data, family = "binomial")
summary(glm_age)

glm_height = glm(main_data$survival_7_years~main_data$weight, data = main_data, family = "binomial")
summary(glm_height)

t.test(main_data$gleason_score~main_data$survival_7_years)
t.test(main_data$age~main_data$survival_7_years)
t.test(main_data$height~main_data$survival_7_years)
#t test fail
t.test(main_data$weight~main_data$survival_7_years)
#t test fail
t.test(main_data$family_history~main_data$survival_7_years)
t.test(sub_data$first_degree_history~sub_data$survival_7_years)
t.test(sub_data$tumor_diagnosis~sub_data$survival_7_years)
t.test(sub_data$tumor_6_months~sub_data$survival_7_years)
t.test(sub_data$tumor_1_year~sub_data$survival_7_years)
t.test(sub_data$psa_diagnosis~sub_data$survival_7_years)
t.test(main_data$psa_6_months~main_data$survival_7_years)
t.test(sub_data$psa_1_year~sub_data$survival_7_years)
#t test fail
t.test(main_data$tea~main_data$survival_7_years)

t.test(sub_data$gleason_score~sub_data$survival_7_years)


```


```{r}
sum(is.na(main_data))

sub_data = subset(main_data, select = c(gleason_score, t_score, n_score, m_score, stage, age, race, first_degree_history, previous_cancer, tumor_diagnosis, tumor_1_year, psa_diagnosis, psa_1_year, U05, O01, O08, O09, O10, P01, P02, P03, S07, S10, rd_thrpy, chm_thrpy, cry_thrpy, brch_thrpy, rad_rem, multi_thrpy, survival_7_years))

sum(is.na(sub_data))

sum(is.na(sub_data$tumor_6_months))
sum(is.na(sub_data$psa_6_months))

sub_data$race = factor(sub_data$race, level = c("4", "3", "2", "1"))

sapply(sub_data, function(x) sum(is.na(x)))

sub_data = na.omit(sub_data)

table(sub_data$survival_7_years)

summary(sub_data)

str(sub_data)

write.csv(sub_data,"/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/sub_data.csv", row.names = FALSE)

sub_data = sub_data[,-15]

set.seed(2)
indx = sample(2, nrow(sub_data), replace = T, prob = c(0.8, 0.2))

train = sub_data[indx == 1,]
test = sub_data[indx == 2,]

str(sub_data)

table(train$survival_7_years)
table(test$survival_7_years)

summary(sub_data$age)

table(sub_data$survival_7_years, sub_data$survival_1_year)

model_fit = glm(train$survival_7_years~., data = train, family = "binomial")

summary(model_fit)

probabilities_val <- predict.glm(model_fit, newdata = test, type = "response")

predicted_class_val = ifelse(probabilities_val > 0.5, 1, 0)

probabilities_val_train <- predict.glm(model_fit, newdata = train, type = "response")

predicted_class_val_train = ifelse(probabilities_val_train > 0.5, 1, 0)

library(caret)

confusionMatrix(factor(predicted_class_val), test$survival_7_years, positive = "1")

confusionMatrix(factor(predicted_class_val_train), train$survival_7_years, positive = "1")
library(dplyr)

mydata <- subset(sub_data, select = c(gleason_score, first_degree_history, tumor_diagnosis, tumor_1_year, psa_diagnosis, psa_1_year))

mydata

predictors <- colnames(mydata)

library(tidyverse)
library(broom)

mydata <- mydata %>% mutate(logit = log(probabilities/(1-probabilities))) %>% gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

```


```{r}

library(caret)

k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(train)),breaks=k,labels=FALSE) 
models.err <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rf")))


for(i in 1:k)
{ 
  ind <- sample(2, nrow(train), replace = T, prob = c(0.7, 0.3))
  Train <- train[ind == 1, ]
  Validation <- train[ind == 2, ]
  
  pr.err <- c()
  for(mt in seq(1,ncol(Train))){
    library(randomForest)
    rf <- randomForest(survival_7_years~., data = Train, ntree = 100, mtry = ifelse(mt == ncol(Train), mt - 1,mt))
    predicted <- predict(rf, newdata = Validation, type = "class")
    pr.err <- c(pr.err,mean(Validation$survival_7_years != predicted)) 
  }
  
  bestmtry <- which.min(pr.err) 
  bestmtry
 ## library(randomForest)
  ##rf <- randomForest(survival_7_years~., data = train, ntree = 100, mtry = bestmtry)
  ##rf.pred <- predict(rf, newdata = test, type = "class")
  ##models.err[i] <- mean(test$survival_7_years != rf.pred)
}


bestmtry

varImp(rf_final)

rf_final = randomForest(survival_7_years~., data = train, ntree = 100, mtry = 3)
rf_pred = predict(rf_final, newdata = test, type = "class")

rf_pred_train = predict(rf_final, newdata = train, type = "class")

confusionMatrix(factor(rf_pred), test$survival_7_years, positive = "1")

confusionMatrix(factor(rf_pred_train), train$survival_7_years, positive = "1")

```


```{r}

main_test = read_csv("/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/(name)_score copy.csv")

main_test[,!colnames(main_test) %in% c('id','height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')]=lapply(main_test[,!colnames(main_test) %in% c('id', 'height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')],as.factor)

levels(main_test$age) = list("Below 78" = c(32:78), "Above 78" = c(78:104))

str(main_test)

data_test = subset(main_test, select = c(gleason_score, t_score, n_score, m_score, stage, age, race, first_degree_history, previous_cancer, tumor_diagnosis, tumor_1_year, psa_diagnosis, psa_1_year, U05, O01, O08, O09, O10, P01, P02, P03, S07, S10, rd_thrpy, chm_thrpy, cry_thrpy, brch_thrpy, rad_rem, multi_thrpy))

str(data_test)

sapply(data_test, function(x) sum(is.na(x)))

data_test = na.omit(data_test)


rf_pred_test = predict(rf_final, newdata = data_test, type = "class")

table(rf_pred_test)


```


```{r}
install.packages("survival")
library("survival")


main_data = read.csv("/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/training_data_vaish.csv")
str(main_data)

surv_object <- Surv(time = main_data$surv1_flag, event = main_data$survival_1_year)
surv_object

fit2.cox <- coxph(surv_object ~ gleason_score + t_score +n_score+ stage+ age+ race+ first_degree_history+ previous_cancer, data = main_data)

library("survminer")
fit=survfit(fit2.cox)
ggsurvplot(survfit(fit2.cox), color = "#2E9FDF",ggtheme = theme_minimal())
plot(fit)

summary(fit2.cox)

fit2$std.err

fit2
ggsurvplot(fit2, data = main_data, pval = TRUE)
```


```{r}

p_data = read.csv("/Users/sujaikarunakaran/Desktop/Assignments/Spring 2020/Ranga/Prostrate Cancer/participant_files/participant_files/training_data_sujai.csv")

str(p_data)
str(pr_data)
pr_data = p_data
pr_data = pr_data[,-13]
pr_data = pr_data[,-29]

str(pr_data)

pr_data[,!colnames(pr_data) %in% c('No..of.days', 'id','height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')]=lapply(pr_data[,!colnames(pr_data) %in% c('No..of.days','id', 'height','weight', 'family_history', 'first_degree_history', 'tumor_diagnosis', 'tumor_6_months', 'tumor_1_year', 'psa_diagnosis', 'psa_6_months', 'psa_1_year', 'symptoms', 'tea', 'gleason_score')],as.factor)

pr_data$age = as.factor(pr_data$age)
levels(pr_data$age) = list("Below 78" = c(32:78), "Above 78" = c(78:104))

summary(pr_data$tumor_diagnosis)


pr_data$tumor_diagnosis = as.factor(pr_data$tumor_diagnosis)
levels(pr_data$tumor_diagnosis) = list("7 to 38" = c(7:38), "39-49" = c(39:49), "50 - 58" = c(50:58), "58 - 195" = c(58:195))

str(pr_data)
summary(pr_data$tea)

pr_data$tea = as.factor(pr_data$tea)
levels(pr_data$tea) = list("0 to 1" = c(0:1), "2 to 5" = c(2:5), "5 & above" = c(5:12))

pr_data$survival_7_years = ifelse(pr_data$survival_7_years == 1, 0, 1)

library(survival)
library(survminer)
library(dplyr)

pr_data <- pr_data %>% mutate(age = ifelse(age >78, "above 78", "below 78"))

surv_object <- Surv(time = pr_data$No..of.days, event = pr_data$Event)

surv_object

fit1 <- survfit(surv_object~stage, data = pr_data)
summary(fit1)
ggsurvplot(fit1, data = pr_data)

fit2 <- survfit(surv_object~stage+rd_thrpy, data = pr_data)
summary(fit2)

fit3 <- survfit(surv_object~stage+h_thrpy, data = pr_data)
summary(fit3)

fit4 <- survfit(surv_object~stage+h_thrpy+rd_thrpy+chm_thrpy+cry_thrpy+brch_thrpy+rad_rem+tumor_diagnosis, data = pr_data)
ggsurvplot(fit4, data = pr_data)
summary(fit4)

fit5 <- survfit(surv_object~stage+tumor_diagnosis, data = pr_data)
summary(fit5)

fit6 <- survfit(surv_object~stage+h_thrpy+tumor_diagnosis, data = pr_data)
summary(fit6)

fit7 <- survfit(surv_object~stage+rad_rem, data = pr_data)
summary(fit7)

fit8 <- survfit(surv_object~age+race, data = pr_data)
summary(fit8)
ggsurvplot(fit8, data = pr_data)

fit9 <- survfit(surv_object~U05+O01+O08+O09+O10+P01+P02+P03+S07+S10, data = pr_data)
summary(fit9)
ggsurvplot(fit9, data = pr_data)

fit.coxph <- coxph(surv_object~stage+h_thrpy+rd_thrpy+chm_thrpy+cry_thrpy+brch_thrpy+rad_rem+tumor_diagnosis+age+race+U05+O01+O08+O09+O10+P01+P02+P03+S07+S10, data = pr_data)
ggforest(fit.coxph, data = pr_data)


```




























