######################################
# title: "analysis_table3_sensitivity"
# author: "Hitomi Kariya"
# date: "5/16/2020"
# output: html_document
######################################

##### Prepare for the analysis #####
setwd("~/Downloads/Thesis")
library(readr)
library(tidyverse)
thesis_table3_sensitivity <- read_csv("Thesis/thesis_table3_sensitivity.csv")
df_sens <- thesis_table3_sensitivity
#data for sensitivity analysis - flu, depression

##### Check & Clean the dataset #####
str(df_sens)
head(df_sens)
colnames(df_sens)
#recode subgroups (nothing = 0, pap_only = 1, completed =2)
df_sens$uptake <- ifelse(df_sens$uptake == "nothing", 0, ifelse(df_sens$uptake == "pap_only", 1, 2))
#recode enroll_cat (12+ = 0, <12m =1)
df_sens$enroll_cat <- ifelse(df_sens$enroll_cat == "12+", 0, 1)
#recode status (not_uptake = 0, uptake =1)
df_sens$status <- ifelse(df_sens$status == "not_uptake", 0, 1)
str(df_sens) #check the class of the variables
df_sens[,2:4] <- lapply(df_sens[,2:4], as.factor) #factorized the categorical variables
str(df_sens) #re-check the class of the variables
#expand the dataset
exp_df_sens <- data.frame(service = rep(df_sens$service, df_sens$frequency),
                          subgroup = rep(df_sens$uptake, df_sens$frequency),
                          enroll = rep(df_sens$enroll_cat, df_sens$frequency),
                          status = rep(df_sens$status, df_sens$frequency))
str(exp_df_sens)
#check the exp_df_diabetes
exp_df_sens %>% filter (service == "flu" & subgroup == 0 & enroll == 0 & status == 1) %>% nrow()
# subset the dataset to flu
exp_df_sens_flu <- exp_df_sens %>% filter (service == "flu")
# subset the dataset to depression
exp_df_sens_dep <- exp_df_sens %>% filter (service == "depression")

##### Fit a model to Flu #####
## Unadjusted OR for Flu, sensitivity
# fit a GLM model
model <- glm(status ~ subgroup, family = binomial, data = exp_df_sens_flu)
coef(summary(model))
#OR estiamte comparing intervention to control
exp(coef(model))
#95% CI comparing intervention to control
exp(confint.default(model))

##### Fit a model to Depression #####
## Unadjusted OR for Depression, sensitivity
# fit a GLM model
model <- glm(status ~ subgroup, family = binomial, data = exp_df_sens_dep)
coef(summary(model))
#OR estiamte comparing intervention to control
exp(coef(model))
#95% CI comparing intervention to control
exp(confint.default(model))


