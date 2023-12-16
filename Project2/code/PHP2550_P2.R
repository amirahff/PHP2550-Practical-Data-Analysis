###############
### LIBRARY ###
###############

library(tidyverse)
library(ggplot2)
library(naniar)
library(gt)
library(gtsummary)
library(kableExtra)
library(GGally)
library(corrplot)
library(patchwork)
library(splitstackshape)
library(mice)
library(glmnet)
library(pROC)
library(lme4)
library(caret)
library(tableone)

#################
### FUNCTIONS ###
#################

logit2prob <- function(logit){
  #' Get Probability from logit value
  #' @param logit, Logit value
  #' @return prob, probability from the logit value
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

lasso_ridge <- function(df) { 
  #' Runs 10-fold CV for lasso & ridge and returns corresponding coefficients. Also include 
  #'    coefficient from logistic regression model with no regularization.
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error for lasso and ridge
  
  # Matrix form for ordered variables 
  x.ord <- model.matrix(trach~., data = df)[,-1]
  y.ord <- df$trach
  
  # Generate folds
  k <- 10 
  set.seed(1) # consistent seeds between imputed data sets
  folds <- sample(1:k, nrow(df), replace=TRUE)
  
  # Lasso model
  # Use Cross validation to search for optimal lambda
  lasso_mod_cv <- cv.glmnet(x.ord, y.ord, nfolds = 10, foldid = folds, 
                            alpha = 1, family = "binomial") 
  # Build Lasso model using optimal lambda
  lasso_mod <- glmnet(x.ord, y.ord, nfolds = 10, alpha = 1
                      ,family = 'binomial'
                      ,lambda = lasso_mod_cv$lambda.min)
  
  # Ridge model
  # Use Cross validation to search for optimal lambda
  ridge_mod_cv <- cv.glmnet(x.ord, y.ord, nfolds = 10, foldid = folds, 
                            alpha = 0, family = "binomial") 
  # Build Ridge model using optimal lambda
  ridge_mod <- glmnet(x.ord, y.ord, nfolds = 10, alpha = 0
                      ,family = 'binomial'
                      ,lambda = ridge_mod_cv$lambda.min)
  
  # Get coefficients from Lasso and Ridge
  coef = list()
  coef_lasso <- coef(lasso_mod, lambda = lasso_mod$lambda.min) 
  coef_ridge <- coef(ridge_mod, lambda = ridge_mod$lambda.min) 
  
  coef = append(coef, coef_lasso)
  coef = append(coef, coef_ridge)
  
  return(coef) 
} 

get_avg_coefs <- function(df) {
  #' Get the average coefficients for lasso and ridge from the multiple imputation datasets
  #' @param df, multiple imputation datasets
  #' @return, the average coefficients for lasso and ridge 
  
  # Initiation 
  coefs = vector('list',5)
  
  # For each imputation data set, estimate the coefficients
  for(i in 1:5) {
    coef = lasso_ridge(df %>% 
                         filter(.imp == i) %>% 
                         dplyr::select(-c(.imp,center)))
    coefs[[i]] = coef
  }
  
  # Find average lasso and ridge coefficients over imputed datasets
  lasso_coef1 <- coefs[[1]][[1]]
  lasso_coef2 <- coefs[[2]][[1]] 
  lasso_coef3 <- coefs[[3]][[1]]
  lasso_coef4 <- coefs[[4]][[1]]
  lasso_coef5 <- coefs[[5]][[1]] 
  lasso_coef <- cbind(lasso_coef1, lasso_coef2, lasso_coef3, 
                      lasso_coef4, lasso_coef5) 
  avg_coefs_lasso <- apply(lasso_coef, 1, mean) 
  
  ridge_coef1 <- coefs[[1]][[2]] 
  ridge_coef2 <- coefs[[2]][[2]] 
  ridge_coef3 <- coefs[[3]][[2]] 
  ridge_coef4 <- coefs[[4]][[2]] 
  ridge_coef5 <- coefs[[5]][[2]] 
  ridge_coef <- cbind(ridge_coef1, ridge_coef2, ridge_coef3, 
                      ridge_coef4, ridge_coef5) 
  avg_coefs_ridge <- apply(ridge_coef, 1, mean) 
  
  
  # Store coefficients in the result list
  result = vector('list',2)
  result[[1]] = avg_coefs_lasso
  result[[2]] = avg_coefs_ridge
  
  return(result)
}

pred_eval <- function(valid_df, coefs) {
  #' Predict new data using defined coefficients and return several metrics
  #' @param valid_df, data to be predicted (multiple imputation)
  #' @param coefs, set of coefficients from lasso/ridge
  #' @return auc,f1,sensitivity,specificity,precision aggregated from
  #'  all the imputated dataset
  
  all_auc = c()
  all_f1 = c()
  all_sensitivity = c()
  all_specificity = c()
  all_precision = c()
  for(i in 1:5){
    x <- model.matrix(trach~., data = valid_df %>% 
                        filter(.imp == i) %>% 
                        dplyr::select(-c(.imp,center)))
    y <- valid_df[valid_df$.imp == i,'trach']
    
    y_pred = logit2prob(x %*% as.matrix(coefs))
    
    all_auc = append(all_auc, round(auc(y, y_pred),4))
    cm = confusionMatrix(factor(round(y_pred)), reference = factor(y), positive = '1')
    f1 = ifelse(is.na(cm$byClass[7]),0,cm$byClass[7])
    all_f1 = append(all_f1, round(f1 ,4))
    sensitivity = ifelse(is.na(cm$byClass[1]),0,cm$byClass[1])
    all_sensitivity = append(all_sensitivity, round(sensitivity ,4))
    specificity = ifelse(is.na(cm$byClass[2]),0,cm$byClass[2])
    all_specificity = append(all_specificity, round(specificity ,4))
    precision = ifelse(is.na(cm$byClass[5]),0,cm$byClass[5])
    all_precision = append(all_precision, round(precision ,4))
  }
  
  result = vector('list',2)
  result[[1]] = mean(all_auc)
  result[[2]] = mean(all_f1)
  result[[3]] = mean(all_sensitivity)
  result[[4]] = mean(all_specificity)
  result[[5]] = mean(all_precision)
  return(result)
}

pred_eval_ml <- function(valid_df, model) {
  #' Predict new data using defined multilevel model
  #' @param valid_df, data to be predicted (multiple imputation)
  #' @param model, set of coefficients from lasso/ridge
  #' @return auc,f1,sensitivity,specificity,precision aggregated from
  #'  all the imputated dataset
  
  all_auc = c()
  all_f1 = c()
  all_sensitivity = c()
  all_specificity = c()
  all_precision = c()
  
  yProb = predict(m1_trach
                  , newdata = trach_test %>% 
                    filter(imp == i) %>% 
                    dplyr::select(-c(imp))
                  , type="response")
  y = trach_test[trach_test$imp == i,'trach']
  
  for(i in 1:5){
    x <- valid_df %>% 
      filter(imp == i) %>% 
      dplyr::select(-c(imp))
    y <- valid_df[valid_df$imp == i,'trach']
    
    y_pred = predict(model
                     , newdata = x
                     , type="response")
    
    all_auc = append(all_auc, round(auc(y, y_pred),4))
    cm = confusionMatrix(factor(round(y_pred)), reference = factor(y), positive = '1')
    f1 = ifelse(is.na(cm$byClass[7]),0,cm$byClass[7])
    all_f1 = append(all_f1, round(f1 ,4))
    sensitivity = ifelse(is.na(cm$byClass[1]),0,cm$byClass[1])
    all_sensitivity = append(all_sensitivity, round(sensitivity ,4))
    specificity = ifelse(is.na(cm$byClass[2]),0,cm$byClass[2])
    all_specificity = append(all_specificity, round(specificity ,4))
    precision = ifelse(is.na(cm$byClass[5]),0,cm$byClass[5])
    all_precision = append(all_precision, round(precision ,4))
    
    result = vector('list',2)
    result[[1]] = mean(all_auc)
    result[[2]] = mean(all_f1)
    result[[3]] = mean(all_sensitivity)
    result[[4]] = mean(all_specificity)
    result[[5]] = mean(all_precision)
    return(result)
  }
}

###################################
### READ DATA AND PREPROCESSING ###
###################################

# Set working directory and read data
setwd("/Users/amirahff/Documents/Brown Biostatistics/PHP 2550/project2")
raw_df <- read.csv("project2.csv")

#####################
### PREPROCESSING ###
#####################

# Check data type
str(raw_df)

# Is there any duplicate data
# Yes, record_id = 2000824
raw_df %>%
  group_by(record_id) %>%
  count() %>%
  filter(n > 1)

# Remove duplicate data
df = raw_df %>%
  group_by(record_id) %>%
  distinct() %>%
  ungroup()

# Rename some columns and change data type
df = df %>%
  rename('peep_cm_h2o.36' = 'peep_cm_h2o_modified.36'
         ,'peep_cm_h2o.44' = 'peep_cm_h2o_modified.44'
         ,'ventilation_support_level.44' = 'ventilation_support_level_modified.44'
         ,'trach' = 'Trach'
         ,'death' = 'Death') %>%
  mutate(prenat_ster = case_when(prenat_ster=='Yes'~1,prenat_ster=='No'~0)
         ,com_prenat_ster = case_when(com_prenat_ster=='Yes'~1,com_prenat_ster=='No'~0)
         ,mat_chorio = case_when(mat_chorio=='Yes'~1,mat_chorio=='No'~0)
         ,gender = case_when(gender=='Male'~1,gender=='Female'~0)
         ,sga = case_when(sga=='SGA'~1,sga=='Not SGA'~0)
         ,any_surf = case_when(any_surf=='Yes'~1,any_surf=='No'~0)
         ,death = case_when(death=='Yes'~1,death=='No'~0)) %>%
  mutate(record_id = as.factor(record_id)
         , center = as.factor(center)
         , mat_race = as.factor(mat_race)
         , mat_ethn = as.factor(mat_ethn)
         , del_method = as.factor(del_method)
         , prenat_ster = as.factor(prenat_ster)
         , com_prenat_ster = as.factor(com_prenat_ster)
         , mat_chorio = as.factor(mat_chorio)
         , gender = as.factor(gender)
         , sga = as.factor(sga)
         , ventilation_support_level.36 = as.factor(ventilation_support_level.36)
         , med_ph.36 = as.factor(med_ph.36)
         , ventilation_support_level.44 = as.factor(ventilation_support_level.44)
         , med_ph.44 = as.factor(med_ph.44)
         , any_surf = as.factor(any_surf)
         , trach = as.factor(trach)
         , death = as.factor(death)) %>%
  mutate(across(where(is.numeric), round, 4))

###########################
### UNIVARIATE ANALYSIS ###
###########################

# Univariate EDA for categorical and continuous columns
categorical_columns = colnames(df[sapply(df, class) %in% c('character','factor')])[-1]
continuous_columns = colnames(df[sapply(df, class) %in% c('numeric','integer')])

# Tableone
df_tableone = CreateTableOne(data = df %>% dplyr::select(-c(record_id)), factorVars = categorical_columns)
df_tableone

# Summary for Continuous
summary(df[,continuous_columns])

####################
### MISSING DATA ###
####################

# How many?
n_miss(df)
prop_miss(df)

# Getting the frequency table of how many missing data each family has
missingVar = table(rowSums(sapply(df, is.na))) 
as.data.frame(t(as.matrix(missingVar))) %>%
  mutate(' ' = c('# Baby'), .before = 0) %>%
  kableExtra::kbl(caption = 'Frequencies of Total Variables Missing for 
                  Each Baby'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position') %>%
  add_header_above(c(' ' = 1, "# Variables Missing for Each Baby" = 14, ' ' = 1)
                   , escape = T)

# Getting the frequency table of how many missing data each variable has
missingObs = table(colSums(sapply(df, is.na))) 
as.data.frame(t(as.matrix(missingObs))) %>%
  mutate(' ' = c('# Variables'), .before = 1) %>%
  kableExtra::kbl(caption = 'Frequencies of Total Observations Missing for 
                  Each Variable'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position') %>%
  add_header_above(c(' '=1, '# Observations Missing for Each Variable'=21, ' '=1)
                   , escape = T)

# Variables' missing data proportion 
varMissingProp = miss_var_summary(df) 
varMissingProp %>%
  filter(n_miss > 0) %>%
  kableExtra::kbl(caption = 'Missing Data Proportion for Each Variable'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Variable','Observation Missing','Proportion 
                                  Missing')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

# Missing Pattern table 
missingPatternTable = miss_case_table(df)
missingPatternTable %>%
  kableExtra::kbl(caption = 'Missing Data Pattern Summary '
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Number Variable Missing','Total Case','Case 
                                  Proportion')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

# Missing Pattern Plot
gg_miss_upset(df, nsets = 7) 

# Is the missing data related to the other variables?
# Discharge_before_44, center, and death in particular
df = df %>%
  mutate(miss36 = rowSums(sapply(df[,c('weight_today.36'
                                       ,'ventilation_support_level.36'
                                       ,'inspired_oxygen.36'
                                       ,'p_delta.36'
                                       ,'peep_cm_h2o.36'
                                       ,'med_ph.36')], is.na))
         ,miss44 = rowSums(sapply(df[,c('weight_today.44'
                                        ,'ventilation_support_level.44'
                                        ,'inspired_oxygen.44'
                                        ,'p_delta.44'
                                        ,'peep_cm_h2o.44'
                                        ,'med_ph.44')], is.na))
  ) %>%
  mutate(complete36 = round(1 - (miss36/6),2)
         ,complete44 = round(1 - (miss44/6),2)
         ,discharge_before_44 = as.factor(ifelse(hosp_dc_ga <= 44, 1, 0))
  ) %>%
  dplyr::select(-c(miss36,miss44))

# Center  
df %>%
  group_by(center) %>%
  summarize(avg_complete36 = mean(complete36)
            ,avg_complete44 = mean(complete44)
            ,count = n())

# Discharge before 44 weeks 
df %>%
  group_by(discharge_before_44) %>%
  summarize(avg_complete36 = mean(complete36)
            ,avg_complete44 = mean(complete44)
            ,count = n())

df %>%
  group_by(discharge_before_44) %>%
  summarize(total_complete36 = sum(ifelse(complete36 > 0,1,0))
            ,total_complete44 = sum(ifelse(complete44 > 0,1,0))
            ,count = n())

# Summary table for missingness in week 44
df %>%
  mutate(missing44 = ifelse(complete44 > 0,1,0)) %>%
  dplyr::select(-c(record_id,complete36,complete44)) %>%
  tbl_summary(by = missing44
              , statistic = list(
                all_continuous() ~ "{mean} ({sd})"
                ,all_categorical() ~ "{n} / {N} ({p}%)"
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Missing44**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

##########################
### BIVARIATE ANALYSIS ###
##########################

# Bivariate EDA For Trachoestomy
df %>%
  dplyr::select(-c(record_id)) %>%
  tbl_summary(by = trach
              , statistic = list(
                all_continuous() ~ "{mean} ({sd})"
                ,all_categorical() ~ "{n} / {N} ({p}%)"
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tracheostomy**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , font_size = 10
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

# Center Summary
df %>%
  dplyr::select(-c(record_id,complete36,complete44)) %>%
  tbl_summary(by = center
              # , type = list(c(categorical_columns) ~ "categorical")
              , statistic = list(
                all_continuous() ~ "{mean} ({sd})"
                ,all_categorical() ~ "{n} / {N} ({p}%)"
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Center**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , font_size = 7
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

###############################################
### CORRELATION BETWEEN CONTINUOUS VARIABLE ###
###############################################

cor_mat = cor(df[,continuous_columns]
              , use="pairwise.complete.obs")

# Remove diagonal and redundant values
cor_mat[!lower.tri(cor_mat)] = NA 
cor_df = data.frame(cor_mat) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.35)
# Only take continuous pair with abs(correlation) > 0.35
cor_df %>%
  rename('variable1' = 'rowname', 'variable2' = 'variable') %>%
  filter(!(str_detect(variable1, "_diff") | str_detect(variable2, "_diff"))) %>%
  arrange(desc(correlation)) %>%
  kableExtra::kbl(caption = 'Correlation Between Continuous Variables'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , font_size = 10
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

###########################
### FINAL PREPROCESSING ###
###########################

# Delete unimportant/unusable/redundant variables
# Modify weight to kgs
df = df %>%
  mutate(any_44 = ifelse(complete44 > 0,1,0)
         ,any_36 = ifelse(complete36 > 0,1,0)) %>%
  mutate(bw = round(bw/1000,2)
         ,weight_today.44 = round(weight_today.44/1000,2)
         ,weight_today.36 = round(weight_today.36/1000,2)) %>%
  dplyr::select(-c(mat_ethn,ga,blength,birth_hc,com_prenat_ster,mat_chorio
                   ,gender,any_surf,discharge_before_44,death,bw))

########################
### TRAIN TEST SPLIT ###
########################

# Stratify the train and test data by the trach, center, and missingness of 44wk data
set.seed(7)
trach_train_id = stratified(df, c('trach','center','any_44'), 0.3)

trach_train_raw = df %>%
  filter(!record_id %in% trach_train_id$record_id)

trach_test_raw = df %>%
  filter(record_id %in% trach_train_id$record_id)

trach_train_o36_raw = trach_train_raw %>% filter(any_36 == 1)
trach_train_w44_raw = trach_train_raw %>% filter(any_44 == 1)
trach_test_o36_raw = trach_test_raw %>% filter(any_36 == 1)
trach_test_w44_raw = trach_test_raw %>% filter(any_44 == 1)

###############################
### MISSING DATA IMPUTATION ###
###############################

# Potential columns to include (consideration after EDA)
# Trach and Death has different subset of data based on the EDA
# Record_id are kept for the future use
trach36col = c( 'record_id','trach','center','mat_race','del_method'
                ,'prenat_ster','sga','weight_today.36','ventilation_support_level.36'
                ,'inspired_oxygen.36','p_delta.36','peep_cm_h2o.36','med_ph.36')
trach44col = c( 'record_id','trach','center','mat_race','del_method'
                ,'prenat_ster','sga','weight_today.36','ventilation_support_level.36'
                ,'inspired_oxygen.36','peep_cm_h2o.36','p_delta.36','med_ph.36'
                ,'weight_today.44','ventilation_support_level.44'
                ,'inspired_oxygen.44','peep_cm_h2o.44','p_delta.44','med_ph.44')

# Prepare to impute 
trach_train_o36_raw = trach_train_o36_raw[,trach36col]
trach_test_o36_raw = trach_test_o36_raw[,trach36col]
trach_train_w44_raw = trach_train_w44_raw[,trach44col]
trach_test_w44_raw = trach_test_w44_raw[,trach44col]

trach_train_o36 = trach_train_o36_raw[,-1]
trach_test_o36 = trach_test_o36_raw[,-1]
trach_train_w44 = trach_train_w44_raw[,-1]
trach_test_w44 = trach_test_w44_raw[,-1]

## With 36 only data (trach)
trach_train_o36_mice <- mice(trach_train_o36, 5, pri=F)
trach_test_o36_mice <- mice.mids(trach_train_o36_mice, newdata = trach_test_o36
                                 , pri = F)

## With 44 weeks data (trach)
trach_train_w44_mice <- mice(trach_train_w44, 5, pri=F)
trach_test_w44_mice <- mice.mids(trach_train_w44_mice, newdata = trach_test_w44
                                 , pri = F)

# Generate fully imputed data
trach_train_o36_long <- mice::complete(trach_train_o36_mice,action="long")[,-2] 
trach_test_o36_long <- mice::complete(trach_test_o36_mice,action="long")[,-2] 
trach_train_w44_long <- mice::complete(trach_train_w44_mice,action="long")[,-2] 
trach_test_w44_long <- mice::complete(trach_test_w44_mice,action="long")[,-2] 

trach_train_w44_long = trach_train_w44_long%>%
  mutate(weight_today.diff = round(weight_today.44 - weight_today.36,3)
         ,ventilation_support_level.diff = (as.numeric(ventilation_support_level.44)-1) 
         -  (as.numeric(ventilation_support_level.36)-1) 
         ,inspired_oxygen.diff = round(inspired_oxygen.44 - inspired_oxygen.36,3)
         ,p_delta.diff = round(p_delta.44 - p_delta.36,3)
         ,peep_cm_h2o.diff = round(peep_cm_h2o.44 - peep_cm_h2o.36,3)
         ,med_ph.diff = (as.numeric(med_ph.44)-1) -(as.numeric(med_ph.36)-1))

trach_test_w44_long = trach_test_w44_long%>%
  mutate(weight_today.diff = round(weight_today.44 - weight_today.36,3)
         ,ventilation_support_level.diff = (as.numeric(ventilation_support_level.44)-1) 
         -  (as.numeric(ventilation_support_level.36)-1) 
         ,inspired_oxygen.diff = round(inspired_oxygen.44 - inspired_oxygen.36,3)
         ,p_delta.diff = round(p_delta.44 - p_delta.36,3)
         ,peep_cm_h2o.diff = round(peep_cm_h2o.44 - peep_cm_h2o.36,3)
         ,med_ph.diff = (as.numeric(med_ph.44)-1) -(as.numeric(med_ph.36)-1))

# save(trach_train_o36_long, file = 'trach_train_o36_long.Rdata')
# save(trach_test_o36_long, file = 'trach_test_o36_long.Rdata')
# save(trach_train_w44_long, file = 'trach_train_w44_long.Rdata')
# save(trach_test_w44_long, file = 'trach_test_w44_long.Rdata')
# load('trach_train_o36_long.Rdata')
# load('trach_test_o36_long.Rdata')
# load('trach_train_w44_long.Rdata')
# load('trach_test_w44_long.Rdata')

# Prepare dataset only has difference, not 44 weeks data
# Backup original data
trach_train_w44_long2 = trach_train_w44_long
trach_test_w44_long2 = trach_test_w44_long

# Delete 44 weeks data
trach_train_w44_long = trach_train_w44_long %>%
  dplyr::select(-c(weight_today.44,ventilation_support_level.44
                   ,inspired_oxygen.44,p_delta.44
                   ,peep_cm_h2o.44,med_ph.44))

trach_test_w44_long = trach_test_w44_long %>%
  dplyr::select(-c(weight_today.44,ventilation_support_level.44
                   ,inspired_oxygen.44,p_delta.44
                   ,peep_cm_h2o.44,med_ph.44))


########################
#### LASSO AND RIDGE ###
########################

# Get the avg coefficients from for each dataset 
trach36 = get_avg_coefs(trach_train_o36_long)
trach44 = get_avg_coefs(trach_train_w44_long)

# save(trach36, file="trach36.Rdata")
# load("trach36.Rdata")
# save(trach44, file="trach44.Rdata")
# load("trach44.Rdata")

# Get evaluation metrics for lasso and ridge
metrics_trach_o36_l = pred_eval(trach_test_o36_long,trach36[[1]])
metrics_trach_w44_l = pred_eval(trach_test_w44_long,trach44[[1]])

metrics_trach_o36_r = pred_eval(trach_test_o36_long,trach36[[2]])
metrics_trach_w44_r = pred_eval(trach_test_w44_long,trach44[[2]])

# Compare the metrics from Lasso and Ridge for all the datasets
test_metrics_df = data.frame(name = 'Trach 36 Only'
                             ,metrics = rep(c('AUC','F1','Sensitivity'
                                              ,'Specificity','Precision')
                                            ,each = 2)
                             ,models = rep(c('Lasso','Ridge'), times = 5)
                             ,scores = c(metrics_trach_o36_l[[1]]
                                         ,metrics_trach_o36_r[[1]]
                                         ,metrics_trach_o36_l[[2]]
                                         ,metrics_trach_o36_r[[2]]
                                         ,metrics_trach_o36_l[[3]]
                                         ,metrics_trach_o36_r[[3]]
                                         ,metrics_trach_o36_l[[4]]
                                         ,metrics_trach_o36_r[[4]]
                                         ,metrics_trach_o36_l[[5]]
                                         ,metrics_trach_o36_r[[5]]))
test_metrics_df = rbind(test_metrics_df
                        , data.frame(name = 'Trach 36 and 44'
                                     ,metrics = rep(c('AUC','F1','Sensitivity'
                                                      ,'Specificity','Precision')
                                                    ,each = 2)
                                     ,models = rep(c('Lasso','Ridge')
                                                   , times = 5)
                                     ,scores = c(metrics_trach_w44_l[[1]]
                                                 ,metrics_trach_w44_r[[1]]
                                                 ,metrics_trach_w44_l[[2]]
                                                 ,metrics_trach_w44_r[[2]]
                                                 ,metrics_trach_w44_l[[3]]
                                                 ,metrics_trach_w44_r[[3]]
                                                 ,metrics_trach_w44_l[[4]]
                                                 ,metrics_trach_w44_r[[4]]
                                                 ,metrics_trach_w44_l[[5]]
                                                 ,metrics_trach_w44_r[[5]])))

test_metrics_df %>%
  kableExtra::kbl(caption = 'AUC comparison: Trach for 36 Weeks Only and 36 & 44 Weeks, Test Data'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Name','Metrics','Models','Scores')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')


############################################
### MULTILEVEL: BUILD MULTILEVEL DATASET ###
############################################

# Multilevel long structure
# Combining the imputed 36-week data and imputed 44-week data
# Trach Train
trach_train = trach_train_o36_long %>% dplyr::select(-c(sga,del_method))
colnames(trach_train) = c("imp","trach","center","mat_race"
                          ,"prenat_ster","weight_today","ventilation_support_level"
                          ,"inspired_oxygen","p_delta","peep_cm_h2o","med_ph")
trach_train$time = 36
trach_train = cbind(trach_train, record_id = as.vector(trach_train_o36_raw[,1]))

trach_train_tmp = trach_train_w44_long2[,c(".imp","trach","center","mat_race"
                                           ,"prenat_ster","weight_today.44"
                                           ,"ventilation_support_level.44"
                                           ,"inspired_oxygen.44","p_delta.44"
                                           ,"peep_cm_h2o.44","med_ph.44")]
colnames(trach_train_tmp) = c("imp","trach","center","mat_race"
                              ,"prenat_ster","weight_today","ventilation_support_level"
                              ,"inspired_oxygen","p_delta","peep_cm_h2o","med_ph")
trach_train_tmp$time = 44
trach_train_tmp = cbind(trach_train_tmp, record_id = as.vector(trach_train_w44_raw[,1]))

trach_train = rbind(trach_train, trach_train_tmp) %>%
  arrange(imp, record_id)

# Trach Test
trach_test = trach_test_o36_long %>% dplyr::select(-c(sga,del_method))
colnames(trach_test) = c("imp","trach","center","mat_race"
                         ,"prenat_ster","weight_today","ventilation_support_level"
                         ,"inspired_oxygen","p_delta","peep_cm_h2o","med_ph")
trach_test$time = 36
trach_test = cbind(trach_test, record_id = as.vector(trach_test_o36_raw[,1]))

trach_test_tmp = trach_test_w44_long2[,c(".imp","trach","center","mat_race"
                                         ,"prenat_ster","weight_today.44"
                                         ,"ventilation_support_level.44"
                                         ,"inspired_oxygen.44","p_delta.44"
                                         ,"peep_cm_h2o.44","med_ph.44")]
colnames(trach_test_tmp) = c("imp","trach","center","mat_race"
                             ,"prenat_ster","weight_today","ventilation_support_level"
                             ,"inspired_oxygen","p_delta","peep_cm_h2o","med_ph")
trach_test_tmp$time = 44
trach_test_tmp = cbind(trach_test_tmp, record_id = as.vector(trach_test_w44_raw[,1]))

trach_test = rbind(trach_test, trach_test_tmp) %>%
  arrange(imp, record_id)

##########################################
### MULTILEVEL: FIT AND EVALUATE MODEL ###
##########################################

# Eval values initialization
all_auc_trach = 0
all_f1_trach = 0
all_sen_trach = 0
all_spec_trach = 0
all_prec_trach = 0

# Iterate through all the imputed values
for (i in 1:5) {
  # Model 1 Trach : All significant variables and random effect from centers
  m1_trach = glmer(trach ~ time + mat_race + prenat_ster + weight_today 
                   + ventilation_support_level + inspired_oxygen + p_delta + peep_cm_h2o 
                   + med_ph + (1 | center) 
                   , data = trach_train %>% filter(imp == i) %>% dplyr::select(-c(imp))
                   , family = binomial)
  
  res = pred_eval_ml(trach_test, m1_trach)
  all_auc_trach = all_auc_trach + res[[1]]
  all_f1_trach = all_f1_trach + res[[2]]
  all_sen_trach = all_sen_trach + res[[3]]
  all_spec_trach = all_spec_trach + res[[4]]
  all_prec_trach = all_prec_trach + res[[5]]
}

# Evaluation metrics from Multilevel Model for all the trach datasets
valid_metrics_ml_df = data.frame(name = 'Trach 36 and 44'
                                 ,metrics = c('AUC','F1','Sensitivity'
                                              ,'Specificity','Precision')
                                 ,models = c('Multilevel')
                                 ,scores = c(all_auc_trach/5
                                             ,all_f1_trach/5
                                             ,all_sen_trach/5
                                             ,all_spec_trach/5
                                             ,all_prec_trach/5)
)

#####################################
### EVALUATION METRICS COMPARISON ###
#####################################

# Make evaluation table, combine with lasso and ridge results
all_test_metrics_df = rbind(test_metrics_df,valid_metrics_ml_df)
all_test_metrics_df %>%
  kableExtra::kbl(caption = 'Metrics comparison: Lasso, Ridge, and Multilevel'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Name','Metrics','Models','Scores')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')


# Show plot for evaluation metrics comparison
trach_label = c(`Trach 36 and 44` = 'Baseline + Week 36 + Week 44'
                ,`Trach 36 Only` = 'Baseline + Week 36')

ggplot(all_test_metrics_df,aes(x=metrics,y=scores,fill=models)) + 
  geom_bar(stat="identity", position=position_dodge(), alpha = 0.8)+
  facet_grid(~name, labeller = as_labeller(trach_label)) +
  labs(title = 'Performance Comparison between the Models for Tracheostomy Prediction'
       ,x = 'Metrics'
       ,y = 'Scores')
theme_bw()

##############################################
### FINAL MODEL (LASSO) USING FULL DATASET ###
##############################################

# Impute full dataset
trach_w44_raw = df %>% filter(any_44 == 1)
trach44col = c( 'record_id','trach','center','mat_race','bw','del_method'
                ,'prenat_ster','sga','weight_today.36','ventilation_support_level.36'
                ,'inspired_oxygen.36','peep_cm_h2o.36','p_delta.36','med_ph.36'
                ,'weight_today.44','ventilation_support_level.44'
                ,'inspired_oxygen.44','peep_cm_h2o.44','p_delta.44','med_ph.44')
trach_w44_raw = trach_train_w44_raw[,trach44col]
trach_w44 = trach_w44_raw[,-1]


## With 44 weeks data (trach)
trach_w44_mice <- mice(trach_w44, 5, pri=F)

# Generate fully imputed data
trach_w44_long <- mice::complete(trach_w44_mice,action="long")[,-2] 

trach_w44_long = trach_w44_long%>%
  mutate(weight_today.diff = round(weight_today.44 - weight_today.36,3)
         ,ventilation_support_level.diff = (as.numeric(ventilation_support_level.44)-1) 
         -  (as.numeric(ventilation_support_level.36)-1) 
         ,inspired_oxygen.diff = round(inspired_oxygen.44 - inspired_oxygen.36,3)
         ,p_delta.diff = round(p_delta.44 - p_delta.36,3)
         ,peep_cm_h2o.diff = round(peep_cm_h2o.44 - peep_cm_h2o.36,3)
         ,med_ph.diff = (as.numeric(med_ph.44)-1) -(as.numeric(med_ph.36)-1))

trach_w44_long = trach_w44_long %>%
  dplyr::select(-c(weight_today.44,ventilation_support_level.44
                   ,inspired_oxygen.44,p_delta.44
                   ,peep_cm_h2o.44,med_ph.44))

# Fit lasso for the full imputed dataset
trach44_final = get_avg_coefs(trach_w44_long)

# Get coefficients
trach44_final[[1]]
logit2prob(trach44_final[[1]])