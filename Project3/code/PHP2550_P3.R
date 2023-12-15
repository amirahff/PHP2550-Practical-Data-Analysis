###############
### LIBRARY ###
###############

library(riskCommunicator)
library(tidyverse)
library(tableone)
library(MASS)
library(truncnorm)
library(ggplot2)
library(kableExtra)
library(patchwork)
# The NHANES data here finds the same covariates among this national survey data
library(nhanesA)

############
### SEED ###
############

set.seed(7)

#######################
### PRERPOCESS DATA ###
#######################

data("framingham")

# The Framingham data has been used to create models for cardiovascular risk.
# The variable selection and model below are designed to mimic the models used
# in the paper General Cardiovascular Risk Profile for Use in Primary Care 
# This paper is available (cvd_risk_profile.pdf) on Canvas.

framingham_df <- framingham %>% dplyr::select(c(CVD, TIMECVD, SEX, TOTCHOL, AGE,
                                                SYSBP, DIABP, CURSMOKE, DIABETES, BPMEDS,
                                                HDLC, BMI))
framingham_df <- na.omit(framingham_df)

# CreateTableOne(data=framingham_df, strata = c("SEX"))

# Get blood pressure based on whether or not on BPMEDS
framingham_df$SYSBP_UT <- ifelse(framingham_df$BPMEDS == 0, 
                                 framingham_df$SYSBP, 0)
framingham_df$SYSBP_T <- ifelse(framingham_df$BPMEDS == 1, 
                                framingham_df$SYSBP, 0)

# Looking at risk within 15 years - remove censored data
# dim(framingham_df)
framingham_df <- framingham_df %>%
  filter(!(CVD == 0 & TIMECVD <= 365*15)) %>%
  dplyr::select(-c(TIMECVD))
# dim(framingham_df)

# save(framingham_df, file = 'framingham_df.Rda')

# Filter to each sex
framingham_df_men <- framingham_df %>% filter(SEX == 1)
framingham_df_women <- framingham_df %>% filter(SEX == 2)

# save(framingham_df_men, file = 'framingham_df_men.Rda')
# save(framingham_df_women, file = 'framingham_df_women.Rda')

# blood pressure, demographic, bmi, smoking, and hypertension info
bpx_2017 <- nhanes("BPX_J") %>% 
  dplyr::select(SEQN, BPXSY1 ) %>% 
  rename(SYSBP = BPXSY1)
demo_2017 <- nhanes("DEMO_J") %>% 
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR) %>% 
  rename(SEX = RIAGENDR, AGE = RIDAGEYR)
bmx_2017 <- nhanes("BMX_J") %>% 
  dplyr::select(SEQN, BMXBMI) %>% 
  rename(BMI = BMXBMI)
smq_2017 <- nhanes("SMQ_J") %>%
  mutate(CURSMOKE = case_when(SMQ040 %in% c(1,2) ~ 1,
                              SMQ040 == 3 ~ 0, 
                              SMQ020 == 2 ~ 0)) %>%
  dplyr::select(SEQN, CURSMOKE)
bpq_2017 <- nhanes("BPQ_J") %>% 
  mutate(BPMEDS = ifelse(BPQ050A == 1, 1, 0)) %>%
  dplyr::select(SEQN, BPMEDS) 
tchol_2017 <- nhanes("TCHOL_J") %>% 
  dplyr::select(SEQN, LBXTC) %>% 
  rename(TOTCHOL = LBXTC)
hdl_2017 <- nhanes("HDL_J") %>% 
  dplyr::select(SEQN, LBDHDD) %>% 
  rename(HDLC = LBDHDD)
diq_2017 <- nhanes("DIQ_J") %>% 
  mutate(DIABETES = case_when(DIQ010 == 1 ~ 1, 
                              DIQ010 %in% c(2,3) ~ 0, 
                              TRUE ~ NA)) %>%
  dplyr::select(SEQN, DIABETES) 

# Join data from different tables
df_2017 <- bpx_2017 %>%
  full_join(demo_2017, by = "SEQN") %>%
  full_join(bmx_2017, by = "SEQN") %>%
  full_join(hdl_2017, by = "SEQN") %>%
  full_join(smq_2017, by = "SEQN") %>%
  full_join(bpq_2017, by = "SEQN") %>%
  full_join(tchol_2017, by = "SEQN") %>%
  full_join(diq_2017, by = "SEQN")

# save(df_2017, file = 'df_2017.Rda')
# CreateTableOne(data = df_2017, strata = c("SEX"))

#################
### FIT MODEL ###
#################

# Fit models with log transforms for all continuous variables
mod_men <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                 log(SYSBP_T+1)+CURSMOKE+DIABETES, 
               data= framingham_df_men, family= "binomial")

mod_men <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                 log(SYSBP_T+1)+CURSMOKE+DIABETES, 
               data= framingham_df_men, family= "binomial")

# save(mod_men, file = 'mod_men.Rda')

mod_women <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                   log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                 data= framingham_df_women, family= "binomial")

# save(mod_women, file = 'mod_women.Rda')

###################################################
### PREPARE TARGET POP (NON SIM) DATA FOR MODEL ###
###################################################

# Prepare the non simulated target population dataset for the models
nhanes_prep = df_2017 %>%
  dplyr::select(c(HDLC,TOTCHOL,AGE,SYSBP,BPMEDS,CURSMOKE,DIABETES,SEX)) %>%
  # Take complete data only
  na.omit() %>%
  # Generate SYSBP_UT and SYSBP_T, combination of SYSBP and BPMEDS
  # Generate S, the membership indication
  mutate(SYSBP_UT = ifelse(BPMEDS == 0, SYSBP, 0)
         ,SYSBP_T = ifelse(BPMEDS == 1, SYSBP, 0)
         ,S = 0) %>%
  mutate(CVD = NA) %>%
  dplyr::select(c(HDLC,TOTCHOL,AGE,SYSBP,BPMEDS,SYSBP_UT,SYSBP_T,CURSMOKE
                  ,DIABETES,SEX,S,CVD))

# save(nhanes_prep, file = 'nhanes_prep.Rda')

#########################################
### GET FRAMINGHAM AND NHANES SUMMARY ###
#########################################

# load('framingham_df.Rda')
# load('framingham_df_men.Rda')
# load('framingham_df_women.Rda')
# load('mod_men.Rda')
# load('mod_women.Rda')
# load('df_2017.Rda')
# load('nhanes_prep.Rda')

# Non Log Summary
sum_col = c('SEX','HDLC','TOTCHOL','AGE','SYSBP','CURSMOKE','DIABETES','BPMEDS')

# Get the statistics summary from Framingham
framingham_stat = CreateTableOne(data = framingham_df[,sum_col], strata = c("SEX"))
framingham_stat_tb = print(framingham_stat$ContTable)

# Get the statistics summary from NHANES
nhanes_stat = CreateTableOne(data = nhanes_prep[,sum_col], strata = c("SEX"))
nhanes_stat_tb = print(nhanes_stat$ContTable)

# Print statistics summary of Framingham
framingham_stat_tb %>%
  kableExtra::kbl(caption = 'Framingham Statistics Summary Stratified by Sex'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Sex=1(Male)','Sex=2(Female)', 'P-Value','')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , font_size = 10
                            # , position = 'float_left'
                            , latex_options = 'scale_down')

# Print statistics summary of NHANES
nhanes_stat_tb %>%
  kableExtra::kbl(caption = 'NHANES Statistics Summary Stratified by Sex'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Sex=1(Male)','Sex=2(Female)', 'P-Value','')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , font_size = 10
                            # , position = 'right'
                            , latex_options = 'scale_down')

# Log Data
# Get the statistics summary from framingham
framingham_log = framingham_df %>%
  mutate(LOG.TOTCHOL = log(TOTCHOL)
         ,LOG.SYSBP = log(SYSBP)
         ,LOG.AGE = log(AGE)
         ,LOG.HDLC = log(HDLC)) %>%
  dplyr::select(SEX,LOG.HDLC,LOG.TOTCHOL,LOG.AGE,LOG.SYSBP,CURSMOKE,DIABETES,BPMEDS)

logframingham_stat = CreateTableOne(data = framingham_log, strata = c("SEX"))
logframingham_stat_tb = print(logframingham_stat$ContTable)  

# Get the statistics summary from NHANES
nhanes_log = nhanes_prep %>%
  mutate(LOG.TOTCHOL = log(TOTCHOL)
         ,LOG.SYSBP = log(SYSBP)
         ,LOG.AGE = log(AGE)
         ,LOG.HDLC = log(HDLC)) %>%
  dplyr::select(SEX,LOG.HDLC,LOG.TOTCHOL,LOG.AGE,LOG.SYSBP,CURSMOKE,DIABETES,BPMEDS)

lognhanes_stat = CreateTableOne(data = nhanes_log, strata = c("SEX"))
lognhanes_stat_tb = print(lognhanes_stat$ContTable)  

# Print statistics summary of Log Framingham
logframingham_stat_tb %>%
  kableExtra::kbl(caption = 'Log Framingham Statistics Summary Stratified by Sex'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Sex=1(Male)','Sex=2(Female)', 'P-Value','')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , font_size = 10
                            , latex_options = 'scale_down')

# Print statistics summary of Log NHANES
lognhanes_stat_tb %>%
  kableExtra::kbl(caption = 'Log NHANES Statistics Summary Stratified by Sex'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Sex=1(Male)','Sex=2(Female)', 'P-Value','')) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , font_size = 10
                            , latex_options = 'scale_down')


##################################################
### OBSERVE DISTRIBUTIONS FROM SOURCE POP DATA ###
##################################################

# Non Log
# Get histogram from the framingham dataset continuous columns
fram_hist = vector('list',4)
cont_cols = c('TOTCHOL', 'SYSBP', 'AGE', 'HDLC')
it = 1
sex_label = c(`1` = 'Men',`2`='Women')

for (colname in cont_cols) {
  pl = ggplot(framingham_df) + 
    geom_histogram(aes_string(x = colname)) + 
    facet_grid(~SEX, labeller = as_labeller(sex_label)) + 
    theme_bw() + 
    labs(x = colname
         ,y = 'Count') + 
    theme(text = element_text(size=7)) 
  
  fram_hist[[it]] = pl
  it = it + 1
}

patchworkHist = (fram_hist[[1]] + fram_hist[[2]] + fram_hist[[3]] 
                 + fram_hist[[4]]) + plot_layout(nrow = 2, ncol = 2)
# save(patchworkHist, file='patchworkHist.Rda')

# Print histogram from the framingham dataset continuous columns
# load('patchworkHist.Rda')
patchworkHist + 
  plot_annotation(tag_levels = 'A'
                  ,title = "Histogram of Framingham Continuous Variables"
                  ,caption = 'Figure 1. Histogram of Framingham Continuous Variables'
                  ,theme = theme(plot.title = element_text(size = 10)
                                 ,plot.tag = element_text(size = 3)))


# Log
# Get histogram from the framingham dataset continuous columns
fram_hist2 = vector('list',4)
cont_cols2 = c('LOG.TOTCHOL', 'LOG.SYSBP', 'LOG.AGE', 'LOG.HDLC')
it = 1
sex_label = c(`1` = 'Men',`2`='Women')

for (colname in cont_cols2) {
  pl = ggplot(framingham_log) + 
    geom_histogram(aes_string(x = colname)) + 
    facet_grid(~SEX, labeller = as_labeller(sex_label)) + 
    theme_bw() + 
    labs(x = colname
         ,y = 'Count') + 
    theme(text = element_text(size=7)) 
  
  fram_hist2[[it]] = pl
  it = it + 1
}

patchworkHist2 = (fram_hist2[[1]] + fram_hist2[[2]] + fram_hist2[[3]] 
                 + fram_hist2[[4]]) + plot_layout(nrow = 2, ncol = 2)
# save(patchworkHist, file='patchworkLogHist.Rda')

# Print histogram from the framingham dataset continuous columns
# load('patchworkLogHist.Rda')
patchworkHist2 + 
  plot_annotation(tag_levels = 'A'
                  ,title = "Histogram of Framingham Log Continuous Variables"
                  ,caption = 'Figure 1. Histogram of Framingham Log Continuous Variables'
                  ,theme = theme(plot.title = element_text(size = 10)
                                 ,plot.tag = element_text(size = 3)))

########################
### DEFINE FUNCTIONS ###
########################

brier_transport = function(df) {
  #' Calculate the target population MSE/Brier Score based on Steingrimsson (2023)
  #' @param df, dataset including source population and target population
  #' @param return, MSE/Brier Score from df
  
  # Separate Source and Target Population
  s1 = df %>% filter(S==1)
  s0 = df %>% filter(S==0)
  
  # Calculate the nominator from the target population MSE/Brier Score
  s1 = s1 %>%
    mutate(noms = WEIGHT * (CVD - CVD_HAT)^2)
  nom = sum(s1$noms)
  
  # Calculate the denominator from the target population MSE/Brier Score
  denom = nrow(s0)
  
  # Calculate target population MSE/Brier Score
  result = nom/denom
  return(result)
}

transportability_analysis <- function(source_pop, target_pop, dist_shape='normal') {
  #' Calculate the target population MSE/Brier Score from specified source 
  #'    population dataset and target population dataset for men and women
  #' @param source_pop, dataset of source population
  #' @param target_pop, dataset of target population
  #' @param return, brier score for target population for men and women
  
  # Combine source and target population 
  combined_df = rbind(source_pop, target_pop) 
  # Separate men and women data from the combined dastaset
  combined_df_men = combined_df %>% filter(SEX == 1) 
  combined_df_women = combined_df %>% filter(SEX == 2)
  
  # Make the membership probability model for bot men and women
  pr_s_mod_men <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)
                      +log(SYSBP_T+1)+CURSMOKE+DIABETES
                      , data = combined_df_men, family= "binomial")
  
  pr_s_mod_women <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)
                        +log(SYSBP_T+1)+CURSMOKE+DIABETES
                        , data = combined_df_women, family= "binomial")
  
  # Only take relevant columns to be modeled
  combined_df_mod = combined_df[,c('HDLC','TOTCHOL','AGE','SYSBP_UT'
                                   ,'SYSBP_T','CURSMOKE','DIABETES')]
  
  # Get the Pr(S=1|X) for men and women
  pr_S_m = predict(pr_s_mod_men, newdata = combined_df_mod, type = 'response')
  pr_S_f = predict(pr_s_mod_women, newdata = combined_df_mod, type = 'response')
  # Get the inverse weight for men and women
  weight_m =  (1-pr_S_m) / pr_S_m
  weight_f =  (1-pr_S_f) / pr_S_f
  
  # Get the CVD_hat (y_hat) for men and women
  cvd_hat_m = predict(mod_men, newdata = combined_df_mod , type = 'response')
  cvd_hat_f = predict(mod_women, newdata = combined_df_mod , type = 'response')
  
  # Put the inverse weight and CVD_hat to the combined dataset
  combined_df = combined_df %>%
    mutate(WEIGHT_M = weight_m
           ,WEIGHT_F = weight_f
           ,CVD_HAT_M = cvd_hat_m
           ,CVD_HAT_F = cvd_hat_f) %>%
    mutate(WEIGHT = ifelse(SEX == 1, WEIGHT_M, WEIGHT_F)
           ,CVD_HAT = ifelse(SEX == 1, CVD_HAT_M, CVD_HAT_F)) %>%
    dplyr::select(-c(WEIGHT_M,WEIGHT_F,CVD_HAT_M,CVD_HAT_F))
  
  
  # Calculate Target Population MSE/Brier Score
  result_m = brier_transport(combined_df %>% filter(SEX == 1))
  result_f = brier_transport(combined_df %>% filter(SEX == 2))
  return(c(result_m,result_f))
}

data_gen <- function(n, corr_any, corr_n, corr, dist_shape) {
  #' Generate dataset based on NHANES statistics summary
  #'    and use Framingham continuous distribution shape (ori) or use normal dist
  #' @param n, number of observations in dataset 
  #' @param corr_any, apply correlation in continuous dataset, Yes or No
  #' @param corr_n, how many continuous columns that have correlation
  #' @param corr, correlation magnitude
  #' @param dist_shape, distribution shape for the continuous columns
  #' @param return, final dataset
  
  # Stopping Criterions
  if(!corr_any %in% c(TRUE, FALSE)) {
    stop("corr_any must be TRUE/FALSE")
  }
  
  if((corr_n == FALSE) & (corr_n !=0)) {
    stop("corr_n must be 0 if corr_any is FALSE")
  }
  
  if((corr_n == FALSE) & (!corr_n %in% c(2,3,4))) {
    stop("corr_n must be 2/3/4 if corr_any is TRUE")
  }
  
  if(abs(corr) > 1) {
    stop("Corr cannot be more than 1 or less than -1")
  }
  
  if(!dist_shape %in% c('normal', 'ori')) {
    stop("corr_any must be ori/normal")
  }
  
  # Determine the columns that has correlation
  if(corr_any == TRUE) {
    corr_idxs = sample(1:4, corr_n)
  }
  
  # Initialize final dataset 
  final_df = data.frame()
  # Generate data for men (SEX = 1) and women (SEX = 2)
  for (i in 1:2) {
    if (i == 1) {
      # Generate categorical columns for men bases on NHANES statistics summary
      n_sex = round(n * 0.50)
      sex = rep(i, n_sex)
      bpmeds = rbinom(n_sex, 1, 0.85)
      cursmoke = rbinom(n_sex, 1, 0.17)
      diabetes = rbinom(n_sex, 1, 0.35)      
      
      # Specify mean and sd for men bases on NHANES statistics summary
      # 1)TOTCHOL, 2)SYSBP, 3)AGE, 4)HDLC
      # If distribution shape is similar to framingham/lognormal
      if(dist_shape == 'ori') {
        means = c(5.15, 4.89, 4.10, 3.81)
        sds = c(0.24, 0.14, 0.25, 0.27)
      }
      
      # If distribution shape is normal
      if(dist_shape == 'normal') {
        means = c(177.14, 134.79, 61.88, 47.08)
        sds = c(42.36, 18.90, 13.45, 14.15)
      }
      
    } 
    else {
      # Generate categorical columns for women bases on NHANES statistics summary
      n_sex = n - round(n * 0.49)
      sex = rep(i, n_sex)
      bpmeds = rbinom(n_sex, 1, 0.86)
      cursmoke = rbinom(n_sex, 1, 0.14)
      diabetes = rbinom(n_sex, 1, 0.09)
      
      # Specify mean and sd for women bases on NHANES statistics summary
      # 1)TOTCHOL, 2)SYSBP, 3)AGE, 4)HDLC
      # If distribution shape is similar to framingham/lognormal
      if(dist_shape == 'ori') {
        means = c(5.25, 4.92, 4.12, 4.01)
        sds = c(0.21, 0.15, 0.24, 0.27)
      }
      
      # If distribution shape is normal
      if(dist_shape == 'normal') {
        means = c(194.63,138.58,62.98,57.43)
        sds = c(42.21,21.52,12.87,16.36)
      }
    }
    
    # Generate continuous columns based on determined dist_shape
    continuous_list = vector('list',4)
    for (j in 1:4) {
      # Get mean and sd for the corresponding continuous columns
      # 1)TOTCHOL, 2)SYSBP, 3)AGE, 4)HDLC
      m = means[j]
      s = sds[j]
      
      # If dist_shape == 'ori' then use framingham distribution shape, lognormal
      if (dist_shape == 'ori') {
        dat = rnorm(n_sex, m, s)
      }
      
      # If dist_shape == 'normal' then use normal distribution shape
      if (dist_shape == 'normal') {
        dat = rnorm(n_sex, m, s)
        # For age, truncate the data, min = 1, max = 81 (based on framingham)
        if(j == 3) {dat = rtruncnorm(n = n_sex,a = 1,b = 81,mean = m,sd = s)}
      }
      
      # Combine all continuous columns
      continuous_list[[j]] = dat
    }
    
    # Make the continuous columns correlated if corr_any == TRUE 
    if (corr_any == TRUE) {
      
      # Generate multivariate normal dataset that correspond with the corr value
      corr_mat = matrix(corr, ncol = corr_n, nrow = corr_n)
      diag(corr_mat) = 1
      mvdat = mvrnorm(n = n_sex, mu = rep(0,corr_n), Sigma = corr_mat, empirical = TRUE)
      
      # Apply the correlation to the existing continuous columns by using the 
      #   sorted position of the multivariate normal dataset
      k = 1
      for (corr_idx in corr_idxs) {
        
        # Sort multivariate normal dataset
        rnk = rank(mvdat[ , k], ties.method = "first")
        # Arrange the continuous columns based on the previous sorting
        dat_sorted = sort(continuous_list[[corr_idx]])
        # Replace the original continuous columns
        continuous_list[[corr_idx]] = dat_sorted[rnk]
        k = k + 1
      }
      
      # Remove mvdat var to avoid mvrnomr overwrite error
      rm(mvdat)
    }
    
    # Make dataset based on the continuous and categorical columns      
    continuous_df = data.frame(TOTCHOL = continuous_list[[1]]
                               , SYSBP = continuous_list[[2]]
                               , AGE = continuous_list[[3]]
                               , HDLC = continuous_list[[4]])
    if(dist_shape == 'ori') {
      continuous_df = continuous_df %>%
        mutate(TOTCHOL = exp(TOTCHOL)
               , SYSBP = exp(SYSBP)
               , AGE = exp(AGE)
               , HDLC = exp(HDLC))
    }
    continuous_df = continuous_df[sample(nrow(continuous_df)),]
    
    df_tmp = data.frame(SEX = sex
                        , BPMEDS = bpmeds
                        , CURSMOKE = cursmoke
                        , DIABETES = diabetes
                        , continuous_df)
    
    # Append to final dataset
    final_df = rbind(final_df, df_tmp)
  }
  
  # Preprocess dataset so the structure is accepted by the models
  final_df =  final_df %>%
    mutate(S = 0
           , CVD = NA
           , SYSBP_UT = ifelse(BPMEDS == 0, SYSBP, 0)
           , SYSBP_T = ifelse(BPMEDS == 1, SYSBP, 0)
    ) %>%
    dplyr::select(HDLC, TOTCHOL, AGE, SYSBP, BPMEDS, SYSBP_UT, SYSBP_T
                  , CURSMOKE, DIABETES, SEX, S, CVD)
  
  # Return dataset
  return(final_df)
  
}

# save(brier_transport, file = 'brier_transport.Rda')
# save(transportability_analysis, file = 'transportability_analysis.Rda')
# save(data_gen, file = 'data_gen.Rda')

# load('brier_transport.Rda')
# load('transportability_analysis.Rda')
# load('data_gen.Rda')

##################################################
### TRANSPORTABILITY ANALYSIS FOR NON SIM DATA ###
##################################################

# Apply transportability analysis to the non simulated target population dataset
nonsimulated_result = transportability_analysis(framingham_prep, nhanes_prep)
nonsimulated_result # men 0.12673996 women 0.09572181

####################################################
### GENERATE INITIAL ESTIMATES FOR EXTREME CASES ###
####################################################

# Initial Simulation
init_briers = replicate(1000, {
  dataa = data_gen(n=2359, corr_any=TRUE, corr_n=4, corr=1, dist_shape='normal')
  transportability_analysis(framingham_prep, dataa)
})

# Define num_sim
apply(init_briers,1,'var')/(0.005)^2 #men 96.91471 women 3154.49095


corr_ns = c(2,4)
corrs = c(0, 0.3, 0.7,1)
shapes = c('ori','normal')
num_rep = 3154
num_obs = 2539

###########################
### GENERATE SIMULATION ###
###########################

# Simulation to get the target population MSE/Brier score based on the 
#   specified values of shapes and corrs
outputs = data.frame()
for (shape in shapes) {
  for (corr in corrs) {
    for (corr_n in corr_ns) {
      
      if (corr == 0) {corr_any = FALSE; if (corr_n == 4) {next}}
      else {corr_any = TRUE}
      
      results = replicate(num_rep, {
        dataa = data_gen(n = num_obs
                         , corr_any = corr_any, corr_n = corr_n, corr = corr
                         , dist_shape = shape)
        transportability_analysis(framingham_prep, dataa)
      })
      outputs_tmp <- data.frame(ta_m = t(results)[,1], ta_f = t(results)[,2] 
                                ,shape = shape, corr = corr, corr_n = corr_n)
      outputs = rbind(outputs, outputs_tmp)
    }
  }
}

# save(outputs, file = 'outputs.Rda')

############################
### PERFORMANCE MEASURES ###
############################

# load('outputs.Rda')

mc_bias <- function(estimates) {
  #' Calculate monte carlo relative bias using the mean estimates instead of 
  #'    true estimates
  #' @param estimates, set of estimates
  #' @param return, monte carlo relative bias of the estimates
  nsim = length(estimates)
  mean_estimates = mean(estimates)
  est_diff_squared = (estimates - mean_estimates)^2
  result = sqrt(1/(nsim *(nsim-1)) * sum(est_diff_squared))
  return(result)
}

mc_empSE <- function(estimates) {
  #' Calculate monte carlo empirical SE 
  #' @param estimates, set of estimates
  #' @param return, monte carlo empirical SE
  nsim = length(estimates)
  mean_estimates = mean(estimates)
  est_diff_squared = (estimates - mean_estimates)^2
  est_emp_SE = sqrt(1/(nsim-1) * sum(est_diff_squared))
  result = est_emp_SE / sqrt(2*(nsim-1))
  return(result)
}

mc_MSE <- function(estimates, true_estimate) {
  #' Calculate monte carlo MSE 
  #' @param estimates, set of estimates
  #' @param return, monte carlo MSE
  nsim = length(estimates)
  est_diff_squared = (estimates - true_estimate)^2
  est_MSE = (1/(nsim) * sum(est_diff_squared))
  result = sqrt( sum((est_diff_squared - est_MSE)^2) / ((nsim)*(nsim-1)))
  return(result)
}

# Calculate the performance measures
eval_summary <- outputs %>%
  group_by(shape, corr, corr_n) %>%
  mutate(relative_bias_m = mc_bias(ta_m)
         ,empSE_m = mc_empSE(ta_m)
         ,MSE_m = mc_MSE(ta_m, nonsimulated_result[1])
         ,avg_ta_m = mean(ta_m)
         ,relative_bias_f = mc_bias(ta_f)
         ,empSE_f = mc_empSE(ta_f)
         ,MSE_f = mc_MSE(ta_f, nonsimulated_result[2])
         ,avg_ta_f = mean(ta_f)
  )%>%
  ungroup() %>%
  dplyr::select(-c('ta_m','ta_f')) %>%
  group_by(shape, corr, corr_n) %>%
  slice(1) %>%
  mutate_if(is.numeric, round, 6)

# save(eval_summary, file='eval_summary.Rda' )
# load('eval_summary.Rda')

############################################
### DISPLAY PERFORMANCE MEASURES : TABLE ###
############################################

# Display performance measures table
eval_summary %>%
  dplyr::select(c(shape, corr, corr_n, relative_bias_m, empSE_m, MSE_m, avg_ta_m
                  ,relative_bias_f, empSE_f, MSE_f, avg_ta_f)) %>%
  arrange(MSE_m,MSE_f) %>%
  kableExtra::kbl(caption = 'Performance Measure Comparison'
                  , booktabs = T
                  , escape = T
                  , align = 'c'
                  , col.names = c('Shape', 'Corr Score', 'Corr N', 'Relative Bias M'
                                  , 'Empirical SE M','MSE M', 'Avg TA M'
                                  ,'Relative Bias F', 'Empirical SE F'
                                  ,'MSE F','Avg TA F')) %>%
  kableExtra::kable_classic(full_width = F
                            , font_size = 9 
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')


############################################
### DISPLAY PERFORMANCE MEASURES : PLOTS ###
############################################

# Initialization
all_plot_men = vector('list',4)
all_plot_women = vector('list',4)
iter = 1

for (i in c('M','F')) {
  if(i == 'M') {
    metrics_col = c('relative_bias_m', 'empSE_m', 'MSE_m', 'avg_ta_m')
    nonsim_res = 0.1267
    
    # Avg TA Brier Score
    plot1 = ggplot(eval_summary) + 
      geom_point(aes_string(x = 'factor(corr)', y = metrics_col[4]
                            , fill = 'factor(corr_n)', shape = 'shape')
                 , size = 3) + 
      geom_hline(data = eval_summary, aes(yintercept = nonsim_res
                                          , colour = "0.1267"), show_guide=TRUE) +
      scale_shape_manual(name= 'Continuous Distribution'
                         ,values = c(21,25),labels=c('Normal','Log Normal')) +
      scale_fill_manual(name= '# Correlated Vars'
                        ,values = c('lightblue','salmon')) +
      scale_color_manual(name = 'Non Simulated Estimate'
                         , values = c("0.1267" = "springgreen4"))+
      theme_bw() + 
      labs(x = 'Correlation Score'
           ,y = 'Average Transportability Brier Score') + 
      theme(text = element_text(size=7)
            ,legend.title=element_text(size=5)
            ,legend.text=element_text(size=5)
            ,legend.box.background = element_rect(color = "black")) + 
      guides(fill = guide_legend("# Correlated Vars"
                                 , override.aes = list(shape = 21)))
  }
  
  if(i == 'F') {
    metrics_col = c('relative_bias_f', 'empSE_f', 'MSE_f', 'avg_ta_f')
    nonsim_res = 0.0957
    
    # Avg TA Brier Score
    plot1 = ggplot(eval_summary) + 
      geom_point(aes_string(x = 'factor(corr)', y = metrics_col[4]
                            , fill = 'factor(corr_n)', shape = 'shape')
                 , size = 3) + 
      geom_hline(data = eval_summary, aes(yintercept = nonsim_res
                                          , colour = "0.0957"), show_guide=TRUE) +
      scale_shape_manual(name= 'Continuous Distribution'
                         ,values = c(21,25),labels=c('Normal','Log Normal')) +
      scale_fill_manual(name= '# Correlated Vars'
                        ,values = c('lightblue','salmon')) +
      scale_color_manual(name = 'Non Simulated Estimate'
                         , values = c("0.0957" = "springgreen4"))+
      theme_bw() + 
      labs(x = 'Correlation Score'
           ,y = 'Average Transportability Brier Score') + 
      theme(text = element_text(size=7)
            ,legend.title=element_text(size=5)
            ,legend.text=element_text(size=5)
            ,legend.box.background = element_rect(color = "black")) + 
      guides(fill = guide_legend("# Correlated Vars"
                                 , override.aes = list(shape = 21)))
  }
  
  # Avg Relative Bias
  plot2 = ggplot(eval_summary) + 
    geom_point(aes_string(x = 'factor(corr)', y = metrics_col[1]
                          , fill = 'factor(corr_n)', shape = 'shape')
               , size = 3) + 
    geom_hline(data = eval_summary, aes(yintercept = 0
                                        , colour = "0"), show_guide=TRUE) +
    scale_shape_manual(name= 'Continuous Distribution'
                       ,values = c(21,25),labels=c('Normal','Log Normal')) +
    scale_fill_manual(name= '# Correlated Vars'
                      ,values = c('lightblue','salmon')) +
    scale_color_manual(name = 'Ideal Relative Bias'
                       , values = c("0" = "springgreen4"))+
    theme_bw() + 
    labs(x = 'Correlation Score'
         ,y = 'Relative Bias') + 
    theme(text = element_text(size=7)
          ,legend.title=element_text(size=5)
          ,legend.text=element_text(size=5)
          ,legend.box.background = element_rect(color = "black")) + 
    guides(fill = guide_legend("# Correlated Vars"
                               , override.aes = list(shape = 21)))
  
  # Avg EmpSE
  plot3 = ggplot(eval_summary) + 
    geom_point(aes_string(x = 'factor(corr)', y = metrics_col[2]
                          , fill = 'factor(corr_n)', shape = 'shape')
               , size = 3) + 
    geom_hline(data = eval_summary, aes(yintercept = 0
                                        , colour = "0"), show_guide=TRUE) +
    scale_shape_manual(name= 'Continuous Distribution'
                       ,values = c(21,25),labels=c('Normal','Log Normal')) +
    scale_fill_manual(name= '# Correlated Vars'
                      ,values = c('lightblue','salmon')) +
    scale_color_manual(name = 'Ideal Empirical SE'
                       , values = c("0" = "springgreen4"))+
    theme_bw() + 
    labs(x = 'Correlation Score'
         ,y = 'Empirical SE') + 
    theme(text = element_text(size=7)
          ,legend.title=element_text(size=5)
          ,legend.text=element_text(size=5)
          ,legend.box.background = element_rect(color = "black")) + 
    guides(fill = guide_legend("# Correlated Vars"
                               , override.aes = list(shape = 21)))
  
  # Avg MSE
  plot4 = ggplot(eval_summary) + 
    geom_point(aes_string(x = 'factor(corr)', y = metrics_col[3]
                          , fill = 'factor(corr_n)', shape = 'shape')
               , size = 3) + 
    geom_hline(data = eval_summary, aes(yintercept = 0
                                        , colour = "0"), show_guide=TRUE) +
    scale_shape_manual(name= 'Continuous Distribution'
                       ,values = c(21,25),labels=c('Normal','Log Normal')) +
    scale_fill_manual(name= '# Correlated Vars'
                      ,values = c('lightblue','salmon')) +
    scale_color_manual(name = 'Ideal MSE'
                       , values = c("0" = "springgreen4"))+
    theme_bw() + 
    labs(x = 'Correlation Score'
         ,y = 'MSE') + 
    theme(text = element_text(size=7)
          ,legend.title=element_text(size=5)
          ,legend.text=element_text(size=5)
          ,legend.box.background = element_rect(color = "black")) + 
    guides(fill = guide_legend("# Correlated Vars"
                               , override.aes = list(shape = 21)))
  
  
  if (i == 'M') {
    all_plot_men[[1]] = plot1
    all_plot_men[[2]] = plot2
    all_plot_men[[3]] = plot3
    all_plot_men[[4]] = plot4
  }
  if (i == 'F') {
    all_plot_women[[1]] = plot1
    all_plot_women[[2]] = plot2
    all_plot_women[[3]] = plot3
    all_plot_women[[4]] = plot4
  }
}

# Combine plots for men
patchwork_men = (all_plot_men[[1]] + all_plot_men[[2]] + all_plot_men[[3]] 
                 + all_plot_men[[4]]) + plot_layout(nrow = 2, ncol = 2)
# save(patchwork_men, file='patchwork_men.Rda')

# Combine plots for men
patchwork_women = (all_plot_women[[1]] + all_plot_women[[2]] + all_plot_women[[3]] 
                   + all_plot_women[[4]]) + plot_layout(nrow = 2, ncol = 2)
# save(patchwork_women, file='patchwork_women.Rda')

# load('patchwork_men.Rda')
# load('patchwork_women.Rda')

options(repr.plot.width=6, repr.plot.height=4)
patchwork_men + 
  plot_annotation(tag_levels = 'A'
                  ,title = "Transportability Analysis Performance Measures for Men"
                  ,caption = 'Figure 1. Transportability Analysis Performance Measures for Men'
                  ,theme = theme(plot.title = element_text(size = 10)
                                 ,plot.tag = element_text(size = 3)))


options(repr.plot.width=6, repr.plot.height=4)
patchwork_women + 
  plot_annotation(tag_levels = 'A'
                  ,title = "Transportability Analysis Performance Measures for Women"
                  ,caption = 'Figure 1. Transportability Analysis Performance Measures for Women'
                  ,theme = theme(plot.title = element_text(size = 10)
                                 ,plot.tag = element_text(size = 3)))

