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
library(tableone)

###########################
### PREPROCESS RAW DATA ###
###########################

# first process child data
setwd("/Users/amirahff/Documents/Brown Biostatistics/PHP 2550/project1")
child_df <- read.csv("K01BB.csv")

child_df <- child_df %>%
  select(c(participant_id:su_interview_complete))  %>%
  filter(redcap_event_name == "child_baseline_arm_1")

# select demographic variables
child_df <- child_df %>% 
  select(-c(participant_id, part, lastgrade, redcap_event_name, famid, 
            visit_date, time, redcap_survey_identifier, enroll_timestamp, 
            handednesst, tgender, sexorient, whichlang, nativelang, traceoth,
            usborn, relation, guardian, livewith___0:livewith___7, 
            attendance, demographics_complete, langpref, pacemaker, 
            longlive)) %>%
  rename(taian = trace___0, tasian = trace___1, tnhpi = trace___2, 
         tblack = trace___3, twhite = trace___4, trace_other = trace___5)

# drop brief because scoring difficult
child_df <- child_df %>%
  select(-c(brief_ysr_timestamp:brief_ysr_complete))

# cigarette usage summarize
child_df <- child_df %>%
  mutate(cig_ever = suc1, num_cigs_30 = suc11) %>%
  select(-c(suc1:honc10))

# e-cig usage summarize
child_df <- child_df %>%
  mutate(e_cig_ever = ecig1, num_e_cigs_30 = ecig4) %>%
  select(-c(ecig1:ehonc10))

# marijuana usage summarize
child_df <- child_df %>%
  mutate(mj_ever = mj1, num_mj_30 = mj8) %>%
  select(-c(mj1:mpi29))

# alchohol usage summarize
child_df <- child_df %>%
  mutate(alc_ever = alc2, num_alc_30 = alc7) %>%
  select(-c(alc1:alcsus3))

# other drugs and norms - dropping 
child_df <- child_df %>%
  select(-c(odrg1:othdrglist,
            perceived_norms_peers_timestamp:perceived_norms_peers_complete,
            substance_use_cigarettes_timesta:substance_use_other_drug_use_com))

# brief problem monitor scoring
child_df <- child_df %>%
  mutate(bpm_att = rowSums(dplyr::select(., c(bpm1,bpm3,bpm4,bpm5,bpm10))),
         bpm_ext = rowSums(dplyr::select(., c(bpm2,bpm6,bpm7,bpm8,bpm15,
                                              bpm16,bpm17))),
         bpm_int = rowSums(dplyr::select(., c(bpm9,bpm11,bpm12,bpm13,bpm18,
                                              bpm19)))) %>%
  select(-c(brief_problem_monitor_timestamp:brief_problem_monitor_complete))

# emotional regulation
child_df <- child_df %>%
  mutate(erq_cog = rowMeans(dplyr::select(., c(erq1,erq3,erq5,erq7,
                                               erq8,erq10))),
         erq_exp = rowMeans(dplyr::select(., c(erq2,erq4,erq6,
                                               erq9)))) %>%
  select(-c(emotion_regulation_questionnaire:emotion_regulation_questionnair1))

# physical - dropping for the purpose of this research
child_df <- child_df %>%
  select(-c(physical_development_scale_ysr_t:physical_development_scale_ysr_c,
            height1:body_measurements_complete))

# life stress - dropping for the purpose of this research
child_df <- child_df %>%
  select(-c(life_stress_ysr_timestamp:life_stress_ysr_complete))

# parental monitoring scoring
child_df <- child_df %>%
  mutate(pmq_parental_knowledge = (pmq1+pmq2+pmq3+pmq4+pmq5+pmq6+
                                     pmq7+pmq8+(5-pmq9))/9,
         pmq_child_disclosure = (pmqcd1+pmqcd2+(5-pmqcd3)+(5-pmqcd4)+pmqcd5)/5,
         pmq_parental_solicitation = rowMeans(dplyr::select(., pmqps1:pmqps5)),
         pmq_parental_control = rowMeans(dplyr::select(., pmqpc1:pmqpc5))) %>%
  select(-c(parental_monitoring_questionnair:parental_monitoring_questionnai1))

# dysregulation - drop to simplify analysis
child_df <- child_df %>%
  select(-c(dysregulation_inventory_ysr_time:dysregulation_inventory_ysr_comp))

# early adolescent temperament - drop to simplify analysis
child_df <- child_df %>%
  select(-c(early_adolescent_temperament_que:early_adolescent_temperament_qu1))

# alcohol and substance abuse - too few observed so remove
child_df <- child_df %>%
  select(-c(miniaud1:minikid_sud_2_complete))

# remove remaining diet questions for purposes of this research
child_df <- child_df %>%
  select(-c(intuitive_eating_scale_timestamp:su_interview_complete))

# parent df
parent_df <- read.csv("K01BB.csv") %>%
  filter(redcap_event_name == "parent_baseline_arm_2") %>%
  select(c(parent_id, page:chart23)) 

# demographics
parent_df <- parent_df %>%
  select(-c(pgender, marstat, handednessp, plang1:plang3,
            praceoth, ppacemaker, pusa, pedu1:pedu3,
            prelation:parent_demographics_complete, govtasst___0:govtasst___5,
            parent_demographics_asd_timestam, 
            parent_demographics_asd_complete)) %>%
  rename(paian = prace___0, pasian = prace___1, pnhpi = prace___2, 
         pblack = prace___3, pwhite = prace___4, prace_other = prace___5)

# brief - dropping for difficulty scoring
parent_df <- parent_df %>%
  select(-c(brief_p_on_c_timestamp:brief_p_on_c_complete))

# swan - p on c
parent_df <- parent_df %>%
  mutate(swan_inattentive = rowSums(dplyr::select(., swan1:swan9), 
                                    na.rm=TRUE),
         swan_hyperactive = rowSums(dplyr::select(., swan10:swan18), 
                                    na.rm=TRUE)) %>%
  select(-c(swan_p_on_c_timestamp:swan_p_on_c_complete))

# connors - drop because swan will be similar
parent_df <- parent_df %>%
  select(-c(connors_p_on_c_timestamp:connors_p_on_c_complete))

# pbpm - parent answering about child
parent_df <- parent_df %>%
  mutate(bpm_att_p = rowSums(dplyr::select(., c(pbpm1,pbpm3,pbpm4,pbpm5,pbpm10))),
         bpm_ext_p = rowSums(dplyr::select(., c(pbpm2,pbpm6,pbpm7,pbpm8,pbpm15,
                                                pbpm16,pbpm17))),
         bpm_int_p = rowSums(dplyr::select(., c(pbpm9,pbpm11,pbpm12,pbpm13,pbpm18,
                                                pbpm19)))) %>%
  select(-c(bpm_p_on_c_timestamp:bpm_p_on_c_complete))

# alc and drug use
parent_df <- parent_df %>%
  mutate(magic2 = ifelse(magic1 == 0, 0, magic2),
         magic5 = ifelse(magic4 == 0, 0, magic5),
         smoke_exposure_6mo = pmax(magic2, magic5),
         magic8 = ifelse(magic7 == 0, 0, magic8),
         magic11 = ifelse(magic10 == 0, 0, magic11),
         smoke_exposure_12mo = pmax(magic8, magic11),
         magic14 = ifelse(magic13 == 0, 0, magic14),
         magic17 = ifelse(magic16 == 0, 0, magic17),
         smoke_exposure_2yr = pmax(magic14, magic17),
         magic20 = ifelse(magic19 == 0, 0, magic20),
         magic23 = ifelse(magic22 == 0, 0, magic23),
         smoke_exposure_3yr = pmax(magic20, magic23),
         magic26 = ifelse(magic25 == 0, 0, magic26),
         magic29 = ifelse(magic28 == 0, 0, magic29),
         smoke_exposure_4yr = pmax(magic26, magic29),
         magic32 = ifelse(magic31 == 0, 0, magic32),
         magic35 = ifelse(magic34 == 0, 0, magic35),
         smoke_exposure_5yr = pmax(magic32, magic35)
  ) %>%
  select(-c(nidaliftetime___1:inject,penncig2:penn_state_ecigarette_dependenc1,
            penn_state_cigarette_dependence_, 
            nida_quick_screen_timestamp,
            nida_quick_screen_complete, magic_timestamp:magic_complete)) %>%
  rename(mom_numcig = penncig1)



# brief - dropping because difficulty scoring
parent_df <- parent_df %>%
  select(-c(briefa_timestamp:briefa_complete))

# parental monitoring - parent answering on child
parent_df <- parent_df %>%
  mutate(ppmq_parental_knowledge = (ppmq1+ppmq2+ppmq3+ppmq4+ppmq5+ppmq6+
                                      ppmq7+ppmq8+(5-ppmq9))/9,
         ppmq_child_disclosure = (ppmqcd1+ppmqcd2+(5-ppmqcd3)+(5-ppmqcd4)
                                  +ppmqcd5)/5,
         ppmq_parental_solicitation = rowMeans(dplyr::select(., ppmqps1:ppmqps5)),
         ppmq_parental_control = rowMeans(dplyr::select(., ppmqpc1:ppmqpc5))) %>%
  select(-c(ppmq1:ppmqps5,parental_monitoring_questionnai2,
            parental_monitoring_questionnai3))

# chaos - dropping for purposes of this research
parent_df <- parent_df %>%
  select(-c(chaos_timestamp:chaos_complete))

# bpm adult
parent_df <- parent_df %>%
  mutate(bpm_att_a = rowSums(dplyr::select(., c(abpm1,abpm6,abpm7,abpm8,abpm9,
                                                abpm12))),
         bpm_ext_a = rowSums(dplyr::select(., c(abpm3,abpm13,abpm14,abpm17,
                                                abpm18))),
         bpm_int_a = rowSums(dplyr::select(., c(abpm2,abpm4,abpm5,abpm10,abpm15,
                                                abpm16)))) %>%
  select(-c(brief_problem_monitoradult_times:brief_problem_monitoradult_compl))

# parent emotional regulation 
parent_df <- parent_df %>%
  mutate(erq_cog_a = rowMeans(dplyr::select(., c(perq1,perq3,perq5,perq7,
                                                 perq8,perq10))),
         erq_exp_a = rowMeans(dplyr::select(., c(perq2,perq4,perq6,
                                                 perq9)))) %>%
  select(-c(emotion_regulation_questionnair2:emotion_regulation_questionnair3))

# adult temperament - drop to simplify analysis
parent_df <- parent_df %>%
  select(-c(adult_temperament_questionnaire_:adult_temperament_questionnaire1))

# etq - drop to simplify analysis
parent_df <- parent_df %>%
  select(-c(eatq_p_on_c_timestamp:eatq_p_on_c_complete))

# stress - dropping for purposes of this research
parent_df <- parent_df %>%
  select(-c(nih_toolbox_stress_timestamp:teen_birthday_complete))

# reported smoking during pregnancy and postpartum
parent_df <- parent_df %>% 
  select(-c(BBID:ethn2, bl_6:bl_280, s2_10:s2_280, s3_6:s3_280, 
            s4_6:s4_280,  s5_6:s5_280, s6_6:s6_280, s7_6:s7_280, 
            chart21A:chart23) )   %>%
  rename(mom_smoke_16wk = bl_5,
         mom_smoke_22wk = s2_5, 
         mom_smoke_32wk = s3_5,
         mom_smoke_pp1 = s4_5,
         mom_smoke_pp2 = s5_5,
         mom_smoke_pp12wk = s6_5,
         mom_smoke_pp6mo = s7_5,
         cotimean_34wk = wk34cot_cotimean,
         cotimean_pp6mo = mo6momcot_cotimean,
         cotimean_pp6mo_baby = mo6babcot_cotimean)


new_df <- inner_join(parent_df, child_df, by = "parent_id")
# write.csv(new_df, "project1.csv", row.names=FALSE)

####################
### MISSING DATA ###
####################

# How many?
n_miss(new_df)
prop_miss(new_df)

# Getting the frequency table of how many missing data each family has
missingVar = table(rowSums(sapply(new_df, is.na))) 
as.data.frame(t(as.matrix(missingVar))) %>%
  mutate(' ' = c('# Family'), .before = 0) %>%
  kableExtra::kbl(caption = 'Frequencies of Total Variables Missing for 
                  Each Family'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

# Getting the frequency table of how many missing data each variable has
missingObs = table(colSums(sapply(new_df, is.na))) 
as.data.frame(t(as.matrix(missingObs))) %>%
  mutate(' ' = c('# Variables'), .before = 1) %>%
  kableExtra::kbl(caption = 'Frequencies of Total Observations Missing for 
                  Each Variable'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position') 

# Variables' missing data proportion 
varMissingProp = miss_var_summary(new_df)
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
missingPatternTable = miss_case_table(new_df)
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
gg_miss_upset(new_df, nsets = 5) 

# Finding out which ids have more than 40 missing variables
new_df$na_count = apply(is.na(new_df), 1, sum)

# Filter out ids that has more than 40 missing variables 
new_df = new_df %>%
  filter(na_count < 47)

##############################
### GENERATE NEW VARIABLES ###
##############################

# Generate new variables, modify and rename some variables
new_df2 = new_df %>%
         # Simplify mom smoke indicator
  mutate(mom_smoke_16wk = factor(mom_smoke_16wk, levels=c('2=No','1=Yes')
                                 , labels = c(0,1))
         , mom_smoke_22wk = factor(mom_smoke_22wk, levels=c('2=No','1=Yes')
                                   , labels = c(0,1))
         , mom_smoke_32wk = factor(mom_smoke_32wk, levels=c('2=No','1=Yes')
                                   , labels = c(0,1))
         , mom_smoke_pp1 = factor(mom_smoke_pp1, levels=c('2=No','1=Yes')
                                  , labels = c(0,1))
         , mom_smoke_pp2 = factor(mom_smoke_pp2, levels=c('2=No','1=Yes')
                                  , labels = c(0,1))
         , mom_smoke_pp12wk = factor(mom_smoke_pp12wk, levels=c('2=No','1=Yes')
                                     , labels = c(0,1))
         , mom_smoke_pp6mo = factor(mom_smoke_pp6mo, levels=c('2=No','1=Yes')
                                    , labels = c(0,1))
         # Make new indicator combining cig & ecig
         , cig = pmax(cig_ever,e_cig_ever)
         , drug = mj_ever
         , alc = alc_ever
         # Make tobacco consumption indicator
         , pcig = ifelse(nidatob > 0, 1, 0)
         # Make drug consumption indicator
         , pdrug = ifelse(pmax(nidapres,nidaill) > 0, 1, 0)
         # Make alcohol consumption indicator
         , palc = ifelse(nidaalc > 0, 1, 0)
         # Simplify child race 
         , race = case_when(tethnic == 1 ~ 'HispLatino'
                            , twhite == 1 ~ 'White'
                            , taian == 1 ~ 'Other'
                            , tasian == 1 ~ 'Other'
                            , tnhpi == 1 ~ 'Other'
                            , tblack == 1 ~ 'Black'
                            , trace_other == 1 ~ 'Other'
                            , TRUE ~ NA)
         # Simplify parent race 
         , prace = case_when(pethnic == 1 ~ 'HispLatino'
                            , pwhite == 1 ~ 'White'
                            , paian == 1 ~ 'Other'
                            , pasian == 1 ~ 'Other'
                            , pnhpi == 1 ~ 'Other'
                            , pblack == 1 ~ 'Black'
                            , prace_other == 1 ~ 'Other'
                            , TRUE ~ NA)
         # Rectify swan record for ids mentioned
         , swan_inattentive = ifelse(parent_id %in% c(50502,51202,51602,52302
                                                           ,53002,53502,53902,54402
                                                           ,54602,54702)
                                     ,NA
                                     ,swan_inattentive)
         , swan_hyperactive = ifelse(parent_id %in% c(50502,51202,51602,52302
                                                           ,53002,53502,53902,54402
                                                           ,54602,54702)
                                     ,NA
                                     ,swan_hyperactive)
         # Rectify income record
         , income = case_when(income == '' ~ NA
                              , income == '250, 000' ~ 250000
                              , TRUE ~ as.numeric(income))
         # Make higher edu (education after high school) indicator
         , phigheredu = case_when(pedu %in% c(0,1,2) ~ 0
                              , pedu %in% c(3,4,5,6) ~ 1
                              , TRUE ~ NA)
         ) %>%
          # Transform mom smoke indicators into numeric
  mutate(mom_smoke_16wk = as.numeric(as.character(mom_smoke_16wk))
         , mom_smoke_22wk = as.numeric(as.character(mom_smoke_22wk))
         , mom_smoke_32wk = as.numeric(as.character(mom_smoke_32wk))
         , mom_smoke_pp1 = as.numeric(as.character(mom_smoke_pp1))
         , mom_smoke_pp2 = as.numeric(as.character(mom_smoke_pp2))
         , mom_smoke_pp12wk = as.numeric(as.character(mom_smoke_pp12wk))
         , mom_smoke_pp6mo = as.numeric(as.character(mom_smoke_pp6mo))
         # Make indicator whether child use any substance at all (cig/drug/alc) 
         , substance_at_all = case_when(cig == 1 | alc == 1 | drug == 1 ~ 1
                                        , cig == 0 & alc == 0 & drug == 0 ~ 0
                                        , TRUE ~ NA
                                        )
         # Make indicator whether parent use any substance at all (cig/drug/alc)
         , psubstance_at_all = case_when(pcig == 1 | palc == 1 | pdrug == 1 ~ 1
                                        , pcig == 0 & palc == 0 & pdrug == 0 ~ 0
                                        , TRUE ~ NA
                                        )
         # Track how many types of substance used by child (0-3)
         , num_substance_used = rowSums(dplyr::select(., cig:alc), na.rm=TRUE) 
         # Track how many types of substance used by parent (0-3)
         , pnum_substance_used = rowSums(dplyr::select(., pcig:palc), na.rm=TRUE)
         # Total child's parental monitoring score
         , pmq_total = pmq_parental_knowledge + pmq_child_disclosure 
                       + pmq_parental_solicitation + pmq_parental_control
         # Total parent's parental monitoring score
         , ppmq_total = ppmq_parental_knowledge + ppmq_child_disclosure 
                       + ppmq_parental_solicitation + ppmq_parental_control
         # Average of brief problem monitor score (child)
         , bpm_summary = ifelse(is.na(bpm_int)
                                , (bpm_att+bpm_ext)/2
                                , (bpm_att+bpm_ext+bpm_int)/3)
         # Average of brief problem monitor score (parent)
         , pbpm_summary = ifelse(is.na(bpm_att_a)
                                 , (bpm_int_a+bpm_ext_a)/2
                                 , ifelse(is.na(bpm_ext_a)
                                          ,(bpm_int_a+bpm_att_a)/2
                                          ,(bpm_att_a+bpm_ext_a+bpm_int_a)/3))
         # Average of emotion regulation score (child)
         , erq_summary = ifelse(is.na(erq_cog)
                                , erq_exp
                                , ifelse(is.na(erq_exp)
                                         , erq_cog
                                         , (erq_exp+erq_cog)/2))
         # Average of emotion regulation score (child)
         , perq_summary = ifelse(is.na(erq_cog_a)
                                , erq_exp_a
                                , ifelse(is.na(erq_exp_a)
                                         , erq_cog_a
                                         , (erq_exp_a+erq_cog_a)/2))
         # Average of ADHD score (child)
         , swan_summary = (swan_hyperactive + swan_inattentive)/2
         # Age gap between parent and child
         , age_gap = abs(tage - page)
         # Simplify income based on this range (https://money.usnews.com/money/
         #    +personal-finance/family-finance/articles/
         #    +where-do-i-fall-in-the-american-economic-class-system)
         , income = case_when(income < 38133 ~ 1
                              , (income >= 38133 & income <57200) ~ 2
                              , (income >= 57200 & income <114000) ~ 3
                              , income >= 114000 ~ 4
                              , TRUE ~ NA)
         ) %>%
         # Indicator whether mom smoked at all during pregnancy or not
  mutate(mom_prenatal_smoke = case_when((mom_smoke_16wk == 0 & mom_smoke_22wk == 0
                                       & mom_smoke_32wk == 0) ~ 0
                                       , (mom_smoke_16wk == 1 | mom_smoke_22wk == 1
                                       | mom_smoke_32wk == 1)  ~ 1
                                       , TRUE ~ NA)
         # Indicator whether mom smoked at all post pregnancy or not
         , mom_postnatal_smoke = case_when((mom_smoke_pp12wk == 0 & mom_smoke_pp6mo == 0) ~ 0
                                       , (mom_smoke_pp12wk == 1 | mom_smoke_pp6mo == 1) ~ 1
                                       , TRUE ~ NA)
         # Indicator whether mom consistently smoked during pregnancy or not
         #    number get bigger if mom consistently reported smoking every followup
         , mom_prenatal_smoke_consistency = mom_smoke_16wk + mom_smoke_22wk + mom_smoke_32wk   
         # Indicator whether mom consistently smoked post pregnancy or not
         #    number get bigger if mom consistently reported smoking every followup
         , mom_postnatal_smoke_consistency = mom_smoke_pp12wk + mom_smoke_pp6mo 
         # Indicator whether child exposed by smoked at all during pregnancy or not
         , prenatal_exposure = case_when(smoke_exposure_6mo == 0 & smoke_exposure_12mo == 0 ~ 0
                                       , smoke_exposure_6mo == 1 | smoke_exposure_12mo == 1 ~ 1
                                       , TRUE ~ NA
                                       )
         # Indicator whether child exposed by smoked at all post pregnancy or not
         , postnatal_exposure = case_when(smoke_exposure_2yr == 0 & smoke_exposure_3yr == 0
                                       & smoke_exposure_4yr == 0 & smoke_exposure_5yr == 0 ~ 0
                                       , smoke_exposure_2yr == 1 | smoke_exposure_3yr == 1
                                       | smoke_exposure_4yr == 1 | smoke_exposure_5yr == 1 ~ 1
                                       , TRUE ~ NA)
         # Indicator whether child consistently exposed by smoked during pregnancy or not
         #    number get bigger if child consistently exposed every followup
         , prenatal_exposure_consistency =  smoke_exposure_6mo + smoke_exposure_12mo
         # Indicator whether child consistently exposed by smoked post pregnancy or not
         #    number get bigger if child consistently exposed every followup
         , postnatal_exposure_consistency = smoke_exposure_2yr + smoke_exposure_3yr 
                                            + smoke_exposure_4yr + smoke_exposure_5yr   
         # Difference between child & parent parental monitoring score
         , pc_pmq_disharmony = abs(pmq_total - ppmq_total)
         ) %>%
         # impute some missing value in prenatal_exposure if mom_prenatal_smoke == 1
  mutate(prenatal_exposure = ifelse(is.na(prenatal_exposure) & mom_prenatal_smoke == 1
                                    ,1
                                    ,prenatal_exposure)
         # impute some missing value in postnatal_exposure if mom_postnatal_smoke == 1
         , postnatal_exposure = ifelse(is.na(postnatal_exposure) & mom_postnatal_smoke == 1
                                    ,1
                                    ,postnatal_exposure)
         ) %>%
         # Get prenatal & postnatal smoke exposure combination
         #   (e.g., 01 meaning no exposure during pregnancy but exposure after pregnancy)
  mutate(exposure_pattern = case_when(is.na(prenatal_exposure) ~ NA
                                            , is.na(postnatal_exposure) ~ NA
                                            , TRUE ~ paste(prenatal_exposure,postnatal_exposure)
                                            )
         # Get prenatal & postnatal mom smoke combination
         #   (e.g., 01 meaning mom didn't smoke during pregnancy but smoke after pregnancy
         , mom_smoke_pattern = case_when(is.na(mom_prenatal_smoke) ~ NA
                                            , is.na(mom_postnatal_smoke) ~ NA
                                            , TRUE ~ paste(mom_prenatal_smoke,mom_postnatal_smoke)
                                            )
  )



###################
### FILTER DATA ###
###################

# Only take necessary variables for analysis
new_df3 = new_df2 %>%
  dplyr::select(-c(plang:prace_other,childasd:cotimean_34wk,bpm_att_p:smoke_exposure_5yr
                   ,language:trace_other,num_cigs_30,num_e_cigs_30,num_mj_30,num_alc_30))

#############################
### UNIVARARIATE ANALYSIS ###
#############################

categorical_var = c('psex','employ','pedu','income','tsex','cig_ever'
                    ,'e_cig_ever','mj_ever','alc_ever','cig','drug','alc'
                    ,'pcig','pdrug','palc','race','prace','phigheredu'
                    ,'substance_at_all','psubstance_at_all','mom_prenatal_smoke'
                    ,'mom_postnatal_smoke','prenatal_exposure','postnatal_exposure'
                    ,'exposure_pattern','mom_smoke_pattern')

# Make tableone to get the population characteristic
pop_characteristic = CreateTableOne(data = new_df3, factorVars = categorical_var)
pop_characteristic

####################################
### INTERRELATEDNESS BETWEEN EXT ###
####################################

externalizing_behaviors = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp'
                            ,'swan_inattentive','swan_hyperactive'
                            ,'num_substance_used')

ext_behavior = new_df3[,externalizing_behaviors] %>%
  rename('Attention\nProblem' = 'bpm_att'
         ,'Internalizing\nProblem' = 'bpm_int'
         ,'Externalizing\nProblem' = 'bpm_ext'
         ,'Cognitive\nReappraisal' = 'erq_cog'
         ,'Expressive\nSuppression' = 'erq_exp'
         ,'ADHD\nInattentive' = 'swan_inattentive'
         ,'ADHD\nHyperactive' = 'swan_hyperactive'
         ,'Substance\nVariety' = 'num_substance_used')

# Make pair plot for all EXT variables
ggpairs(ext_behavior
        ,lower = list(continuous=wrap("smooth", colour="grey"))
        ,diag = list(continuous = wrap("densityDiag", colour = "blue"))
        ,upper = list(continuous = wrap("cor", size = 3))
        ,progress = NULL) + 
  theme_bw() + 
  labs(title = 'Pair Plot of EXT Variables') +
  theme(legend.position = 'bottom'
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5)
        , text=element_text(size=5))

############################################################
### COMPARE EXT CHARACTERISTICS BETWEEN DIFFERENT GROUPS ###
############################################################

# Tried using tsex, employ, phigheredu, and income but nothing significant

# Mom Prenatal Smoke
# Filter dataset
included = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
             ,'swan_hyperactive','num_substance_used','mom_prenatal_smoke')
prenatalComparison = new_df3[,included]

# Stratify summary by group
prenatalComparison %>%
  tbl_summary(by = mom_prenatal_smoke
              , type = list(c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
                              ,'swan_hyperactive','num_substance_used') ~ 'continuous')
              , statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
              , missing = 'no'
              , label = list(
                'bpm_att' = 'Attention Problem'
                ,'bpm_int' = 'Internalizing Problem'
                ,'bpm_ext' = 'Externalizing Problem'
                ,'erq_cog' = 'Cognitive Reappraisal'
                ,'erq_exp' = 'Expressive Suppression' 
                ,'swan_inattentive' = 'ADHD Inattentive'
                ,'swan_hyperactive' = 'ADHD Hyperactive'
                ,'num_substance_used' = 'Substance Variety'
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Maternal Prenatal Smoking**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

# Mom Postnatal Smoke
# Filter dataset
included = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
             ,'swan_hyperactive','num_substance_used','mom_postnatal_smoke')
prenatalComparison = new_df3[,included]

# Stratify summary by group
prenatalComparison %>%
  tbl_summary(by = mom_postnatal_smoke
              , type = list(c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
                              ,'swan_hyperactive','num_substance_used') ~ 'continuous')
              , statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
              , missing = 'no'
              , label = list(
                'bpm_att' = 'Attention Problem'
                ,'bpm_int' = 'Internalizing Problem'
                ,'bpm_ext' = 'Externalizing Problem'
                ,'erq_cog' = 'Cognitive Reappraisal'
                ,'erq_exp' = 'Expressive Suppression' 
                ,'swan_inattentive' = 'ADHD Inattentive'
                ,'swan_hyperactive' = 'ADHD Hyperactive'
                ,'num_substance_used' = 'Substance Variety'
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Maternal Postnatal Smoking**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

# Prenatal Exposure
# Filter dataset
included = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
             ,'swan_hyperactive','num_substance_used','prenatal_exposure')
prenatalComparison = new_df3[,included]

# Stratify summary by group
prenatalComparison %>%
  tbl_summary(by = prenatal_exposure
              , type = list(c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
                              ,'swan_hyperactive','num_substance_used') ~ 'continuous')
              , statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
              , missing = 'no'
              , label = list(
                'bpm_att' = 'Attention Problem'
                ,'bpm_int' = 'Internalizing Problem'
                ,'bpm_ext' = 'Externalizing Problem'
                ,'erq_cog' = 'Cognitive Reappraisal'
                ,'erq_exp' = 'Expressive Suppression' 
                ,'swan_inattentive' = 'ADHD Inattentive'
                ,'swan_hyperactive' = 'ADHD Hyperactive'
                ,'num_substance_used' = 'Substance Variety'
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Prenatal Environmental Smoking Exposure**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

# Postnatal Exposure
# Filter dataset
included = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
             ,'swan_hyperactive','num_substance_used','postnatal_exposure')
# prenatalComparison = new_df3[,c(9:10,26:30,46,56)]
prenatalComparison = new_df3[,included]

# Stratify summary by group
prenatalComparison %>%
  tbl_summary(by = postnatal_exposure
              , type = list(c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
                              ,'swan_hyperactive','num_substance_used') ~ 'continuous')
              , statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
              , missing = 'no'
              , label = list(
                'bpm_att' = 'Attention Problem'
                ,'bpm_int' = 'Internalizing Problem'
                ,'bpm_ext' = 'Externalizing Problem'
                ,'erq_cog' = 'Cognitive Reappraisal'
                ,'erq_exp' = 'Expressive Suppression' 
                ,'swan_inattentive' = 'ADHD Inattentive'
                ,'swan_hyperactive' = 'ADHD Hyperactive'
                ,'num_substance_used' = 'Substance Variety'
              )) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Postnatal Environmental Smoking Exposure**") %>%
  bold_labels() %>%
  as_kable_extra(booktabs = TRUE) %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'scale_down') 

#########################################################
### CORRELATION OF EXT WITH OTHER CONTINUOUS VARIABLE ###
#########################################################

# Variables to be compared
EXT = c('bpm_att','bpm_int','bpm_ext','erq_cog','erq_exp','swan_inattentive'
        ,'swan_hyperactive','num_substance_used')
non_EXT = c('bpm_att_a','bpm_int_a','bpm_ext_a','erq_cog_a','erq_exp_a'
            ,'pnum_substance_used','mom_prenatal_smoke_consistency'
            ,'mom_postnatal_smoke_consistency','prenatal_exposure_consistency'
            ,'postnatal_exposure_consistency','pmq_parental_knowledge'
            ,'pmq_child_disclosure','pmq_parental_solicitation'
            ,'pmq_parental_control','tage','age_gap')

# Make correlation matrix
cor_mat = cor(new_df3[,c(EXT,non_EXT)]
              , use="pairwise.complete.obs")

# Remove diagonal, redundant values and rename the values
cor_mat[!lower.tri(cor_mat)] = NA 
cor_df = data.frame(cor_mat) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.35)
# Only take non EXT and EXT pair with abs(correlation) > 0.35
cor_df %>%
  rename('variable1' = 'rowname', 'variable2' = 'variable') %>%
  filter(!(variable1 %in% EXT & variable2 %in% EXT)
         , !(variable1 %in% non_EXT & variable2 %in% non_EXT)) %>%
  mutate(variable2 = case_when(variable2=='bpm_att' ~ 'Attention Problem'
                               ,variable2=='bpm_int' ~ 'Internalizing Problem'
                               ,variable2=='bpm_ext' ~ 'Externalizing Problem'
                               ,variable2=='erq_cog' ~ 'Cognitive Reappraisal'
                               ,variable2=='erq_exp' ~ 'Expressive Suppression'
                               ,variable2=='swan_inattentive' ~ 'ADHD Inattentive'
                               ,variable2=='swan_hyperactive' ~ 'ADHD Hyperactive'
                               ,variable2=='num_substance_used' ~ 'Substance Variety')
         ,variable1 = case_when(variable1=='bpm_att_a' ~ 'Parent Attention Problem'
                                ,variable1=='bpm_int_a' ~ 'Parent Internalizing Problem'
                                ,variable1=='bpm_ext_a' ~ 'Parent Externalizing Problem'
                                ,variable1=='erq_cog_a' ~ 'Parent Cognitive Reappraisal'
                                ,variable1=='mom_prenatal_smoke_consistency' 
                                ~ 'Maternal Prenatal Smoking Consistency'
                                ,variable1=='mom_postnatal_smoke_consistency' 
                                ~ 'Maternal Postnatal Smoking Consistency'
                                ,variable1=='prenatal_exposure_consistency' 
                                ~ 'Prenatal Environmental Smoking Exposure Consistency'
                                ,variable1=='postnatal_exposure_consistency' 
                                ~ 'Postnatal Environmental Smoking Exposure Consistency'
                                ,variable1=='pnum_substance_used' ~ 'Parental Substance Variety'
                                ,variable1=='pmq_parental_knowledge' ~ 'Parental Knowledge'
                                ,variable1=='pmq_child_disclosure' ~ 'Child Disclosure'
                                ,variable1=='pmq_parental_solicitation' ~ 'Parental Solicitation'
                                ,variable1=='pmq_parental_control' ~ 'Parental Control'
                                ,variable1=='age_gap' ~ 'Parent Child Age Gap')) %>%
  arrange(desc(variable2)) %>%
  rename('Other Variables' = 'variable1'
         , 'Externalizing Behavior Variables' = 'variable2'
         , 'Correlation' = 'correlation') %>%
  mutate(Correlation = round(Correlation, 2)) %>%
  kableExtra::kbl(caption = 'Correlation Between EXT & Non EXT Variables'
                  , booktabs = T
                  , escape = T
                  , align = 'c') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

#############################################################
### PLOT BETWEEN EXT, SDP AND OTHER POTENTIAL CONFOUNDER ###
############################################################

# Make plot of all EXT variables with SDP/Other potential confounder found previously

### SWAN INATTENTIVE ###
swan_inattentive_p1 = ggplot(subset(new_df3, !is.na(mom_postnatal_smoke))) +
  geom_boxplot(aes(x=as.factor(mom_postnatal_smoke), y=swan_inattentive)
               , alpha = 0.5, fill = 'firebrick', color = 'firebrick') + 
  theme_bw() + 
  labs(x = 'Mom Postnatal Smoking'
       ,y = 'ADHD Inattentive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_inattentive_p1

swan_inattentive_p2 = ggplot(subset(new_df3, !is.na(mom_postnatal_smoke))) +
  geom_jitter(aes(x=bpm_att_a, y=swan_inattentive), color = 'grey') +
  geom_smooth(aes(x=bpm_att_a, y=swan_inattentive), method = 'lm'
              , alpha = 0.5, fill = 'firebrick', color = 'firebrick') + 
  facet_grid(mom_postnatal_smoke~.
             , labeller = as_labeller(c(`0`='Mom not smoking postnatal'
                                        ,`1`='Mom smoking postnatal'))) +
  theme_bw() + 
  labs(x = 'Parent Attention Problem Score'
       ,y = 'ADHD Inattentive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_inattentive_p2

swan_inattentive_p3 = ggplot(subset(new_df3, !is.na(mom_postnatal_smoke))) +
  geom_jitter(aes(x=bpm_int_a, y=swan_inattentive), color = 'grey') +
  geom_smooth(aes(x=bpm_int_a, y=swan_inattentive), method = 'lm'
              , alpha = 0.5, fill = 'firebrick', color = 'firebrick') + 
  facet_grid(mom_postnatal_smoke~.
             , labeller = as_labeller(c(`0`='Mom not smoking (Post)'
                                        ,`1`='Mom smoking (Post)'))) +
  theme_bw() + 
  labs(x = 'Parent Internalizing Problem Score'
       ,y = 'ADHD Inattentive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_inattentive_p3

### SWAN HYPERACTIVE ###
swan_hyperactive_p1 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_boxplot(aes(x=as.factor(mom_prenatal_smoke), y=swan_hyperactive)
               , alpha = 0.5, fill = 'salmon2', color = 'salmon2') + 
  theme_bw() + 
  labs(x = 'Mom Prenatal Smoking'
       ,y = 'ADHD Hyperactive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_hyperactive_p1

swan_hyperactive_p2 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_jitter(aes(x=mom_prenatal_smoke_consistency, y=swan_hyperactive), color = 'grey') +
  geom_smooth(aes(x=mom_prenatal_smoke_consistency, y=swan_hyperactive), method = 'lm'
              , alpha = 0.5, fill = 'salmon2', color = 'salmon2') + 
  theme_bw() + 
  labs(x = 'Mom Prenatal Smoking Consistency'
       ,y = 'ADHD Hyperactive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_hyperactive_p2

swan_hyperactive_p3 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_jitter(aes(x=bpm_att_a, y=swan_hyperactive), color = 'grey') +
  geom_smooth(aes(x=bpm_att_a, y=swan_hyperactive), method = 'lm'
              , alpha = 0.5, fill = 'salmon2', color = 'salmon2') + 
  facet_grid(mom_prenatal_smoke~.
             , labeller = as_labeller(c(`0`='Mom not smoking (Pre)'
                                        ,`1`='Mom smoking (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Attention Problem Score'
       ,y = 'ADHD Hyperactive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_hyperactive_p3

swan_hyperactive_p4 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_jitter(aes(x=bpm_int_a, y=swan_hyperactive), color = 'grey') +
  geom_smooth(aes(x=bpm_int_a, y=swan_hyperactive), method = 'lm'
              , alpha = 0.5, fill = 'salmon2', color = 'salmon2') + 
  facet_grid(mom_prenatal_smoke~.
             , labeller = as_labeller(c(`0`='Mom not smoking (Pre)'
                                        ,`1`='Mom smoking (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Internalizing Problem Score'
       ,y = 'ADHD Hyperactive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_hyperactive_p4

swan_hyperactive_p5 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_jitter(aes(x=erq_exp_a, y=swan_hyperactive), color = 'grey') +
  geom_smooth(aes(x=erq_exp_a, y=swan_hyperactive), method = 'lm'
              , alpha = 0.5, fill = 'salmon2', color = 'salmon2') + 
  facet_grid(mom_prenatal_smoke~.
             , labeller = as_labeller(c(`0`='Mom not smoking (Pre)'
                                        ,`1`='Mom smoking (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Cognitive Reappraisal Score'
       ,y = 'ADHD Hyperactive Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# swan_hyperactive_p5

### CHILD ATTENTION PROBLEM SCORE ###
bpm_att_p1 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_boxplot(aes(x=as.factor(prenatal_exposure), y=bpm_att)
               , alpha = 0.5, fill = 'lightgoldenrod2', color = 'lightgoldenrod2') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure'
       ,y = 'Child Attention Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_att_p1

bpm_att_p2 = ggplot(subset(new_df3, !is.na(race))) +
  geom_boxplot(aes(x=as.factor(race), y=bpm_att)
               , alpha = 0.5, fill = 'lightgoldenrod2', color = 'lightgoldenrod2') + 
  theme_bw() + 
  labs(x = 'Race'
       ,y = 'Child Attention Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_att_p2

bpm_att_p3 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_jitter(aes(x=bpm_att_a, y=bpm_att), color = 'grey') +
  geom_smooth(aes(x=bpm_att_a, y=bpm_att), method = 'lm'
              , alpha = 0.5, fill = 'lightgoldenrod2', color = 'lightgoldenrod2') + 
  facet_grid(prenatal_exposure~.
             , labeller = as_labeller(c(`0`='Not Smoke Exposed (Pre)'
                                        ,`1`='Smoke Exposed (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Attention Problem Score'
       ,y = 'Child Attention Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_att_p3

bpm_att_p4 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_jitter(aes(x=bpm_int_a, y=bpm_att), color = 'grey') +
  geom_smooth(aes(x=bpm_int_a, y=bpm_att), method = 'lm'
              , alpha = 0.5, fill = 'lightgoldenrod2', color = 'lightgoldenrod2') + 
  facet_grid(prenatal_exposure~.
             , labeller = as_labeller(c(`0`='Not Smoke Exposed (Pre)'
                                        ,`1`='Smoke Exposed (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Internalizing Problem Score'
       ,y = 'Child Attention Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_att_p4

### CHILD INTERNALIZING PROBLEM SCORE ###
bpm_int_p1 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_boxplot(aes(x=as.factor(prenatal_exposure), y=bpm_int)
               , alpha = 0.5, fill = 'springgreen3', color = 'springgreen3') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure'
       ,y = 'Child Internalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_int_p1

bpm_int_p2 = ggplot(subset(new_df3, !is.na(race))) +
  geom_boxplot(aes(x=as.factor(race), y=bpm_int)
               , alpha = 0.5, fill = 'springgreen3', color = 'springgreen3') + 
  theme_bw() + 
  labs(x = 'Race'
       ,y = 'Child Internalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_int_p2

bpm_int_p3 = ggplot(subset(new_df3, !is.na(employ))) +
  geom_boxplot(aes(x=as.factor(employ), y=bpm_int)
               , alpha = 0.5, fill = 'springgreen3', color = 'springgreen3') + 
  scale_x_discrete(labels=c('No', 'Part-Time', 'Full-Time')) +
  theme_bw() + 
  labs(x = 'Parent Employment Status'
       ,y = 'Child Internalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_int_p3

bpm_int_p4 = ggplot(new_df3) +
  geom_jitter(aes(x=prenatal_exposure_consistency, y=bpm_int), color = 'grey') +
  geom_smooth(aes(x=prenatal_exposure_consistency, y=bpm_int), method = 'lm'
              , alpha = 0.5, fill = 'springgreen3', color = 'springgreen3') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure Consistency'
       ,y = 'Child Internalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_int_p4

### CHILD EXTERNALIZING PROBLEM SCORE ###
bpm_ext_p1 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_boxplot(aes(x=as.factor(prenatal_exposure), y=bpm_ext)
               , alpha = 0.5, fill = 'skyblue3', color = 'skyblue3') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure'
       ,y = 'Child Externalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_ext_p1

bpm_ext_p2 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_jitter(aes(x=bpm_att_a, y=bpm_ext), color = 'grey') +
  geom_smooth(aes(x=bpm_att_a, y=bpm_ext), method = 'lm'
              , alpha = 0.5, fill = 'skyblue3', color = 'skyblue3') + 
  facet_grid(prenatal_exposure~.
             , labeller = as_labeller(c(`0`='Not Smoke Exposed (Pre)'
                                        ,`1`='Smoke Exposed (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Attention Problem Score'
       ,y = 'Child Externalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_ext_p2

bpm_ext_p3 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_jitter(aes(x=bpm_int_a, y=bpm_ext), color = 'grey') +
  geom_smooth(aes(x=bpm_int_a, y=bpm_ext), method = 'lm'
              , alpha = 0.5, fill = 'skyblue3', color = 'skyblue3') + 
  facet_grid(prenatal_exposure~.
             , labeller = as_labeller(c(`0`='Not Smoke Exposed (Pre)'
                                        ,`1`='Smoke Exposed (Pre)'))) +
  theme_bw() + 
  labs(x = 'Parent Internalizing Problem Score'
       ,y = 'Child Externalizing Problem Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# bpm_ext_p3

### CHILD EXPRESSIVE SUPRESSION ###
erq_exp_p1 = ggplot(subset(new_df3, !is.na(mom_prenatal_smoke))) +
  geom_boxplot(aes(x=as.factor(mom_prenatal_smoke), y=bpm_ext)
               , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Prenatal Mom Smoking'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p1

erq_exp_p2 = ggplot(subset(new_df3, !is.na(prenatal_exposure))) +
  geom_boxplot(aes(x=as.factor(prenatal_exposure), y=bpm_ext)
               , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p2

erq_exp_p3 = ggplot(subset(new_df3, !is.na(postnatal_exposure))) +
  geom_boxplot(aes(x=as.factor(postnatal_exposure), y=bpm_ext)
               , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Postnatal Smoke Exposure'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p3

erq_exp_p4 = ggplot(subset(new_df3, !is.na(prenatal_exposure_consistency))) +
  geom_jitter(aes(x=prenatal_exposure_consistency, y=bpm_ext), color = 'grey') +
  geom_smooth(aes(x=prenatal_exposure_consistency, y=bpm_ext), method = 'lm'
              , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Prenatal Smoke Exposure Consistency'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p4

erq_exp_p5 = ggplot(subset(new_df3, !is.na(postnatal_exposure_consistency))) +
  geom_jitter(aes(x=postnatal_exposure_consistency, y=bpm_ext), color = 'grey') +
  geom_smooth(aes(x=postnatal_exposure_consistency, y=bpm_ext), method = 'lm'
              , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Postnatal Smoke Exposure Consistency'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p5

erq_exp_p6 = ggplot(subset(new_df3, !is.na(pnum_substance_used))) +
  geom_jitter(aes(x=pnum_substance_used, y=bpm_ext), color = 'grey') +
  geom_smooth(aes(x=pnum_substance_used, y=bpm_ext), method = 'lm'
              , alpha = 0.5, fill = 'hotpink2', color = 'hotpink2') + 
  theme_bw() + 
  labs(x = 'Parent Substance Types Used'
       ,y = 'Child Expressive Supression Score'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# erq_exp_p6

### CHILD SUBSTANCE TYPES USED ###
subs_used_p1 = ggplot(subset(new_df3, !is.na(mom_postnatal_smoke_consistency))) +
  geom_jitter(aes(x=mom_postnatal_smoke_consistency, y=num_substance_used), color = 'grey') +
  geom_smooth(aes(x=mom_postnatal_smoke_consistency, y=num_substance_used), method = 'lm'
              , alpha = 0.5, fill = 'mediumpurple4', color = 'mediumpurple4') + 
  theme_bw() + 
  labs(x = 'Mom Postnatal Smoke Consistency'
       ,y = 'Child Substance Types Used'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# subs_used_p1

subs_used_p2 = ggplot(subset(new_df3, !is.na(pmq_parental_knowledge))) +
  geom_jitter(aes(x=pmq_parental_knowledge, y=num_substance_used), color = 'grey') +
  geom_smooth(aes(x=pmq_parental_knowledge, y=num_substance_used), method = 'lm'
              , alpha = 0.5, fill = 'mediumpurple4', color = 'mediumpurple4') + 
  theme_bw() + 
  labs(x = 'Parental Knowledge Score (Child POV)'
       ,y = 'Child Substance Types Used'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# subs_used_p2

subs_used_p3 = ggplot(subset(new_df3, !is.na(pmq_child_disclosure))) +
  geom_jitter(aes(x=pmq_child_disclosure, y=num_substance_used), color = 'grey') +
  geom_smooth(aes(x=pmq_child_disclosure, y=num_substance_used), method = 'lm'
              , alpha = 0.5, fill = 'mediumpurple4', color = 'mediumpurple4') + 
  theme_bw() + 
  labs(x = 'Child Disclosure Score (Child POV)'
       ,y = 'Child Substance Types Used'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# subs_used_p3

subs_used_p4 = ggplot(subset(new_df3, !is.na(pmq_parental_control))) +
  geom_jitter(aes(x=pmq_parental_control, y=num_substance_used), color = 'grey') +
  geom_smooth(aes(x=pmq_parental_control, y=num_substance_used), method = 'lm'
              , alpha = 0.5, fill = 'mediumpurple4', color = 'mediumpurple4') +
  theme_bw() + 
  labs(x = 'Parental Control Score (Child POV)'
       ,y = 'Child Substance Types Used'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# subs_used_p4

subs_used_p5 = ggplot(subset(new_df3, !is.na(bpm_att_a))) +
  geom_jitter(aes(x=bpm_att_a, y=num_substance_used), color = 'grey') +
  geom_smooth(aes(x=bpm_att_a, y=num_substance_used), method = 'lm'
              , alpha = 0.5, fill = 'mediumpurple4', color = 'mediumpurple4') +
  theme_bw() + 
  labs(x = 'Parent Attention Problem Score'
       ,y = 'Child Substance Types Used'
       ,) +
  theme(legend.position = 'bottom'
        ,text = element_text(size=7)
        , plot.title = element_text(hjust = 0.5)
        , plot.caption = element_text(hjust = 0.5))
# subs_used_p5

# Combine EXT variable plots
patchwork1 = (swan_inattentive_p1 + swan_inattentive_p2 + swan_inattentive_p3)
patchwork2 = (swan_hyperactive_p1 + swan_hyperactive_p2 + swan_hyperactive_p3 
              + swan_hyperactive_p4 + swan_hyperactive_p5)
patchwork3 = (bpm_att_p1 + bpm_att_p2 + bpm_att_p3 + bpm_att_p4)
patchwork4 = (bpm_int_p1 + bpm_int_p2 + bpm_int_p3 + bpm_int_p4)
patchwork5 = (bpm_ext_p1 + bpm_ext_p2 + bpm_ext_p3)
patchwork6 = (erq_exp_p1 + erq_exp_p2 + erq_exp_p3 + erq_exp_p4 
              +erq_exp_p5 + erq_exp_p6)
patchwork7 = (subs_used_p1 + subs_used_p2 + subs_used_p3 + subs_used_p4
              +subs_used_p5)

patchwork1 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child ADHD Inattentive Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

patchwork2 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child ADHD Hyperactive Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10)))

patchwork3 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child Attention Problem Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

patchwork4 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child Internalizing Problem Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

patchwork5 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child Externalizing Supression Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

patchwork6 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child Expressive Supression Score vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

patchwork7 + 
  plot_annotation(tag_levels = 'A'
                  ,title = 'Child Substance Types Used vs SDP & Other Potential Confounders',
                  theme = theme(plot.title = element_text(size = 12)
                                ,plot.tag = element_text(size = 10))) 

########################################
### REGRESSION FOR ALL EXT VARIABLES ###
########################################

# Make dataset for regression purpose
new_df4 = new_df3 %>%
  mutate(mom_postnatal_smoke = as.factor(mom_postnatal_smoke)
         ,mom_prenatal_smoke = as.factor(mom_prenatal_smoke)
         ,race = as.factor(race)
         ,employ = as.factor(employ)
         ,prenatal_exposure = as.factor(prenatal_exposure)
         ,postnatal_exposure = as.factor(postnatal_exposure)
  )

### ADHD Inattentive
m1.2 = lm(swan_inattentive ~ bpm_att_a, data = new_df4)
summary(m1.2) # pval = 0.0281
m1.3 = lm(swan_inattentive ~ bpm_int_a, data = new_df4)
summary(m1.3) # pval = 0.0192 
m1.3 = lm(swan_inattentive ~ pmq_child_disclosure, data = new_df4)
summary(m1.3) # pval = 0.0161
m1.4 = lm(swan_inattentive ~ pmq_child_disclosure + bpm_int_a
          , data = new_df4)
summary(m1.4) # pval = not statsig
m1.5 = lm(swan_inattentive ~ pmq_child_disclosure + bpm_int_a
          , data = new_df4)
summary(m1.5) # pval = pmq_child_disclosure 0.0366

### ADHD Hyperactive
m2.1 = lm(swan_hyperactive ~ mom_prenatal_smoke ,data = new_df4)
summary(m2.1) # pval = 0.022349
m2.2 = lm(swan_hyperactive ~ bpm_att_a ,data = new_df4)
summary(m2.2) # pval = 0.0217
m2.3 = lm(swan_hyperactive ~ bpm_int_a ,data = new_df4)
summary(m2.3) # pval = 0.000458
m2.4 = lm(swan_hyperactive ~ mom_prenatal_smoke + bpm_att_a + bpm_int_a
          ,data = new_df4)
summary(m2.4) #best statsig: bpm_int_a 0.0155

### BPM Att
m3.1 = lm(bpm_att ~ bpm_att_a, data = new_df4)
summary(m3.1) # pval = 0.000170
m3.2 = lm(bpm_att ~ bpm_int_a, data = new_df4)
summary(m3.2) # pval = 0.027010
m3.3 = lm(bpm_att ~ bpm_att_a + bpm_int_a
          ,data = new_df4)
summary(m3.3) #best statsig: bpm_att_a .003985

### BPM Int
m4.1 = lm(bpm_int ~ prenatal_exposure, data = new_df4)
summary(m4.1) # pval = 0.020687
m4.2 = lm(bpm_int ~ prenatal_exposure_consistency, data = new_df4)
summary(m4.2) # pval 11 = 0.041796

### BPM Ext
m5.1 = lm(bpm_ext ~ prenatal_exposure, data = new_df4)
summary(m5.1) # pval = 0.0285
m5.2 = lm(bpm_ext ~ bpm_att_a, data = new_df4)
summary(m5.2) # pval = 0.000545
m5.3 = lm(bpm_ext ~ bpm_int_a, data = new_df4)
summary(m5.3) # pval = 0.0222
m5.4 = lm(bpm_ext ~ prenatal_exposure + bpm_att_a + bpm_int_a
          ,data = new_df4)
summary(m5.4) #best statsig: bpm_att_a 0.021966

### ERQ Exp
m6.1 = lm(erq_exp ~ postnatal_exposure, data = new_df4)
summary(m6.1) # pval = 0.00604
m6.2 = lm(erq_exp ~ prenatal_exposure_consistency, data = new_df4)
summary(m6.2) # pval = 0.022
m6.3 = lm(erq_exp ~ postnatal_exposure_consistency, data = new_df4)
summary(m6.3) # pval = 0.00104
m6.4 = lm(erq_exp ~  prenatal_exposure_consistency + postnatal_exposure_consistency 
          ,data = new_df4)
summary(m6.4) #best statsig: postnatal_exposure_consistency 0.0191

### Substance Types Used
m7.1 = lm(num_substance_used ~ mom_postnatal_smoke_consistency, data = new_df4)
summary(m7.1) # pval = 0.0193
m7.2 = lm(num_substance_used ~ pmq_parental_knowledge, data = new_df4)
summary(m7.2) # pval = 0.000531
m7.3 = lm(num_substance_used ~ pmq_child_disclosure, data = new_df4)
summary(m7.3) # pval = 0.01438
m7.4 = lm(num_substance_used ~ pmq_parental_control, data = new_df4)
summary(m7.4) # pval = 0.0327
m7.5 = lm(num_substance_used ~ bpm_att_a, data = new_df4)
summary(m7.5) # pval = 0.00512
m7.6 = lm(num_substance_used ~  bpm_att_a + mom_postnatal_smoke_consistency 
          + pmq_parental_knowledge + pmq_parental_control
          ,data = new_df4)
summary(m7.6) #best statsig: bpm_att_a 0.00815, pmq 0.00943
