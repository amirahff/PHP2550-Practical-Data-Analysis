# Predictive Modeling for Tracheostomy Outcomes in Bronchopulmonary Dysplasia

## Introduction

Bronchopulmonary Dysplasia (BPD) is a complication associated with prematurity, impacting a significant number of infants each year, particularly in its severe form, affecting 10,000-15,000 newborns annually. Various factors, including genetics and epigenetics, influence the development of BPD. This condition manifests as a persistent lung ailment, primarily afflicting prematurely born infants, necessitating oxygen therapy for their respiratory support. In BPD, there is notable damage to the lungs and airways (bronchi), leading to tissue damage (dysplasia) in the small air sacs of the lungs (alveoli). The severity of BPD is categorized into different grades, with Grade 3 BPD marking a critical point where reliance on a ventilator is necessary at 36 weeks corrected gestational age. Notably, 75% of infants with Grade 3 BPD continue to require ventilator support upon discharge, while 25% do not. For those who need ventilator support upon discharge, a tracheostomy involving a surgical opening in the neck facilitating connection to a ventilator becomes a prerequisite. The incidence of tracheostomy in infants with BPD ranges from 2-4%, escalating to 12% in cases of severe or Grade 3 BPD.

While the advantages of performing a tracheostomy include ensuring a stable airway, improving ventilator synchronization, and fostering growth, it is essential to acknowledge the associated risks. These risks include an increased likelihood of mortality compared to cases without tracheostomy, the potential for accidental decannulation leading to fatal outcomes, cannula obstruction with similar dire consequences, elevated rates of infection affecting the skin, trachea, and lungs, and the development of tracheal stenosis.

Given these considerations, cautious decision-making is crucial when contemplating the implementation of tracheostomy in infants diagnosed with BPD. Consequently, this study aims to develop statistical models utilizing clinical data collected at 36 and 44 weeks post-menstrual age (PMA). These models aim to predict the eventual necessity for tracheostomy or the likelihood of mortality preceding discharge, providing a valuable framework for informed decision-making in the management of BPD in newborns.

## Data

Participants in this study were sourced from the BPD Collaborative Registry, a collaborative network of BPD programs in the United States and Sweden. The consortium was established to bridge evidence gaps and advance research for improving care in children affected by severe bronchopulmonary dysplasia (BPD). The registry focuses on infants born with a gestational age of less than 32 weeks and diagnosed with sBPD, defined according to the 2001 NHLBI criteria, specifically requiring FiO2 ≥ 0.3 or positive pressure ventilation (invasive or non-invasive) at 36 weeks post-menstrual age (PMA). Standard demographic and clinical data are routinely collected at four key time points: birth, 36 weeks PMA, 44 weeks PMA, and discharge. For this study, we extracted data from the registry for patients with BPD and complete growth information, covering the period from January 1 to July 19, 2021. At the time of analysis, 10 BPD Collaborative centers had contributed data that meets the study inclusion criteria

The dataset is structured around individual record_id entries representing premature infants. Key characteristics at birth include gender, corrected gestational age, originating center, birth measurements (weight, length, head circumference), and maternal characteristics such as race and ethnicity. Additional birth-related information, including delivery method, Prenatal Corticosteroids administration, Maternal Chorioamnionitis presence, and surfactant administration within the first 72 hours, is also captured.

Data at 36 and 44 weeks includes information on baby weight, Level of support, PEEP (Positive End-Expiratory and Pressure), Fraction of inspired O2, Peak inspiratory pressure, and medication administration for Pulmonary Hypertension. This dataset's primary outcomes of interest are whether infants underwent a tracheostomy at discharge and their mortality status.

The full code for this project can be found in the code folder and the explanation of the variables can be seen in the codebook folder.

## Software and Packages

Version of software and packages used in the analysis:  
R version 4.2.2 (2022-10-31)    
lme4_1.1-30  
pROC_1.18.0  
glmnet_4.1-8  
Matrix_1.5-1  
mice_3.15.0  
splitstackshape_1.4.8 patchwork_1.1.2  
corrplot_0.92  
GGally_2.1.2  
kableExtra_1.3.4  
gtsummary_1.7.2  
gt_0.9.0  
naniar_1.0.0  
lubridate_1.9.3  
forcats_1.0.0   
stringr_1.5.0  
dplyr_1.1.3  
purrr_1.0.1  
readr_2.1.4  
tidyr_1.3.0  
tibble_3.2.1  
ggplot2_3.4.3  
tidyverse_2.0.0  
formatR_1.14  

