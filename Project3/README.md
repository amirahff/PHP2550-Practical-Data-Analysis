# Assessing the Trasnportability of Cardiovascular Risk Prediction Models from the Framingham Heart Study to Target Population

## Introduction

Predictive models are typically designed to generate predictions for a specific demographic. For instance, a healthcare system may develop a risk prediction model to identify patients at high risk for cardiovascular events among its care recipients. The data informing the development of such models, known as source study data, often come from controlled experiments, extensive observational databases, or longitudinal studies. However, these datasets usually do not represent a random sampling from the intended demographic, resulting in a discrepancy between the target population and the one represented in the source study data. Take, for example, the widely used Framingham ATP-III model, which predicts the ten-year risk of cardiovascular events; it was developed using data primarily from white subjects, which may lead to suboptimal predictions for ethnically diverse populations.

Recently, a variety of techniques have been developed to assess the effectiveness of predictive models within a specified target population (or to adapt model performance metrics from the source to the target population). Our aim is to apply these methods to a risk score model created with data from the Framingham Heart Study, and then to evaluate the model's performance using a simulated study population drawn from the NHANES (National Health and Nutrition Examination Survey) data. We seek to understand how well a prediction model performs in a target population that differs from the one originally used for the model's development and/or evaluation.

## Data

In this study, we reference two datasets: 1) The Framingham Heart Study, and 2) The National Health and Nutrition Examination Survey Data (NHANES).

The Framingham Heart Study is a long-term, prospective investigation into the causes of cardiovascular disease within a cohort of free-living individuals in Framingham, Massachusetts. This dataset includes clinical examination data such as cardiovascular disease risk factors and indicators like blood pressure. Additionally, it documents whether the participants have experienced cardiovascular events, including myocardial infarction (hospitalized and silent or unrecognized), fatal coronary heart disease, atherothrombotic infarction, cerebral embolism, intracerebral hemorrhage, subarachnoid hemorrhage, or fatal cerebrovascular disease. We regard this dataset as the source population, which we use to construct a model to identify individuals at high risk for cardiovascular events.

Conversely, NHANES data represents a nationally representative sample of adults and children in the United States. Data collection encompasses detailed, face-to-face interviews, physical and physiological examinations, and laboratory tests, some of which overlap with data from the Framingham Heart Study. However, NHANES does not include long-term outcome information such as cardiovascular disease events. We consider this dataset the target population and aim to determine how well a model built from the source population performs with this data.

The common variables extracted from both datasets include: 1) TOTCHOL, serum total cholesterol (mg/dL); 2) SYSBP, systolic blood pressure; 3) AGE, age at examination; 4) HDLC, high-density lipoprotein cholesterol (mg/dL); 5) SEX, participant sex; 6) CURSMOKE, current cigarette smoking at examination; 7) DIABETES, diabetic status; 8) BPMEDS, use of anti-hypertensive medication at examination.

In terms of preprocessing, we generate two new variables derived from SYSBP and BPMEDS. The new variables are SYSBP_T, representing the systolic blood pressure for participants using anti-hypertensive medication. For participants on such medication, we retain the SYSBP value; otherwise, we assign a 0 score. The second variable, SYSBP_UT, represents the systolic blood pressure for participants not using anti-hypertensive medication. Here, if the participant does not take the medication, we use the SYSBP value; if they do, we assign a 0 score.

The full code for this project can be found in the code folder.

Version of software and packages used in the analysis:
R version 4.2.2 (2022-10-31) 
patchwork_1.1.2
kableExtra_1.3.4      
nhanesA_0.7.4
truncnorm_1.0-9
MASS_7.3-60
tableone_0.13.2       
dplyr_1.1.3   
tidyr_1.3.0
tidyverse_2.0.0
tibble_3.2.1  
ggplot2_3.4.3
riskCommunicator_1.0.1 formatR_1.14 
simMetric_0.1.1
EnvStats_2.8.1 
lubridate_1.9.3
forcats_1.0.0
stringr_1.5.0
gtsummary_1.7.2
gt_0.9.0
purrr_1.0.1
readr_2.1.4

