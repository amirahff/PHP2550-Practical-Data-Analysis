## Exploratory Analysis of Direct and Indirect Prenatal Smoking Exposure Effects on Children Externalizing Behavior

## Introduction 

The primary research goal of this report is to delve into the effects of smoking during pregnancy (SDP) and exposure to environmental tobacco smoke (ETS) on various aspects of adolescent development, specifically focusing on self-regulation, substance use, and externalizing behaviors. The data for this analysis were sourced from Dr. Lauren Micalizzi of the Brown University Department of Behavioral and Social Sciences. The study initially recruited a cohort of low-income pregnant women, totaling 738 individuals, as part of a smoke avoidance intervention program to reduce maternal smoking and ETS exposure during pregnancy. Furthermore, the study also assessed children's exposure to ETS in the immediate postpartum period. For this research, a subset of 100 adolescents and their mothers was randomly selected for recruitment, forming the core dataset.  
  
The data can be broadly categorized into two sections: child and parent data to gain a comprehensive understanding of the adolescent and parent dynamics. The child section has essential demographic information, including race and sex. Additionally, scores related to attention, internalizing and externalizing problems, and emotion regulation attributes such as Cognitive Reappraisal and Expressive Suppression. Moreover, the dataset encompasses information on child substance use and the parent-child relationship. The parent section mirrors many of the child's data categories. It also includes additional information such as the child's ADHD status, parental demographic details like income, employment, and education, and data directly related to smoking during pregnancy (SDP) and exposure to environmental tobacco smoke (ETS).  
  
Despite the richness of the dataset, it's important to acknowledge certain limitations that may influence the scope and interpretation of the analysis findings. First, cotinine levels, which serve as a biomarker of nicotine exposure, were only measured at two-time points---34 weeks gestation and 6 months after birth. This limitation necessitates a reliance on self-reported data for nicotine exposure in other instances. Second, the data analyzed in this report are limited to baseline measurements for both children and parents. Therefore, this study does not encompass longitudinal analysis, limiting the ability to draw conclusions about changes over time. Finally, this research's inclusion criteria required data for both the parent and child, resulting in a reduced dataset of 49 parent-child pairs. This reduced sample size may impact the generalizability of the findings to a broader population.  

The full code for this project can be found in the code folder and the explanation of the variables can be seen in the codebook folder.

## Method

- Conducted data preprocessing, which included aggregating questionnaire responses and removing irrelevant variables, for a dataset comprising 49 parent-child pairs. 
- Addressed missing data by concentrating on variables with minimal missing values and developed new variables for a more nuanced analysis. Introduced variables like 'Substance Variety' to quantify the total types of substances used, and 'Maternal Smoking Consistency' along with 'Environmental Smoking Exposure Consistency' to measure the intensity of exposure.
- Performed univariate analysis to examine demographic patterns and behavior scores.
- Conducted bivariate analysis to explore multiple relationships:
1. Between maternal prenatal and postnatal smoking, as well as prenatal and postnatal environmental. 
2. tobacco smoke exposure, to determine differences based on exposure.
3. Between various externalizing behavior variables to assess their interrelatedness.
With other potential variables, such as parental externalizing behaviors and demographic factors.
4. Preliminary Multivariate Analysis between the potential confounders for each externalizing behaviors using regression analysis.

## Result 

### Relationship between Maternal Smoking and Smoking Exposure


![Summary Table for Maternal Smoking](https://github.com/amirahff/PHP2550/blob/main/Project1/images/P1_F1.png)

![Summary Table for Smoking Exposure](https://github.com/amirahff/PHP2550/blob/main/Project1/images/P1_F2.png)

Using the Wilcoxon test at a 0.05 significance level, our findings indicate:
1. Children of mothers who smoked during pregnancy exhibit statistically significantly different scores in ADHD Hyperactivity.
2. Prenatal smoking exposure shows statistically significant differences in scores for both Internalizing and Externalizing Problems in children.
3. Postnatal smoking exposure (from year 1 to year 5) significantly affects children's scores in Expressive Suppression.

### Interrelatedness between Externalizing Behaviors
![Interrelatedness between Externalizing Behaviors](https://github.com/amirahff/PHP2550/blob/main/Project1/images/P1_F3.png){width = 50%}

Some notable correlation clusters observed among the variables are:
1. ADHD Inattentive, ADHD Hyperactive, Attention Problem, and Externalizing Problem.
2. Internalizing Problem, Attention Problem, and Externalizing problem.
3. Number of Substance Types Used and Externalizing Problem. 
4. Expressive suppression and Internalizing Problem. 
5. No correlation observed on Cognitive Reappraisal.

### Correlation between Externalizing Behaviors with Other Factors
![Correlation between Externalizing Behaviors with Other Factors](https://github.com/amirahff/PHP2550/blob/main/Project1/images/P1_F4.png)

Children's externalizing behaviors predominantly correlate with parental attention and internalizing issues, alongside consistent direct or environmental prenatal and postnatal smoking exposure. Additionally, parental involvement is negatively correlated with substance use in children.

### Conclusion 

- In general, exposure to smoking during pregnancy, whether from maternal smoking or secondary exposure within the environment, tends to show statistically significant difference among externalizing children behavior scores such as ADHD Hyperactivity scores, Internalizing and Externalizing Problems. 
- Postnatal smoking exposure notably affects Expressive Suppression scores in children.
- No statistically significant difference was observed on the Wilcoxon test for the Attention Problem, Cognitive Reappraisal, ADHD Inattentive Score, and Substance Variety, but some of these metrics established some degree of correlation with the statistically significant metrics.
- Other related variables, such as Parent Internalizing and Attention Problem, positively correlate with children's externalizing behaviors. This could indicate that parent behavior may also affect some of the children's externalizing behaviors.
- Increased parental involvement is associated with lower substance use in children.
- These findings suggest complex interrelations between parental behaviors, smoking exposure, and children's behavioral outcomes that need more advanced analysis, such as regression analyses or causal inference. Larger samples are needed to facilitate more extensive analyses to understand externalizing behaviors and their influencing factors better.


## Software and Packages

Version of software and packages used in the analysis:
R version 4.2.2 (2022-10-31)  
patchwork_1.1.2  
corrplot_0.92  
GGally_2.1.2    
kableExtra_1.3.4  
gtsummary_1.7.2  
gt_0.9.0  
naniar_1.0.0  
tidyverse_1.3.2  
ggplot2_3.4.3  