# Discrete Event Simulation of EAU Follow-up Algorithm
## General Information
This repository details the underpinning data and R code used to model the European Association of Urology Follow-up Algorithm (see figure 1). <br>

<img width="760" height="320" alt="image" src="https://github.com/user-attachments/assets/eee4aa40-d19e-4c0a-8518-ad3c1417d280" /> <br>
Figure 1. EAU follow-up algorithm for patients with kidney stones following treatment.
<br>

The model examines both the current standard of care (see below), and multiple theoretical diagnostic accuracy thresholds for prediction of recurrence. <br>
The model examines differing types of imaging modality and lengths of follow-up. <br>

Ascertainment of the current standard of care is detailed below:

### Ascertainment of Current Standard of Care
We utilised UK Biobank to ascertain the prognostic accuracy of the current standard of care (EAU risk stratification - Table 3.3: https://uroweb.org/guidelines/urolithiasis/chapter/guidelines) <br>
<br>
The methodology to do this is detailed in the following link: <br>
<br>
```https://github.com/rg2u17/Recurrence_ML``` <br>
<br>
Then selecting the following script: <br>
<br>
*5. Ascertain Past medical history for kidney stone formers*
<br>

#### Definitions:
We examined **stone event** rates at both 5 and 10 years and these are defined below: <br>
<br>
**Stone Event** = renal/ureteric colic OR intervention <br>
**Recurrence** = Subsequent stone event >6 months after previous <br>
<br>

#### 5 Year Stone event rates

<img width="760" height="500" alt="image" src="https://github.com/user-attachments/files/24189808/EAU.Risk.stratification.counts.UKB.tiff" /> <br>
**Figure 2.** Counts of stone events over 5 years subdivided by EAU risk stratification <br>

<img width="760" height="500" alt="image" src="https://github.com/user-attachments/files/24189807/EAU.Risk.stratification.percentages.UKB.tiff" /> <br>
**Figure 3.** Proportions of stone events over 5 years subdivided by EAU risk stratification <br>

<img width="592" height="414" alt="image" src="https://github.com/user-attachments/assets/4089a5d7-f10e-40c6-b2d5-8a514dbf2c75" /> <br>
**Figure 4.** Kaplan Meier plot of 1st -> 2nd stone event (symptomatic recurrence) <br>

#### 10 Year Stone event rates

<img width="760" height="500" alt="image" src="https://github.com/user-attachments/files/24189857/EAU.risk.stratification.10.year.counts.tiff" /> <br>
**Figure 5.** Counts of stone events over 10 years subdivided by EAU risk stratification <br>



Each script is labelled in numeric order, and those wishing to replicate the model should run each in order. <br>

## How to Download Model
**To download this model use:** <br>
```git clone https://github.com/rg2u17/Kidney-Stone-Economics.git``` <br>
<br>

## How to Run Model
**To run this model we suggest using the rmarkdown file (kidney_stone_economics_full_model.rmd) in RStudio and selecting run all** <br>
Please note that this requires a significant amount of compute to run - Despite efforts to minimise this within each script <br>
<br>
We recommend running the scripts sequentially in a HPC cluster (if available), alternatively running it on a local PC may take several days to run. <br>
<br>
Prior to running all the scripts please check all packages are installed - if not, then you can run this in the console prior to running all the markdown chunks: <br>
```install.packages(c("tidyverse", "gt", "gtExtra", "DiagrammeR", "data.table", "janitor", "pROC", "ggplot2", "pracma", "glue", "cutpointr", "caret", "cowplot", "ggsignif", "ggpubr", "broom", "pryr", "lubridate", "sfsmisc", "patchwork", "mice"))``` <br>
<br>

**NB: There are two types of test score distribution** - both are labelled no. 4: <br>
1. Beta distribution for score - e.g. prognostic model output <br>
2. Normal distribution for scores - e.g. blood test result <br>
<br>

**File paths:**
At present, each file that is required is stored in the Inputs folder. We recommend setting the working directory for your project one level above this. When you run this command within R: <br>
<br>
```getwd()``` <br>
<br>
The output should be: <br>
<br>
.../Kidney-Stone-Economics <br>
<br>
Otherwise you will need to assign the working directory to where the file is stored, for example:
<br>
```setwd('~/Desktop/Kidney-Stone-Economics')``` <br>
<br>
To set the working working directory to an appropriate place <br>
This will ensure the scripts run without having to alter the file path

