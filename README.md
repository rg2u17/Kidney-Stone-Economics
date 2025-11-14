# Discrete Event Simulation of EAU Follow-up Algorithm
## General Information
This repository details the underpinning data and R code used to model the European Association of Urology Follow-up Algorithm (see figure). <br>

The model examines multiple theoretical diagnostic accuracy thresholds for prediction of recurrence, as well as modelling differing types of imaging modality and lengths of follow-up. <br>

Each script is labelled in numeric order, and those wishing to replicate the model should run each in order. <br>

**There are two types of test score distribution** - both are labelled no. 4: <br>
1. Beta distribution for score - e.g. prognostic model output <br>
2. Normal distribution for scores - e.g. blood test result <br>
<br>

<img width="760" height="320" alt="image" src="https://github.com/user-attachments/assets/eee4aa40-d19e-4c0a-8518-ad3c1417d280" /> <br>
Figure. EAU follow-up algorithm for patients with kidney stones following treatment.
<br>

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
**File paths:**
At present, each file that is required is stored in the Inputs folder. We recommend setting the working directory for your project one level above this. When you run this command within R: <br>
<br>
```getwd()``` <br>
<br>
The output should be: <br>
<br>
_Kidney-Stone-Economics/Inputs_ <br>
<br>
Otherwise you will need to use:
<br>
```setwd('File/Path/To/Kidney-Stone-Economics')``` <br>
<br>
To set the working working directory to an appropriate place <br>
This will ensure the scripts run without having to alter the file path



