# Discrete Event Simulation of EAU Follow-up Algorithm

This repository details the underpinning data and R code used to model the European Association of Urology Follow-up Algorithm (see figure). <br>

The model examines multiple theoretical diagnostic accuracy thresholds for prediction of recurrence, as well as modelling differing types of imaging modality and lengths of follow-up. <br>

Each script for is labelled in numeric order, and those wishing to replicate the model should run each in order. <br>

There are two models - both labelled no. 4: <br>
1. Normal distribution for scores - e.g. blood test result <br>
2. Beta distribution for score - e.g. prognostic model output <br>
<br>

<img width="760" height="320" alt="image" src="https://github.com/user-attachments/assets/eee4aa40-d19e-4c0a-8518-ad3c1417d280" /> <br>
Figure. EAU follow-up algorithm for patients with kidney stones following treatment.

**To download this model use:** <br>
```git clone git@github.com:rg2u17/Kidney-Stone-Economics.git``` <br>
<br>
**To run this model we suggest using the rmarkdown file in RStudio and selecting run all** <br>
Please note that this requires a significant amount of compute to run - we recommend running this in a HPC cluster otherwise it may take several days to run. <br>
Efforts have been made to minimise compute load within each script.
