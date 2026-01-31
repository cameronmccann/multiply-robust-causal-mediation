# Estimation of Causal Mediation Effects under Unmeasured Cluster-Level Confounding Using Machine Learning

## Overview

We conducted a simulation study and an empirical application to assess the performance of a multiply-robust estimator for causal mediation analysis with clustered data when an unmeasured cluster-level confounder is present. This repository contains the code and data used at each stage of the study (i.e., Simulation Study and Empirical Application).

In the **simulation study**, there are individual-level treatment (binary), mediator (binary or continuous), and outcome (binary or continuous) variables. We assessed the multiply-robust estimator’s ability to accurately estimate the cluster-average and individual-average natural direct and indirect effects when an unmeasured cluster-level confounder and three individual-level covariates influence the treatment-mediator and mediator-outcome relationships. 

The **Empirical Application** uses data from the National Longitudinal Study of Adolescent to Adult Health (Add Health; Harris & Udry, 2008) to illustrate the application of the proposed method. Specifically, we estimate the direct and indirect effects of adolescent sports participation on adult depression through late-adolescent self-esteem. The cleaned version of the dataset used is included in the `Application/Data/Cleaned` folder.

## Folder Structure

The repository is organized into four main folders, which include the following information:
```
    ├── README.md
    ├── Code/                                   # Scripts to conduct simulation & obtain results (e.g., bias, MSE, etc)
    │   ├── 01_S1-conduct-simulation_tacc.R 
    │   ├── 02_S1-results-processing.R
    │   ├── 03_S1-post-processing.R 
    │   ├── 04_S1-report-results.R
    │   └── 05_S1-study-design.R                
    |
    ├── Functions/                              # Helper functions for data generation and estimation             
    |   ├── a.c.R
    │   ├── a.mc.R
    │   ├── bound.R
    │   ├── crossfit.R
    │   ├── effect.R
    │   ├── eif.R
    │   ├── estimate_mediation.R
    │   ├── generate_clusters.R
    │   ├── generate_confounders.R
    │   ├── generate_data.R
    │   ├── generate_mediator.R
    │   ├── generate_outcome.R
    │   ├── generate_treatment.R
    │   ├── get_inference.R
    │   ├── internal_estimate_mediation.R
    │   ├── m.ac.R
    │   ├── make_fold_K.R
    │   ├── mu.ac.R
    │   ├── mu.mac.R
    │   ├── my.R
    │   ├── pm1.R
    │   ├── trueVals.R
    │   └── v.ac.R
    |
    ├── Output/                        
    │   ├── S1_Simulation-Output/               # Simulation output (e.g., direct & indirect estimates) for each run under each simulation
    |   |   └── .../ 
    │   └── S1_Results/                         # Performance measures for each simulation condition 
    |       ├── Data/
    │       ├── Figures/
    |       └── Tables/
    |
    └── Application/                            # Main folder for the empirical application 
        ├── Code/
        |   ├── 01_empirical-application_data-cleaning.R
        │   └── 02_empirical-application_analysis.R
        ├── Data/
        │   ├── Raw/
        │   └── Cleaned/
        ├── Functions/
        └── Output/
            
```

## Requirements and Dependencies

This repository requires R (version 4.x or higher) and specific R packages for conducting the simulation study and the empirical application. The scripts use the pacman package to automatically install and load missing packages. 

### Simulation Study
#### Conducting the Simulation 

The following packages are required to run the simulations:

- `doParallel`
- `foreach`
- `parallel`
- `purrr` 
- `glue` 
- `fastDummies` 
- `dplyr`
- `readr`
- `ggplot2`
- `stringr`

#### Obtain the Simulation Results 

The following packages are required to analyze the simulation results:

- `tidyverse`
- `ggplot2`
- `flextable`
- `stringr`
- `ggdag`
- `dagitty`
- `huxtable`
- `dplyr`

### Empirical Application

The following packages are required for the empirical application:

- `tidyverse`
- `ggplot2`
- `extrafont`
- `stringr`
- `mice`
- `boot`
- `utils`
- `parallel`
- `glue` 
- `mvtnorm` 
- `SuperLearner` 
- `origami` 
- `fastDummies`

## How to Use This Repository

### Running the Simulation

To conduct the simulation and obtain results, follow these steps: 

1. Clone or download the repository.
2. Navigate to the `Code` folder.
3. Run the simulation scripts in the following order:
    - `01_S1-conduct-simulation_tacc.R` 
    - `02_S1-results-processing.R` 
    - `03_S1-post-processing.R` 
    - `04_S1-report-results.R` 
        
### Running the Empirical Application

To reproduce the empirical application results, follow these steps: 

1. Navigate to the `Application/Code` folder.
2. Run `01_empirical-application_data-cleaning.R` (optional).
3. Execute `02_empirical-application_analysis.R` to analyze the empirical dataset and obtain the effect estimates.
4. The results will be saved in the `Application/Output` folder.

        
