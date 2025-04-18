# Multiply Robust Causal Mediation 

## Overview

We conducted a simulation study and an empirical application to assess the performance of the multiply robust estimator in estimating mediation effects with clustered data when an unmeasured cluster-level confounder is present. This repository contains the code and data used at each stage of the study (i.e., Simulation Study and Empirical Application).

In the **simulation study**, there are individual-level treatment (binary), mediator (binary or continuous), and outcome (binary or continuous) variables. We assessed the multiply robust estimator’s ability to accurately estimate the cluster- and individual-average (in)direct effect when an unmeasured cluster-level confounder and three individual-level covariates influence the treatment-mediator and mediator-outcome relationships. 

The **Empirical Application** uses data from the National Longitudinal Study of Adolescent to Adult Health (Add Health; Harris & Udry, 2008) to illustrate the application of the methods evaluated. The cleaned version of the dataset used is included in the `Application/Data/Cleaned` folder.

## Folder Structure

The repository is organized into four main folders, which include the following information:
```
    ├── README.md
    ├── Code/                          
    │   ├── 01_S1-conduct-simulation_tacc.R   # Conducts the simulation
    │   └── 02_S1-obtain-results_tacc.R       # Processes results & generates figures
    ├── Functions/                            # Helper functions for data generation and estimation             
    │   ├── generate_clusters.R
    │   ├── generate_confounders.R
    │   ├── generate_treatment.R
    │   ├── generate_mediator.R
    │   ├── generate_outcome.R
    │   ├── pm1.R
    │   ├── my.R
    │   ├── generate_data2.0c.R
    │   ├── trueVals2.0d.R
    │   ├── trueVals2.0f.R
    │   ├── crossfit.R
    │   ├── make_fold_K.R
    │   ├── eif.R
    │   ├── a.c.R
    │   ├── a.mc.R
    │   ├── mu.mac.R
    │   ├── v.ac.R
    │   ├── mu.ac.R
    │   ├── get_inference.R
    │   ├── internal_estimate_mediation.R
    │   ├── bound.R
    │   ├── effect.R
    │   └── estimate_mediation.R
    ├── Output/                        
    │   ├── S1_Simulation-Output/               # Simulation output (e.g., direct & indirect estimates) for each run under each simulation
    |   |   └── .../ 
    │   └── S1_Results/                         # Performance measures for each simulation condition 
    │       └── .../ 
    |           ├── Data/
    │           ├── Figures/
    |           └── Tables/
    └── Application/                            # Main folder for the empirical application 
        ├── Code/
        |   ├── 01_empirical-application_data-cleaning.R
        │   └── 02b_empirical-application_analysis.R
        ├── Data/
        │   ├── Raw/
        │   └── Cleaned/
        ├── Functions/
        └── Output/
            
```

## Requirements and Dependencies

This repository requires R (version 4.x or higher) and specific R packages for conducting the simulation study and the empirical application. The scripts use the pacman package to automatically install and load missing packages, but you can manually install them if preferred. 

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
    - `01_S1-conduct-simulation_tacc.R`: Runs the first simulation and stores output (e.g., direct & indirect estimates) for each run under each simulation condition in the `Output/S1_Simulation-Output` folder.
    - `02_S1-obtain-results_tacc.R`: Processes output from the first simulation by computing performance metrics and generating visuals and tables (stored in `Output/S1_Results`).
    # - Repeat the process for the second simulation (`S2`) and the Supplemental Simulations (`S1_Supp1`, `S1_Supp2`, and `S1_Supp3`).
        
        
### Running the Empirical Application

1. Navigate to the `Application/Code` folder.
2. Run `01_empirical-application_data-cleaning.R` to prepare the data for analysis (optional).
3. Execute `02_empirical-application_analysis.R` to analyze the empirical dataset and obtain the effect estimates.
4. The results, including visualizations and estimates, will be saved in the `Application/Output` folder.

        
