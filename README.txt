This folder contains the code and outputs for the causal inference final project.

Folder structure:

causal/
README.txt

scripts/
download_data.R
clean_data.R
dag.R
estimate.R
diagnostics.R
sensitivity.R

data/
raw/
processed/
codebooks/
fyc2018_variable_names.csv

outputs/
figures/
models/

Recommended execution order:

Run scripts/download_data.R

This downloads the 2018 MEPS Full-Year Consolidated File and saves the raw data as:

data/raw/fyc2018_raw.rds

Run scripts/clean_data.R

This cleans the raw MEPS data and constructs the analysis dataset. The cleaned dataset is saved as:

data/processed/analysis_df.rds

Run scripts/dag.R

This generates the DAG figure and reports the adjustment set used in the analysis.

Run scripts/estimate.R

This estimates the effect of private insurance coverage on flu vaccination using:

Naive difference in means
Outcome regression
IPW
AIPW
Bootstrap confidence intervals for outcome regression and AIPW

Run scripts/diagnostics.R

This produces propensity score and covariate balance diagnostics, including:

Propensity score overlap plot
Love plot
Balance table

Run scripts/sensitivity.R

This performs sensitivity analysis for unmeasured confounding using:

sensemakr
E-value

Additional documentation:

data/codebooks/fyc2018_variable_names.csv
contains the original MEPS variable names and descriptions used for reference during variable selection and data cleaning.

Notes:

The main analysis compares individuals with private insurance to uninsured individuals.

The outcome is whether the individual received a flu shot in the past year.

The adjustment variables used in the final analysis are:

age, race_ethnicity, education_years, poverty_category, region, physical_health, and mental_health.

The variable male was not included in the final adjustment set.

The scripts assume that the working directory is the main causal folder.