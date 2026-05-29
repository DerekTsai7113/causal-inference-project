
# Clean 2018 MEPS Full-Year Consolidated data
# Treatment: private insurance vs uninsured
# Outcome: flu vaccination in past 12 months

library(tidyverse)
library(haven)

set.seed(2026)

# Load raw data
# Assumes download_data.R has already been executed

fyc = readRDS("data/raw/fyc2018_raw.rds")

# Check required variables

required_vars = c(
  "DUPERSID",
  "AGE18X",
  "SEX",
  "RACETHX",
  "EDUCYR",
  "POVCAT18",
  "REGION18",
  "RTHLTH31",
  "MNHLTH31",
  "INSCOV18",
  "ADFLST42"
)

missing_vars = setdiff(required_vars, names(fyc))

if (length(missing_vars) > 0) {
  stop(
    "Missing required variables: ",
    paste(missing_vars, collapse = ", ")
  )
}


# Restrict to adults
df = fyc %>%
  filter(as.numeric(AGE18X) >= 18)


df = df %>%
  mutate(
    INSCOV18_num = as.numeric(INSCOV18),
    
    insured_private = case_when(
      INSCOV18_num == 1 ~ 1,
      INSCOV18_num == 3 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(insured_private))

# Outcome:
# Flu shot in past 12 months
#
# ADFLST42:
# SAQ 12 MTHS: FLU VACCINATION
#
# Coding:
# 1 = Yes
# 2 = No
# Negative values are treated as missing


df = df %>%
  mutate(
    ADFLST42_num = as.numeric(ADFLST42),
    
    flu_shot = case_when(
      ADFLST42_num == 1 ~ 1,
      ADFLST42_num == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Covariates

df = df %>%
  mutate(
    age = as.numeric(AGE18X),
    
    male = case_when(
      as.numeric(SEX) == 1 ~ 1,
      as.numeric(SEX) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    race_ethnicity = factor(as.numeric(RACETHX)),
    
    education_years = case_when(
      as.numeric(EDUCYR) >= 0 ~ as.numeric(EDUCYR),
      TRUE ~ NA_real_
    ),
    
    poverty_category = factor(as.numeric(POVCAT18)),
    
    region = factor(as.numeric(REGION18)),
    
    physical_health = factor(as.numeric(RTHLTH31)),
    
    mental_health = factor(as.numeric(MNHLTH31))
  )

# Keep analysis variables(produce clean data set)
analysis_df = df %>%
  select(
    DUPERSID,
    insured_private,
    flu_shot,
    age,
    male,
    race_ethnicity,
    education_years,
    poverty_category,
    region,
    physical_health,
    mental_health
  )

analysis_df = analysis_df %>%
  drop_na()

saveRDS(
  analysis_df,
  "data/processed/analysis_df.rds"
)


# Basic summaries

print(list(
  treatment_distribution = table(
    analysis_df$insured_private,
    useNA = "ifany"
  ),
  outcome_distribution = table(
    analysis_df$flu_shot,
    useNA = "ifany"
  ),
  dimensions = dim(analysis_df)
))