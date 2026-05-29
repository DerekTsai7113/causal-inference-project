# =========================================================
# estimate.R
# Estimation of causal effect:
# Private insurance vs uninsured
# =========================================================

library(tidyverse)

set.seed(2026)

analysis_df = readRDS("data/processed/analysis_df.rds")

df = analysis_df %>%
  mutate(
    insured_private = as.numeric(insured_private),
    flu_shot = as.numeric(flu_shot)
  )

# Naive difference


risk_by_group = df %>%
  group_by(insured_private) %>%
  summarise(
    n = n(),
    flu_rate = mean(flu_shot),
    .groups = "drop"
  )

print(risk_by_group)

naive_diff =
  risk_by_group$flu_rate[risk_by_group$insured_private == 1] -
  risk_by_group$flu_rate[risk_by_group$insured_private == 0]

print(paste(
  "Naive risk difference:",
  round(naive_diff, 3)
))


# Outcome regression


fit_or = glm(
  flu_shot ~
    insured_private +
    age +
    race_ethnicity +
    education_years +
    poverty_category +
    region +
    physical_health +
    mental_health,
  data = df,
  family = binomial()
)

# Standardized risks

df1 = df
df1$insured_private = 1

df0 = df
df0$insured_private = 0

risk1 = mean(
  predict(
    fit_or,
    newdata = df1,
    type = "response"
  )
)

risk0 = mean(
  predict(
    fit_or,
    newdata = df0,
    type = "response"
  )
)

or_rd = risk1 - risk0

print(paste(
  "OR risk if private insured:",
  round(risk1, 3)
))

print(paste(
  "OR risk if uninsured:",
  round(risk0, 3)
))

print(paste(
  "Outcome regression risk difference:",
  round(or_rd, 3)
))


# Propensity score model


ps_model = glm(
  insured_private ~
    age +
    race_ethnicity +
    education_years +
    poverty_category +
    region +
    physical_health +
    mental_health,
  data = df,
  family = binomial()
)

df$ps = predict(
  ps_model,
  type = "response"
)

print(summary(df$ps))

# Outcome model for AIPW


outcome_model = glm(
  flu_shot ~
    insured_private +
    age +
    race_ethnicity +
    education_years +
    poverty_category +
    region +
    physical_health +
    mental_health,
  data = df,
  family = binomial()
)

# Predicted outcomes

df1 = df
df1$insured_private = 1

df0 = df
df0$insured_private = 0

mu1_hat = predict(
  outcome_model,
  newdata = df1,
  type = "response"
)

mu0_hat = predict(
  outcome_model,
  newdata = df0,
  type = "response"
)


# AIPW 

A = df$insured_private
Y = df$flu_shot
e = df$ps

aipw_scores =
  mu1_hat - mu0_hat +
  A * (Y - mu1_hat) / e -
  (1 - A) * (Y - mu0_hat) / (1 - e)

aipw_estimate = mean(aipw_scores)

print(paste(
  "AIPW estimate:",
  round(aipw_estimate, 3)
))


# IPW 

ipw_estimate =
  mean(
    A * Y / e -
      (1 - A) * Y / (1 - e)
  )

print(paste(
  "IPW estimate:",
  round(ipw_estimate, 3)
))


results = data.frame(
  Estimator = c(
    "Naive",
    "Outcome regression",
    "IPW",
    "AIPW"
  ),
  Estimate = c(
    naive_diff,
    or_rd,
    ipw_estimate,
    aipw_estimate
  )
)

print(results)

# Bootstrap for AIPW


B = 100

boot_aipw = replicate(B, {
  
  idx = sample(
    1:nrow(df),
    replace = TRUE
  )
  
  d = df[idx, ]
  
  ps_model = glm(
    insured_private ~
      age +
      race_ethnicity +
      education_years +
      poverty_category +
      region +
      physical_health +
      mental_health,
    data = d,
    family = binomial()
  )
  
  d$ps = predict(
    ps_model,
    type = "response"
  )
  
  outcome_model = glm(
    flu_shot ~
      insured_private +
      age +
      race_ethnicity +
      education_years +
      poverty_category +
      region +
      physical_health +
      mental_health,
    data = d,
    family = binomial()
  )
  
  d1 = d
  d1$insured_private = 1
  
  d0 = d
  d0$insured_private = 0
  
  mu1 = predict(
    outcome_model,
    newdata = d1,
    type = "response"
  )
  
  mu0 = predict(
    outcome_model,
    newdata = d0,
    type = "response"
  )
  
  A = d$insured_private
  Y = d$flu_shot
  e = d$ps
  
  scores =
    mu1 - mu0 +
    A * (Y - mu1) / e -
    (1 - A) * (Y - mu0) / (1 - e)
  
  mean(scores)
})

aipw_ci = quantile(
  boot_aipw,
  c(0.025, 0.975)
)

print("AIPW 95% bootstrap CI:")

print(aipw_ci)


# Bootstrap for outcome regression

boot_or = replicate(B, {
  
  idx = sample(
    1:nrow(df),
    replace = TRUE
  )
  
  d = df[idx, ]
  
  outcome_model = glm(
    flu_shot ~
      insured_private +
      age +
      race_ethnicity +
      education_years +
      poverty_category +
      region +
      physical_health +
      mental_health,
    data = d,
    family = binomial()
  )
  
  d1 = d
  d1$insured_private = 1
  
  d0 = d
  d0$insured_private = 0
  
  mu1 = predict(
    outcome_model,
    newdata = d1,
    type = "response"
  )
  
  mu0 = predict(
    outcome_model,
    newdata = d0,
    type = "response"
  )
  
  mean(mu1 - mu0)
})

or_ci = quantile(
  boot_or,
  c(0.025, 0.975)
)

print("Outcome regression 95% bootstrap CI:")

print(or_ci)