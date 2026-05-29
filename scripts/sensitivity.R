# =========================================================
# sensitivity.R
# Sensitivity analysis for unmeasured confounding
# Part 1: sensemakr
# Part 2: E-value
# Private insurance vs uninsured
# =========================================================

library(tidyverse)
library(sensemakr)
library(EValue)

set.seed(2026)

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/models",
  recursive = TRUE,
  showWarnings = FALSE
)

analysis_df = readRDS("data/processed/analysis_df.rds")

df = analysis_df %>%
  mutate(
    insured_private = as.numeric(insured_private),
    flu_shot = as.numeric(flu_shot)
  )

# Part 1: sensemakr

reg_model = lm(
  flu_shot ~
    insured_private +
    age +
    race_ethnicity +
    poverty_category +
    region +
    physical_health +
    mental_health +
    education_years,
  data = df
)

print(summary(reg_model))

coef_table = as.data.frame(summary(reg_model)$coefficients)
coef_table$variable = rownames(coef_table)

poverty_candidates = coef_table %>%
  filter(grepl("^poverty_category", variable)) %>%
  mutate(abs_t = abs(`t value`)) %>%
  arrange(desc(abs_t))

physical_candidates = coef_table %>%
  filter(grepl("^physical_health", variable)) %>%
  mutate(abs_t = abs(`t value`)) %>%
  arrange(desc(abs_t))

best_poverty = poverty_candidates$variable[1]
best_physical = physical_candidates$variable[1]

benchmark_vars = c(
  best_poverty,
  best_physical,
  "education_years",
  "age"
)

print("Benchmark variables used in sensemakr:")
print(benchmark_vars)

sens = sensemakr(
  model = reg_model,
  treatment = "insured_private",
  benchmark_covariates = benchmark_vars,
  kd = c(1, 2, 3)
)

print(summary(sens))

plot(sens)

png(
  filename = "outputs/figures/sensitivity_plot.png",
  width = 900,
  height = 700
)

plot(sens)

dev.off()

bounds_table = as.data.frame(sens$bounds)

write.csv(
  bounds_table,
  "outputs/models/sensemakr_bounds.csv",
  row.names = FALSE
)


# Part 2: E-value


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

df1 = df
df1$insured_private = 1

df0 = df
df0$insured_private = 0

p1_hat = predict(
  outcome_model,
  newdata = df1,
  type = "response"
)

p0_hat = predict(
  outcome_model,
  newdata = df0,
  type = "response"
)

risk1 = mean(p1_hat)
risk0 = mean(p0_hat)

risk_difference = risk1 - risk0
risk_ratio = risk1 / risk0

risk_results = data.frame(
  Quantity = c(
    "Risk if private insured",
    "Risk if uninsured",
    "Risk difference",
    "Risk ratio"
  ),
  Value = c(
    risk1,
    risk0,
    risk_difference,
    risk_ratio
  )
)

print(risk_results)

evalue_point = evalues.RR(
  est = risk_ratio,
  lo = NA,
  hi = NA
)

print("E-value for standardized risk ratio:")
print(evalue_point)

evalue_results = as.data.frame(evalue_point)

write.csv(
  risk_results,
  "outputs/models/evalue_risk_results.csv",
  row.names = FALSE
)

write.csv(
  evalue_results,
  "outputs/models/evalue_results.csv",
  row.names = FALSE
)