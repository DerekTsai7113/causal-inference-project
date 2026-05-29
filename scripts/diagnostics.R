# =========================================================
# diagnostics.R
# Propensity score diagnostics
# Private insurance vs uninsured
# =========================================================

library(tidyverse)
library(ggplot2)
library(cobalt)

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

# =========================================================
# Quick checks
# =========================================================

print("Treatment distribution:")

print(
  table(
    df$insured_private,
    useNA = "ifany"
  )
)

print("Outcome distribution:")

print(
  table(
    df$flu_shot,
    useNA = "ifany"
  )
)

# =========================================================
# Covariates
# male removed
# =========================================================

covariates = c(
  "age",
  "race_ethnicity",
  "poverty_category",
  "region",
  "physical_health",
  "mental_health",
  "education_years"
)

# =========================================================
# Propensity score model
# =========================================================

ps_formula = as.formula(
  paste(
    "insured_private ~",
    paste(covariates, collapse = " + ")
  )
)

ps_model = glm(
  ps_formula,
  data = df,
  family = binomial()
)

e_hat = predict(
  ps_model,
  type = "response"
)

eps = 0.01

e_hat = pmin(
  pmax(e_hat, eps),
  1 - eps
)

df$e_hat = e_hat

# =========================================================
# ATE IPW weights
# =========================================================

df$w_ate = ifelse(
  df$insured_private == 1,
  1 / df$e_hat,
  1 / (1 - df$e_hat)
)

# =========================================================
# Propensity score and weight summaries
# =========================================================

print("Summary of propensity scores:")

print(
  summary(df$e_hat)
)

print("Proportion e_hat < 0.05:")

print(
  mean(df$e_hat < 0.05)
)

print("Proportion e_hat > 0.95:")

print(
  mean(df$e_hat > 0.95)
)

print("Summary of ATE weights:")

print(
  summary(df$w_ate)
)

# =========================================================
# Propensity score overlap plot
# Save with png() to avoid RStudio redraw warning
# =========================================================

p_overlap = ggplot(
  df,
  aes(
    x = e_hat,
    fill = factor(insured_private)
  )
) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Propensity Score Overlap",
    x = "Estimated propensity score",
    fill = "Private insurance"
  ) +
  theme_minimal()

print(p_overlap)

png(
  filename = "outputs/figures/private_overlap_plot.png",
  width = 900,
  height = 700
)

print(p_overlap)

dev.off()

# =========================================================
# Love plot: SMD before and after weighting
# =========================================================

p_love = love.plot(
  ps_formula,
  data = df,
  weights = df$w_ate,
  method = "weighting",
  estimand = "ATE",
  stats = "mean.diffs",
  threshold = 0.1,
  binary = "std",
  abs = TRUE
)

print(p_love)

png(
  filename = "outputs/figures/private_love_plot.png",
  width = 900,
  height = 700
)

love.plot(
  ps_formula,
  data = df,
  weights = df$w_ate,
  method = "weighting",
  estimand = "ATE",
  stats = "mean.diffs",
  threshold = 0.1,
  binary = "std",
  abs = TRUE
)

dev.off()

# =========================================================
# Balance table
# =========================================================

balance_table = bal.tab(
  ps_formula,
  data = df,
  weights = df$w_ate,
  method = "weighting",
  estimand = "ATE",
  binary = "std"
)

print("Covariate balance table:")

print(balance_table)

# =========================================================
# Save diagnostics object
# =========================================================

saveRDS(
  list(
    ps_model = ps_model,
    balance_table = balance_table,
    diagnostics_data = df
  ),
  "outputs/models/private_ps_diagnostics.rds"
)