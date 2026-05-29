# =========================================================
# dag.R
# Two DAG versions with short labels and optimized text sizes.
# Latent confounding (U / bidirected arrow) removed for sensitivity analysis.
# =========================================================

library(dagitty)
library(ggdag)
library(ggplot2)

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Variable Legend:
# Insu = Insurance
# Flu  = FluShot
# POV  = Poverty
# EDU  = Education
# Phys = PhysicalHealth
# Ment = MentalHealth
# Reg  = Region
# SES  = Socioeconomic Status (Poverty + Education)
# Hth  = General Health (Physical + Mental)

# =========================================================
# FULL DAG
# =========================================================

g_full = dagitty("
dag {

  Insu -> Flu

  Age -> Insu
  Age -> Flu

  POV -> Insu
  POV -> Flu

  EDU -> Insu
  EDU -> Flu

  Race -> Insu
  Race -> Flu

  Phys -> Insu
  Phys -> Flu

  Ment -> Insu
  Ment -> Flu

  Reg -> Insu
  Reg -> Flu
}
")

coordinates(g_full) = list(
  x = c(
    Age = 0,
    POV = 0,
    EDU = 0,
    Race = 0,
    Phys = 4,
    Ment = 4,
    Reg = 4,
    Insu = 2,
    Flu = 6
  ),
  
  y = c(
    Age = 7,
    POV = 6,
    EDU = 5,
    Race = 4,
    Phys = 7,
    Ment = 6,
    Reg = 5,
    Insu = 5.5,
    Flu = 5.5
  )
)

p_full = ggdag(g_full, text = FALSE) +
  geom_dag_edges(
    edge_colour = "gray45",
    edge_width = 0.9
  ) +
  geom_dag_point(
    color = "#2C7FB8",
    size = 18
  ) +
  geom_dag_text(
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  theme_dag() +
  expand_plot(
    expand_x = expansion(c(0.12, 0.12)), 
    expand_y = expansion(c(0.12, 0.12))
  ) +
  ggtitle("Full DAG: Insurance and Flu Vaccination")

print(p_full)

ggsave(
  filename = "outputs/figures/full_dag.png",
  plot = p_full,
  width = 12,
  height = 8,
  dpi = 300
)

# =========================================================
# Adjustment sets
# =========================================================

print("Minimal adjustment sets")

print(
  adjustmentSets(
    g_full,
    exposure = "Insu",
    outcome = "Flu",
    type = "minimal"
  )
)