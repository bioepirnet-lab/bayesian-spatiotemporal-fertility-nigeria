# ============================================================
# REVISION PROJECT SETUP
# ============================================================

# ============================================================
# PROJECT WORKING DIRECTORY
# ============================================================
# IMPORTANT:
# Before running this script, change the working directory
# below to the location where you have saved the project folder.
#
# Example:
# setwd("C:/Users/YourName/Documents/TRFPROJECT_EDIT")
#
# Replace the example path with your own project directory.

setwd("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT")

# Define the results directory BEFORE checking it
results_dir <- file.path(getwd(), "Model_Result")


if (!dir.exists(results_dir)) {
  stop(
    "Model_Result folder not found. ",
    "Please check that the working directory is the project directory."
  )
}

cat("Revision results directory:\n")
cat(results_dir, "\n\n")

# Packages needed for reviewer analyses
library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tibble)
library(INLA)
library(viridis)

# ============================================================
# REVISION SCRIPT
# ============================================================

results_dir <- file.path(getwd(), "Model_Result")

# Load main data
model_data <- readRDS(
  file.path(results_dir, "model_data.rds")
)

model_data_inla <- readRDS(
  file.path(results_dir, "model_data_inla.rds")
)

# Load final model
res_interaction <- readRDS(
  file.path(results_dir, "res_interaction.rds")
)

# Load other original models
res0 <- readRDS(file.path(results_dir, "res0.rds"))
res1 <- readRDS(file.path(results_dir, "res1.rds"))
res2 <- readRDS(file.path(results_dir, "res2.rds"))
res3 <- readRDS(file.path(results_dir, "res3.rds"))
res_full <- readRDS(file.path(results_dir, "res_full.rds"))

# Load spatial information
nga_states <- readRDS(
  file.path(results_dir, "nga_states.rds")
)

nb <- readRDS(
  file.path(results_dir, "nb.rds")
)

g <- readRDS(
  file.path(results_dir, "adjacency_graph.rds")
)

# Load posterior fertility results
lambda_mat <- readRDS(
  file.path(results_dir, "lambda_mat.rds")
)

asfr_summary <- readRDS(
  file.path(results_dir, "asfr_summary.rds")
)

tfr_summary <- readRDS(
  file.path(results_dir, "tfr_summary.rds")
)

# Load diagnostics
cpo_vals <- readRDS(
  file.path(results_dir, "cpo_values.rds")
)

pit_vals <- readRDS(
  file.path(results_dir, "pit_values.rds")
)

# ============================================================
# CHECK THAT REQUIRED ORIGINAL RESULTS ARE AVAILABLE
# ============================================================

required_objects <- c(
  "model_data",
  "model_data_inla",
  "res_interaction",
  "res0",
  "res_full",
  "res1",
  "res2",
  "res3",
  "nga_states",
  "nb",
  "g",
  "lambda_mat",
  "asfr_summary",
  "tfr_summary",
  "cpo_vals",
  "pit_vals"
)

loaded_objects <- ls()

missing_objects <- setdiff(
  required_objects,
  loaded_objects
)

if (length(missing_objects) > 0) {
  stop(
    "The following required objects are missing: ",
    paste(missing_objects, collapse = ", ")
  )
}

cat("All required original analysis objects loaded successfully.\n")

# Check the analytical dataset
dim(model_data_inla)
names(model_data_inla)

# Check final model fixed effects
rownames(res_interaction$summary.fixed)

# Check available random effects
names(res_interaction$summary.random)



# Reviewer 1
# Comment 1 — VIF
# Comment 2 — Health facility scaling
# Comment 3 — Historical health facility
# Minor comments

# Reviewer 2
# Comment 1 — Predictive validation
# Comment 2 — Exposure
# Comment 3 — Education



# ============================================================
# REVIEWER 1 — COMMENT 1
# VARIANCE INFLATION FACTOR (VIF) DIAGNOSTIC
# ============================================================

# Fixed-effect covariates in the final INLA model
vif_vars <- c(
  "prop_urban",
  "prop_secondary",
  "prop_higher",
  "prop_middle",
  "prop_richer",
  "prop_richest",
  "prop_currently_married",
  "mean_age",
  "pop_density",
  "nightlights",
  "conflict_events",
  "hf_density"
)

# Extract the exact complete-case data for these variables
vif_data <- model_data_inla %>%
  dplyr::select(all_of(vif_vars)) %>%
  tidyr::drop_na()

# Check the number of observations used
cat("Number of observations used for VIF:", nrow(vif_data), "\n")

# ------------------------------------------------------------
# Calculate VIF manually for every predictor
# ------------------------------------------------------------

vif_values <- sapply(vif_vars, function(var) {
  
  other_vars <- setdiff(vif_vars, var)
  
  auxiliary_formula <- as.formula(
    paste(
      var,
      "~",
      paste(other_vars, collapse = " + ")
    )
  )
  
  auxiliary_model <- lm(
    auxiliary_formula,
    data = vif_data
  )
  
  R2 <- summary(auxiliary_model)$r.squared
  
  1 / (1 - R2)
})

# Create results table
vif_results <- data.frame(
  Covariate = vif_vars,
  VIF = as.numeric(vif_values)
)

# Sort from highest to lowest VIF
vif_results <- vif_results %>%
  arrange(desc(VIF))

# Display results
print(vif_results)

# ------------------------------------------------------------
# Save VIF results
# ------------------------------------------------------------

saveRDS(
  vif_results,
  file.path(results_dir, "R1_C1_VIF_results.rds")
)

write.csv(
  vif_results,
  file.path(results_dir, "R1_C1_VIF_results.csv"),
  row.names = FALSE
)

cat("\nVIF results saved to Model_Result.\n")


# ============================================================
# VIF SUMMARY
# ============================================================

cat("\nVIF summary:\n")
cat("Minimum VIF:", min(vif_results$VIF), "\n")
cat("Maximum VIF:", max(vif_results$VIF), "\n")

cat("\nVariables with VIF > 5:\n")
print(
  vif_results %>%
    filter(VIF > 5)
)

cat("\nVariables with VIF > 10:\n")
print(
  vif_results %>%
    filter(VIF > 10)
)



# ============================================================
# REVIEWER 1 - COMMENT 2
# Inspect contextual variables before rescaling
# ============================================================

# Health facility density
summary(model_data_inla$hf_density)
sd(model_data_inla$hf_density, na.rm = TRUE)
quantile(model_data_inla$hf_density,
         probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1),
         na.rm = TRUE)

# Population density
summary(model_data_inla$pop_density)
sd(model_data_inla$pop_density, na.rm = TRUE)
quantile(model_data_inla$pop_density,
         probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1),
         na.rm = TRUE)

# Inspect the distributions visually
par(mfrow = c(1, 2))

hist(model_data_inla$hf_density,
     main = "Health Facility Density",
     xlab = "HF density")

hist(model_data_inla$pop_density,
     main = "Population Density",
     xlab = "Population density")

par(mfrow = c(1, 1))


# ============================================================
# REVIEWER 1 - COMMENT 2
# Rescaling contextual variables
# ============================================================

# 1. Standardize health facility density
model_data_inla$hf_density_z <- as.numeric(
  scale(model_data_inla$hf_density)
)

# 2. Log-transform and standardize population density
model_data_inla$log_pop_density <- log(model_data_inla$pop_density)

model_data_inla$log_pop_density_z <- as.numeric(
  scale(model_data_inla$log_pop_density)
)

# Check the transformed variables
summary(model_data_inla$hf_density_z)
sd(model_data_inla$hf_density_z, na.rm = TRUE)

summary(model_data_inla$log_pop_density_z)
sd(model_data_inla$log_pop_density_z, na.rm = TRUE)


# Correlation between the two contextual variables
cor(
  model_data_inla$hf_density_z,
  model_data_inla$log_pop_density_z,
  use = "complete.obs"
)


# ============================================================
# REVIEWER 1 - COMMENT 2
# Recreate the original state-year interaction index
# ============================================================

model_data_inla <- model_data_inla %>%
  mutate(
    st_id = as.integer(interaction(state_id, year_id, drop = TRUE))
  )

# Check
length(unique(model_data_inla$st_id))

table(model_data_inla$survey_year)

head(
  model_data_inla %>%
    select(REGCODE, survey_year, state_id, year_id, st_id)
)

#########################################################
### Verify that the interaction index is correct

model_data_inla %>%
  select(state_id, year_id, survey_year, st_id) %>%
  distinct() %>%
  arrange(state_id, year_id) %>%
  head(20)

# ============================================================
# REVIEWER 1 - COMMENT 2
# Revised final interaction model
# Contextual variables rescaled
# ============================================================

res_interaction_scaled <- inla(
  births ~
    prop_urban +
    prop_secondary +
    prop_higher +
    prop_middle +
    prop_richer +
    prop_richest +
    prop_currently_married +
    mean_age +
    log_pop_density_z +
    nightlights +
    conflict_events +
    hf_density_z +
    
    f(state_id,
      model = "besag",
      graph = g,
      scale.model = TRUE) +
    
    f(year_id,
      model = "rw2") +
    
    f(age_id,
      model = "iid") +
    
    f(st_id,
      model = "iid"),
  
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(compute = TRUE),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

summary(res_interaction_scaled)

res_interaction_scaled$dic$dic
res_interaction_scaled$waic$waic


fixed_scaled <- as.data.frame(
  res_interaction_scaled$summary.fixed
)

fixed_scaled <- fixed_scaled %>%
  tibble::rownames_to_column("covariate") %>%
  mutate(
    RR = exp(mean),
    RR_low = exp(`0.025quant`),
    RR_high = exp(`0.975quant`)
  )

fixed_scaled


# ============================================================
# REVIEWER 1 - COMMENT 2
# REVISED FULL MODEL
# ============================================================

formula_full_scaled <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  
  f(state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE) +
  
  f(year_id,
    model = "rw2") +
  
  f(age_id,
    model = "iid")

res_full_scaled <- inla(
  formula_full_scaled,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

summary(res_full_scaled)

res_full_scaled$dic$dic
res_full_scaled$waic$waic

# ============================================================
# REVISED MODEL COMPARISON
# ============================================================

model_comparison_scaled <- data.frame(
  Model = c(
    "Null",
    "Full (rescaled)",
    "Full + Interaction (rescaled)"
  ),
  
  DIC = c(
    res0$dic$dic,
    res_full_scaled$dic$dic,
    res_interaction_scaled$dic$dic
  ),
  
  WAIC = c(
    res0$waic$waic,
    res_full_scaled$waic$waic,
    res_interaction_scaled$waic$waic
  ),
  
  pD = c(
    res0$dic$p.eff,
    res_full_scaled$dic$p.eff,
    res_interaction_scaled$dic$p.eff
  ),
  
  pWAIC = c(
    res0$waic$p.eff,
    res_full_scaled$waic$p.eff,
    res_interaction_scaled$waic$p.eff
  )
)

model_comparison_scaled

# ============================================================
# REVIEWER 1 - COMMENT 2
# REVISED VIF DIAGNOSTIC
# ============================================================

vif_vars_scaled <- c(
  "prop_urban",
  "prop_secondary",
  "prop_higher",
  "prop_middle",
  "prop_richer",
  "prop_richest",
  "prop_currently_married",
  "mean_age",
  "log_pop_density_z",
  "nightlights",
  "conflict_events",
  "hf_density_z"
)

# Complete observations for all revised fixed effects
vif_data_scaled <- model_data_inla %>%
  dplyr::select(all_of(vif_vars_scaled)) %>%
  na.omit()

nrow(vif_data_scaled)


# Fit ordinary linear model for VIF calculation
vif_lm_scaled <- lm(
  prop_urban ~
    prop_secondary +
    prop_higher +
    prop_middle +
    prop_richer +
    prop_richest +
    prop_currently_married +
    mean_age +
    log_pop_density_z +
    nightlights +
    conflict_events +
    hf_density_z,
  data = vif_data_scaled
)

library(car)

vif_model_scaled <- lm(
  prop_urban ~
    prop_secondary +
    prop_higher +
    prop_middle +
    prop_richer +
    prop_richest +
    prop_currently_married +
    mean_age +
    log_pop_density_z +
    nightlights +
    conflict_events +
    hf_density_z,
  data = vif_data_scaled
)

vif(vif_model_scaled)

# ============================================================
# VIF FOR ALL REVISED FIXED-EFFECT COVARIATES
# ============================================================

vif_formula <- as.formula(
  paste(
    "dummy ~",
    paste(vif_vars_scaled, collapse = " + ")
  )
)

vif_data_scaled$dummy <- rnorm(nrow(vif_data_scaled))

vif_lm_scaled <- lm(
  vif_formula,
  data = vif_data_scaled
)

vif_results_scaled <- car::vif(vif_lm_scaled)

vif_results_scaled


# ============================================================
# DIRECT VIF CALCULATION
# ============================================================

vif_results_scaled <- sapply(vif_vars_scaled, function(var) {
  
  other_vars <- setdiff(vif_vars_scaled, var)
  
  form <- as.formula(
    paste(var, "~", paste(other_vars, collapse = " + "))
  )
  
  fit <- lm(form, data = vif_data_scaled)
  
  r2 <- summary(fit)$r.squared
  
  1 / (1 - r2)
})

vif_results_scaled <- data.frame(
  covariate = names(vif_results_scaled),
  VIF = as.numeric(vif_results_scaled)
)

vif_results_scaled <- vif_results_scaled %>%
  arrange(VIF)

vif_results_scaled

saveRDS(
  vif_results_scaled,
  file.path(results_dir, "R1_C2_VIF_results.rds")
)

write.csv(
  vif_results_scaled,
  file.path(results_dir, "R1_C2_VIF_results.csv"),
  row.names = FALSE
)

saveRDS(
  res_full_scaled,
  file.path(results_dir, "res_full_scaled.rds")
)

saveRDS(
  res_interaction_scaled,
  file.path(results_dir, "res_interaction_scaled.rds")
)

saveRDS(
  model_comparison_scaled,
  file.path(results_dir, "model_comparison_scaled.rds")
)


summary(res_full_scaled)
model_comparison_scaled
vif_results_scaled


# ============================================================
# REVISED FOREST PLOT PIPELINE
# Bayesian Poisson Spatio-Temporal Model + Space-Time Interaction
# ============================================================


# ------------------------------------------------------------
#  Extract fixed effects from the REVISED final model
# ------------------------------------------------------------

forest_data <- as.data.frame(
  res_interaction_scaled$summary.fixed
) %>%
  rownames_to_column("covariate") %>%
  
  # Remove intercept
  filter(covariate != "(Intercept)") %>%
  
  # Convert log-relative risks to rate ratios
  mutate(
    RR      = exp(mean),
    RR_low  = exp(`0.025quant`),
    RR_high = exp(`0.975quant`)
  )


# ------------------------------------------------------------
#  Assign manuscript labels
# ------------------------------------------------------------

forest_data <- forest_data %>%
  mutate(
    covariate_label = case_when(
      covariate == "prop_urban"              ~
        "Urban residence (proportion)",
      
      covariate == "prop_secondary"          ~
        "Secondary education (proportion)",
      
      covariate == "prop_higher"             ~
        "Higher education (proportion)",
      
      covariate == "prop_middle"             ~
        "Middle wealth (proportion)",
      
      covariate == "prop_richer"             ~
        "Richer wealth (proportion)",
      
      covariate == "prop_richest"            ~
        "Richest wealth (proportion)",
      
      covariate == "prop_currently_married"  ~
        "Currently married (proportion)",
      
      covariate == "mean_age"                ~
        "Mean age (years)",
      
      covariate == "log_pop_density_z"       ~
        "Log population density (standardized)",
      
      covariate == "nightlights"             ~
        "Night-time lights",
      
      covariate == "conflict_events"         ~
        "Conflict events",
      
      covariate == "hf_density_z"            ~
        "Health-facility density (standardized)",
      
      TRUE ~ covariate
    )
  )


# ------------------------------------------------------------
#  Set the order of covariates
# ------------------------------------------------------------

forest_data <- forest_data %>%
  mutate(
    covariate_label = factor(
      covariate_label,
      levels = rev(c(
        "Urban residence (proportion)",
        "Secondary education (proportion)",
        "Higher education (proportion)",
        "Middle wealth (proportion)",
        "Richer wealth (proportion)",
        "Richest wealth (proportion)",
        "Currently married (proportion)",
        "Mean age (years)",
        "Log population density (standardized)",
        "Night-time lights",
        "Conflict events",
        "Health-facility density (standardized)"
      ))
    )
  )


# ------------------------------------------------------------
#  Check the estimates before plotting
# ------------------------------------------------------------

forest_data %>%
  select(
    covariate,
    RR,
    RR_low,
    RR_high
  ) %>%
  print()


# ------------------------------------------------------------
#  Create revised forest plot
# ------------------------------------------------------------

forest_plot_revised <- ggplot(
  forest_data,
  aes(
    x = RR,
    y = covariate_label
  )
) +
  
  # 95% credible intervals
  geom_errorbarh(
    aes(
      xmin = RR_low,
      xmax = RR_high
    ),
    height = 0.18,
    linewidth = 1.1,
    color = "#A6CEE3"
  ) +
  
  # Posterior mean rate ratio
  geom_point(
    size = 3.2,
    color = "#1F78B4"
  ) +
  
  # Reference line: RR = 1
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey40"
  ) +
  
  # Logarithmic x-axis
  scale_x_log10(
    breaks = c(
      0.25,
      0.5,
      1,
      2,
      4,
      8
    ),
    limits = c(
      0.2,
      8
    )
  ) +
  
  # Axis and labels
  labs(
    x = "Rate Ratio (log scale)",
    y = NULL
  ) +
  
  # Theme
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    axis.text.y = element_text(
      size = 12,
      color = "black"
    ),
    
    axis.text.x = element_text(
      size = 11,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 13,
      color = "black",
      margin = margin(t = 10)
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      linewidth = 0.3
    ),
    
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    
    plot.margin = margin(
      10, 15, 10, 10
    )
  )


# ------------------------------------------------------------
#  Display the plot
# ------------------------------------------------------------

forest_plot_revised


# ------------------------------------------------------------
#  Save high-resolution PNG
# ------------------------------------------------------------

ggsave(
  filename = "Figure_2_Revised_Forest_Plot.png",
  plot = forest_plot_revised,
  width = 9,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)


# ------------------------------------------------------------
#  Save PDF version
# ------------------------------------------------------------

ggsave(
  filename = "Figure_2_Revised_Forest_Plot.pdf",
  plot = forest_plot_revised,
  width = 9,
  height = 6.5,
  units = "in",
  device = cairo_pdf
)


# ------------------------------------------------------------
# Save the underlying forest-plot estimates
# ------------------------------------------------------------

write.csv(
  forest_data %>%
    select(
      covariate,
      RR,
      RR_low,
      RR_high
    ),
  "Figure_2_Revised_Forest_Plot_Data.csv",
  row.names = FALSE
)


# ============================================================
# REVIEWER 1 — COMMENT 3
# SENSITIVITY ANALYSIS:
# EXCLUDING HISTORICAL HEALTH-FACILITY DENSITY
# ============================================================


# ------------------------------------------------------------
#  Confirm the required variables are available
# ------------------------------------------------------------

required_vars <- c(
  "births",
  "exposure",
  "prop_urban",
  "prop_secondary",
  "prop_higher",
  "prop_middle",
  "prop_richer",
  "prop_richest",
  "prop_currently_married",
  "mean_age",
  "log_pop_density_z",
  "nightlights",
  "conflict_events",
  "state_id",
  "year_id",
  "age_id",
  "st_id"
)

missing_vars <- setdiff(
  required_vars,
  names(model_data_inla)
)

if (length(missing_vars) > 0) {
  stop(
    paste(
      "Missing variables:",
      paste(missing_vars, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
#  Check the state-year interaction structure
# ------------------------------------------------------------

cat(
  "Number of state-year interaction levels:",
  length(unique(model_data_inla$st_id)),
  "\n"
)


# ------------------------------------------------------------
#  Fit sensitivity model WITHOUT health-facility density
# ------------------------------------------------------------

res_interaction_no_hf <- inla(
  
  births ~
    prop_urban +
    prop_secondary +
    prop_higher +
    prop_middle +
    prop_richer +
    prop_richest +
    prop_currently_married +
    mean_age +
    log_pop_density_z +
    nightlights +
    conflict_events +
    
    f(
      state_id,
      model = "besag",
      graph = g,
      scale.model = TRUE
    ) +
    
    f(
      year_id,
      model = "rw2"
    ) +
    
    f(
      age_id,
      model = "iid"
    ) +
    
    f(
      st_id,
      model = "iid"
    ),
  
  family = "poisson",
  
  data = model_data_inla,
  
  E = exposure,
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)


# ------------------------------------------------------------
#  Display model fit
# ------------------------------------------------------------

cat("\n============================================\n")
cat("SENSITIVITY MODEL — WITHOUT HEALTH FACILITY\n")
cat("============================================\n")

cat(
  "DIC:",
  res_interaction_no_hf$dic$dic,
  "\n"
)

cat(
  "WAIC:",
  res_interaction_no_hf$waic$waic,
  "\n"
)


# ------------------------------------------------------------
#  Extract fixed effects and calculate RRs
# ------------------------------------------------------------

sens_fixed <- as.data.frame(
  res_interaction_no_hf$summary.fixed
) %>%
  tibble::rownames_to_column("covariate") %>%
  mutate(
    RR = exp(mean),
    RR_low = exp(`0.025quant`),
    RR_high = exp(`0.975quant`)
  )


# ------------------------------------------------------------
#  Print sensitivity-model estimates
# ------------------------------------------------------------

sens_fixed %>%
  select(
    covariate,
    mean,
    RR,
    RR_low,
    RR_high
  ) %>%
  print()


# ------------------------------------------------------------
#  Extract primary-model estimates for comparison
# ------------------------------------------------------------

primary_fixed <- as.data.frame(
  res_interaction_scaled$summary.fixed
) %>%
  tibble::rownames_to_column("covariate") %>%
  mutate(
    RR_primary = exp(mean),
    RR_low_primary = exp(`0.025quant`),
    RR_high_primary = exp(`0.975quant`)
  ) %>%
  select(
    covariate,
    RR_primary,
    RR_low_primary,
    RR_high_primary
  )


# ------------------------------------------------------------
#  Compare common fixed effects
# ------------------------------------------------------------

sensitivity_comparison <- primary_fixed %>%
  inner_join(
    sens_fixed %>%
      select(
        covariate,
        RR_sensitivity = RR,
        RR_low_sensitivity = RR_low,
        RR_high_sensitivity = RR_high
      ),
    by = "covariate"
  ) %>%
  filter(
    covariate != "(Intercept)"
  ) %>%
  mutate(
    RR_ratio = RR_sensitivity / RR_primary
  )


# ------------------------------------------------------------
#  Print comparison
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PRIMARY MODEL VS SENSITIVITY MODEL\n")
cat("============================================\n")

print(
  sensitivity_comparison
)


# ------------------------------------------------------------
#  Compact comparison table
# ------------------------------------------------------------

sensitivity_comparison %>%
  select(
    covariate,
    RR_primary,
    RR_low_primary,
    RR_high_primary,
    RR_sensitivity,
    RR_low_sensitivity,
    RR_high_sensitivity
  ) %>%
  print()


# ------------------------------------------------------------
#  Save sensitivity model
# ------------------------------------------------------------

saveRDS(
  res_interaction_no_hf,
  file.path(
    results_dir,
    "R1_C3_sensitivity_no_health_facility.rds"
  )
)


# ------------------------------------------------------------
#  Save comparison results
# ------------------------------------------------------------

saveRDS(
  sensitivity_comparison,
  file.path(
    results_dir,
    "R1_C3_sensitivity_comparison.rds"
  )
)

write.csv(
  sensitivity_comparison,
  file.path(
    results_dir,
    "R1_C3_sensitivity_comparison.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# Create a simple model-fit comparison
# ------------------------------------------------------------

model_fit_comparison <- data.frame(
  Model = c(
    "Revised final model",
    "Sensitivity: excluding health-facility density"
  ),
  
  DIC = c(
    res_interaction_scaled$dic$dic,
    res_interaction_no_hf$dic$dic
  ),
  
  WAIC = c(
    res_interaction_scaled$waic$waic,
    res_interaction_no_hf$waic$waic
  )
)

print(model_fit_comparison)

write.csv(
  model_fit_comparison,
  file.path(
    results_dir,
    "R1_C3_model_fit_comparison.csv"
  ),
  row.names = FALSE
)

# ============================================================
# END OF REVIEWER 1 COMMENT 3 ANALYSIS
# ============================================================


# ============================================================
# REVIEWER 1 — PIT DIAGNOSTIC
# Inspect PIT distribution from revised final model
# ============================================================


# ------------------------------------------------------------
#  Extract PIT values
# ------------------------------------------------------------

pit_revised <- res_interaction_scaled$cpo$pit

# Remove missing/non-finite values
pit_revised <- pit_revised[
  is.finite(pit_revised)
]


# ------------------------------------------------------------
#  Basic summary
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PIT SUMMARY — REVISED FINAL MODEL\n")
cat("============================================\n")

cat(
  "Number of PIT values:",
  length(pit_revised),
  "\n"
)

cat(
  "Mean:",
  mean(pit_revised),
  "\n"
)

cat(
  "SD:",
  sd(pit_revised),
  "\n"
)

cat(
  "Median:",
  median(pit_revised),
  "\n"
)

cat(
  "Minimum:",
  min(pit_revised),
  "\n"
)

cat(
  "Maximum:",
  max(pit_revised),
  "\n"
)


# ------------------------------------------------------------
#  Proportion near the boundaries
# ------------------------------------------------------------

boundary_summary <- data.frame(
  Region = c(
    "Lower boundary (<0.05)",
    "Lower tail (0.05–0.10)",
    "Central region (0.10–0.90)",
    "Upper tail (0.90–0.95)",
    "Upper boundary (>0.95)"
  ),
  
  Proportion = c(
    mean(pit_revised < 0.05),
    mean(pit_revised >= 0.05 & pit_revised < 0.10),
    mean(pit_revised >= 0.10 & pit_revised <= 0.90),
    mean(pit_revised > 0.90 & pit_revised <= 0.95),
    mean(pit_revised > 0.95)
  )
)

print(boundary_summary)


# ------------------------------------------------------------
#  Kolmogorov-Smirnov test against Uniform(0,1)
# ------------------------------------------------------------

pit_uniform_test <- ks.test(
  pit_revised,
  "punif"
)

print(pit_uniform_test)


# ------------------------------------------------------------
#  Plot revised PIT histogram
# ------------------------------------------------------------

pit_plot <- ggplot(
  data.frame(PIT = pit_revised),
  aes(x = PIT)
) +
  
  geom_histogram(
    bins = 10,
    boundary = 0,
    closed = "left",
    linewidth = 0.3,
    fill = "#1F78B4",
    color = "white"
  ) +
  
  geom_hline(
    yintercept = length(pit_revised) / 10,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  
  labs(
    x = "Probability Integral Transform (PIT)",
    y = "Frequency"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid.minor = element_blank()
  )

pit_plot


mean(pit_revised)
sd(pit_revised)
boundary_summary
pit_uniform_test

##########################################################
### Fit the Negative Binomial version of the final model
##########################################################

res_interaction_nb <- inla(
  births ~
    prop_urban +
    prop_secondary +
    prop_higher +
    prop_middle +
    prop_richer +
    prop_richest +
    prop_currently_married +
    mean_age +
    log_pop_density_z +
    nightlights +
    conflict_events +
    hf_density_z +
    f(state_id, model = "besag",
      graph = g,
      scale.model = TRUE) +
    f(year_id, model = "rw2") +
    f(age_id, model = "iid") +
    f(st_id, model = "iid"),
  
  family = "nbinomial",
  
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

res_interaction_nb$summary.fixed
res_interaction_nb$dic$dic
res_interaction_nb$waic$waic
res_interaction_nb$summary.hyperpar


pit_nb <- res_interaction_nb$cpo$pit
pit_nb <- pit_nb[is.finite(pit_nb)]

mean(pit_nb)
sd(pit_nb)
median(pit_nb)
min(pit_nb)
max(pit_nb)

boundary_summary_nb <- data.frame(
  Region = c(
    "Lower boundary (<0.05)",
    "Lower tail (0.05–0.10)",
    "Central region (0.10–0.90)",
    "Upper tail (0.90–0.95)",
    "Upper boundary (>0.95)"
  ),
  Proportion = c(
    mean(pit_nb < 0.05),
    mean(pit_nb >= 0.05 & pit_nb < 0.10),
    mean(pit_nb >= 0.10 & pit_nb <= 0.90),
    mean(pit_nb > 0.90 & pit_nb <= 0.95),
    mean(pit_nb > 0.95)
  )
)

boundary_summary_nb
ks.test(pit_nb, "punif")


data.frame(
  Model = c(
    "Poisson + space-time interaction",
    "Negative Binomial + space-time interaction"
  ),
  DIC = c(
    res_interaction_scaled$dic$dic,
    res_interaction_nb$dic$dic
  ),
  WAIC = c(
    res_interaction_scaled$waic$waic,
    res_interaction_nb$waic$waic
  )
)

######### More check

nb_rr <- data.frame(
  Covariate = rownames(res_interaction_nb$summary.fixed),
  Mean = res_interaction_nb$summary.fixed$mean,
  Lower = res_interaction_nb$summary.fixed$`0.025quant`,
  Upper = res_interaction_nb$summary.fixed$`0.975quant`
)

nb_rr <- nb_rr[nb_rr$Covariate != "(Intercept)", ]

nb_rr$RR <- exp(nb_rr$Mean)
nb_rr$RR_lower <- exp(nb_rr$Lower)
nb_rr$RR_upper <- exp(nb_rr$Upper)

nb_rr


# Pearson dispersion ratio for the Negative Binomial model

mu_nb <- res_interaction_nb$summary.fitted.values$mean

pearson_res_nb <- (
  model_data_inla$births - mu_nb
) / sqrt(
  mu_nb + (mu_nb^2 / res_interaction_nb$summary.hyperpar$mean[
    grep("size", rownames(res_interaction_nb$summary.hyperpar),
         ignore.case = TRUE)
  ])
)

pearson_dispersion_nb <- mean(pearson_res_nb^2, na.rm = TRUE)

pearson_dispersion_nb

############### END OF REVIEWER 1 COMMENTS ###################
#############################################################

# ============================================================
# REVIEWER 2 – COMMENT 1
# LEAVE-ONE-STATE-OUT VALIDATION
#
# Predictions are converted from the Poisson linear predictor
# to expected birth counts using:
#
# predicted births = exp(eta) * exposure
# ============================================================


# ------------------------------------------------------------
# 1. Validation data
# ------------------------------------------------------------

validation_data <- model_data_inla

validation_data$births_observed <- validation_data$births

# ------------------------------------------------------------
#  Exact final model specification
# ------------------------------------------------------------

formula_loso <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE
  ) +
  f(year_id, model = "rw2") +
  f(age_id, model = "iid") +
  f(st_id, model = "iid")

# ------------------------------------------------------------
#  Structural checks
# ------------------------------------------------------------

stopifnot(nrow(validation_data) == 1290)
stopifnot(length(unique(validation_data$state_id)) == 37)
stopifnot(length(unique(validation_data$st_id)) == 185)
stopifnot(g$n == 37)
stopifnot(g$cc$n == 1)

# ------------------------------------------------------------
#  State lookup
# ------------------------------------------------------------

state_lookup <- validation_data %>%
  select(state_id, REGCODE, REGNAME) %>%
  distinct() %>%
  arrange(state_id)

# ------------------------------------------------------------
#  Storage
# ------------------------------------------------------------

states <- sort(unique(validation_data$state_id))

loso_results <- vector("list", length(states))

all_observed <- numeric(0)
all_predicted <- numeric(0)

# ------------------------------------------------------------
#  37-state LOSO validation
# ------------------------------------------------------------

for (j in seq_along(states)) {
  
  s <- states[j]
  
  state_name <- state_lookup$REGNAME[
    state_lookup$state_id == s
  ]
  
  cat("\n----------------------------------------\n")
  cat(
    "Fold", j, "of", length(states),
    "| State", s,
    "|", state_name, "\n"
  )
  
  # ----------------------------------------------------------
  # Create fold
  # ----------------------------------------------------------
  
  fold_data <- validation_data
  
  holdout <- fold_data$state_id == s
  
  # Hide all outcomes from the held-out state
  fold_data$births[holdout] <- NA
  
  # ----------------------------------------------------------
  # Fit model
  # ----------------------------------------------------------
  
  fit_fold <- inla(
    formula_loso,
    family = "poisson",
    data = fold_data,
    E = fold_data$exposure,
    
    control.predictor = list(
      compute = TRUE
    ),
    
    control.compute = list(
      dic = FALSE,
      waic = FALSE,
      cpo = FALSE
    )
  )
  
  # ----------------------------------------------------------
  # Extract posterior mean of linear predictor
  # ----------------------------------------------------------
  
  eta_hat <- fit_fold$summary.linear.predictor$mean[
    holdout
  ]
  
  # ----------------------------------------------------------
  # Convert to expected birth counts
  #
  # Poisson mean:
  # lambda_i = E_i * exp(eta_i)
  # ----------------------------------------------------------
  
  predicted <- exp(eta_hat) *
    validation_data$exposure[holdout]
  
  observed <- validation_data$births_observed[
    holdout
  ]
  
  # ----------------------------------------------------------
  # Validation checks
  # ----------------------------------------------------------
  
  if (length(observed) != length(predicted)) {
    
    stop(
      paste(
        "Prediction length mismatch for",
        state_name
      )
    )
  }
  
  if (anyNA(predicted)) {
    
    stop(
      paste(
        "Missing predictions for",
        state_name
      )
    )
  }
  
  if (any(!is.finite(predicted))) {
    
    stop(
      paste(
        "Non-finite predictions for",
        state_name
      )
    )
  }
  
  # Predictions must be non-negative
  if (any(predicted < 0)) {
    
    stop(
      paste(
        "Negative predicted counts for",
        state_name
      )
    )
  }
  
  # ----------------------------------------------------------
  # RMSE
  # ----------------------------------------------------------
  
  rmse <- sqrt(
    mean(
      (observed - predicted)^2
    )
  )
  
  # ----------------------------------------------------------
  # MAE
  # ----------------------------------------------------------
  
  mae <- mean(
    abs(observed - predicted)
  )
  
  # ----------------------------------------------------------
  # Store state results
  # ----------------------------------------------------------
  
  loso_results[[j]] <- data.frame(
    state_id = s,
    REGCODE = state_lookup$REGCODE[
      state_lookup$state_id == s
    ],
    REGNAME = state_name,
    n_test = length(observed),
    RMSE = rmse,
    MAE = mae
  )
  
  # ----------------------------------------------------------
  # Store pooled predictions
  # ----------------------------------------------------------
  
  all_observed <- c(
    all_observed,
    observed
  )
  
  all_predicted <- c(
    all_predicted,
    predicted
  )
  
  cat(
    "n =", length(observed),
    "| RMSE =", round(rmse, 4),
    "| MAE =", round(mae, 4),
    "| Mean predicted =",
    round(mean(predicted), 2),
    "\n"
  )
}

# ============================================================
#  Combine state-level results
# ============================================================

loso_results_df <- bind_rows(
  loso_results
)

# ============================================================
#  Overall predictive performance
# ============================================================

overall_RMSE <- sqrt(
  mean(
    (all_observed - all_predicted)^2
  )
)

overall_MAE <- mean(
  abs(
    all_observed - all_predicted
  )
)

mean_state_RMSE <- mean(
  loso_results_df$RMSE
)

mean_state_MAE <- mean(
  loso_results_df$MAE
)

median_state_RMSE <- median(
  loso_results_df$RMSE
)

median_state_MAE <- median(
  loso_results_df$MAE
)

# ============================================================
#  Summary
# ============================================================

loso_summary <- data.frame(
  states = length(states),
  observations = length(all_observed),
  overall_RMSE = overall_RMSE,
  overall_MAE = overall_MAE,
  mean_state_RMSE = mean_state_RMSE,
  mean_state_MAE = mean_state_MAE,
  median_state_RMSE = median_state_RMSE,
  median_state_MAE = median_state_MAE
)

# ============================================================
#  Print summary
# ============================================================

cat("\n========================================\n")
cat("CORRECTED LOSO VALIDATION SUMMARY\n")
cat("========================================\n")

print(loso_summary)

cat("\n========================================\n")
cat("STATE-LEVEL RESULTS\n")
cat("========================================\n")

print(
  as.data.frame(loso_results_df),
  row.names = FALSE
)

# ============================================================
#  Basic prediction diagnostics
# ============================================================

mean_observed <- mean(all_observed)
mean_predicted <- mean(all_predicted)

median_observed <- median(all_observed)
median_predicted <- median(all_predicted)

bias <- mean(
  all_predicted - all_observed
)

correlation <- cor(
  all_observed,
  all_predicted,
  method = "pearson"
)

cat("\n========================================\n")
cat("PREDICTION DIAGNOSTICS\n")
cat("========================================\n")

cat(
  "Mean observed births  :",
  mean_observed, "\n"
)

cat(
  "Mean predicted births :",
  mean_predicted, "\n"
)

cat(
  "Median observed births:",
  median_observed, "\n"
)

cat(
  "Median predicted births:",
  median_predicted, "\n"
)

cat(
  "Mean prediction bias  :",
  bias, "\n"
)

cat(
  "Pearson correlation   :",
  correlation, "\n"
)

# ============================================================
#  Save results
# ============================================================

write.csv(
  loso_results_df,
  "Reviewer2_LOSO_corrected_state_results.csv",
  row.names = FALSE
)

write.csv(
  loso_summary,
  "Reviewer2_LOSO_corrected_summary.csv",
  row.names = FALSE
)

loso_predictions <- data.frame(
  observed = all_observed,
  predicted = all_predicted
)

write.csv(
  loso_predictions,
  "Reviewer2_LOSO_corrected_predictions.csv",
  row.names = FALSE
)

cat(
  "\nCorrected LOSO validation completed successfully.\n"
)



# ============================================================
# CORRECT OBSERVED VS LOSO-PREDICTED BIRTH COUNTS
# ============================================================

# Create data frame
loso_plot_data <- data.frame(
  observed = all_observed,
  predicted = all_predicted
)

p_loso <- ggplot(
  loso_plot_data,
  aes(x = observed, y = predicted)
) +
  geom_point(
    color = "#00B4D8",
    alpha = 0.65,
    size = 2.2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey30"
  ) +
  labs(
    #title = "Leave-One-State-Out Predictive Validation",
    x = "Observed births",
    y = "LOSO predicted births"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    )
  )

p_loso

#### Or use this

loso_plot_data <- data.frame(
  observed = all_observed,
  predicted = all_predicted
)

r_value <- cor(
  loso_plot_data$observed,
  loso_plot_data$predicted,
  method = "pearson"
)

p_loso <- ggplot(
  loso_plot_data,
  aes(x = observed, y = predicted)
) +
  geom_point(
    color = "#00B4D8",
    alpha = 0.65,
    size = 2.2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey30"
  ) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = paste0(
      "Pearson r = ",
      sprintf("%.3f", r_value)
    ),
    hjust = 1.1,
    vjust = -1.2,
    size = 5
  ) +
  labs(
    x = "Observed births",
    y = "LOSO predicted births"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold")
  )

p_loso


######################################################
### Reviewer 2: Comment 6
######################################################

software_info <- readRDS(
  file.path(results_dir, "software_info.rds")
)

software_info

res_interaction_scaled_saved <- readRDS(
  "Model_Result/res_interaction_scaled.rds"
)

res_interaction_scaled_saved$cpu.used

names(res_interaction_scaled_saved)

res_interaction_scaled_saved$misc


# ============================================================
# REVIEWER 2 — COMMENT 7
# PRIOR SENSITIVITY ANALYSIS
# Scaled final model
# ============================================================

# ------------------------------------------------------------
# 1. Confirm required variables
# ------------------------------------------------------------

required_vars_scaled <- c(
  "births",
  "exposure",
  "prop_urban",
  "prop_secondary",
  "prop_higher",
  "prop_middle",
  "prop_richer",
  "prop_richest",
  "prop_currently_married",
  "mean_age",
  "log_pop_density_z",
  "nightlights",
  "conflict_events",
  "hf_density_z",
  "state_id",
  "year_id",
  "age_id",
  "st_id"
)

missing_vars_scaled <- setdiff(
  required_vars_scaled,
  names(model_data_inla)
)

if (length(missing_vars_scaled) > 0) {
  stop(
    paste(
      "Missing variables:",
      paste(missing_vars_scaled, collapse = ", ")
    )
  )
}

cat("All required scaled-model variables are available.\n")
cat("Number of observations:", nrow(model_data_inla), "\n")
cat("Number of states:", length(unique(model_data_inla$state_id)), "\n")
cat("Number of state-year interaction levels:",
    length(unique(model_data_inla$st_id)), "\n")

# ------------------------------------------------------------
#  Exact scaled final-model formula
# ------------------------------------------------------------

formula_PC_scaled <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  
  # Spatial structured effect
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE
  ) +
  
  # Temporal effect
  f(
    year_id,
    model = "rw2"
  ) +
  
  # Age effect
  f(
    age_id,
    model = "iid"
  ) +
  
  # Space-time interaction
  f(
    st_id,
    model = "iid"
  )

# ------------------------------------------------------------
#  Strong smoothing prior
#    PC prior: P(sd > 1) = 0.01
# ------------------------------------------------------------

formula_PC_strong_scaled <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE,
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)
      )
    )
  ) +
  
  f(
    year_id,
    model = "rw2",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)
      )
    )
  ) +
  
  f(
    age_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)
      )
    )
  ) +
  
  f(
    st_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)
      )
    )
  )

res_PC_strong_scaled <- inla(
  formula_PC_strong_scaled,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

cat("Strong-prior model completed.\n")

# ------------------------------------------------------------
# Moderate smoothing prior
#    PC prior: P(sd > 2) = 0.01
# ------------------------------------------------------------

formula_PC_moderate_scaled <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE,
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  ) +
  
  f(
    year_id,
    model = "rw2",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  ) +
  
  f(
    age_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  ) +
  
  f(
    st_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  )

res_PC_moderate_scaled <- inla(
  formula_PC_moderate_scaled,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

cat("Moderate-prior model completed.\n")

# ------------------------------------------------------------
#  Weak smoothing prior
#    PC prior: P(sd > 5) = 0.01
# ------------------------------------------------------------

formula_PC_weak_scaled <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  log_pop_density_z +
  nightlights +
  conflict_events +
  hf_density_z +
  
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE,
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(5, 0.01)
      )
    )
  ) +
  
  f(
    year_id,
    model = "rw2",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(5, 0.01)
      )
    )
  ) +
  
  f(
    age_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(5, 0.01)
      )
    )
  ) +
  
  f(
    st_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(5, 0.01)
      )
    )
  )

res_PC_weak_scaled <- inla(
  formula_PC_weak_scaled,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

cat("Weak-prior model completed.\n")

# ------------------------------------------------------------
#  Save fitted sensitivity models
# ------------------------------------------------------------

dir.create("Model_Result", showWarnings = FALSE)

saveRDS(
  res_PC_strong_scaled,
  "Model_Result/R2_C7_PC_strong_scaled.rds"
)

saveRDS(
  res_PC_moderate_scaled,
  "Model_Result/R2_C7_PC_moderate_scaled.rds"
)

saveRDS(
  res_PC_weak_scaled,
  "Model_Result/R2_C7_PC_weak_scaled.rds"
)

cat("All three scaled sensitivity models saved successfully.\n")

# ------------------------------------------------------------
#  Model fit comparison
# ------------------------------------------------------------

prior_fit_comparison <- data.frame(
  
  Model = c(
    "Scaled final model",
    "Strong PC prior",
    "Moderate PC prior",
    "Weak PC prior"
  ),
  
  DIC = c(
    res_interaction_scaled$dic$dic,
    res_PC_strong_scaled$dic$dic,
    res_PC_moderate_scaled$dic$dic,
    res_PC_weak_scaled$dic$dic
  ),
  
  WAIC = c(
    res_interaction_scaled$waic$waic,
    res_PC_strong_scaled$waic$waic,
    res_PC_moderate_scaled$waic$waic,
    res_PC_weak_scaled$waic$waic
  ),
  
  pD = c(
    res_interaction_scaled$dic$p.eff,
    res_PC_strong_scaled$dic$p.eff,
    res_PC_moderate_scaled$dic$p.eff,
    res_PC_weak_scaled$dic$p.eff
  ),
  
  pWAIC = c(
    res_interaction_scaled$waic$p.eff,
    res_PC_strong_scaled$waic$p.eff,
    res_PC_moderate_scaled$waic$p.eff,
    res_PC_weak_scaled$waic$p.eff
  )
)

prior_fit_comparison

# ------------------------------------------------------------
#  Posterior hyperparameters
# ------------------------------------------------------------

summary(res_PC_strong_scaled)$hyperpar

summary(res_PC_moderate_scaled)$hyperpar

summary(res_PC_weak_scaled)$hyperpar

# ------------------------------------------------------------
#  Check random-effect names
# ------------------------------------------------------------

names(res_PC_strong_scaled$summary.random)

names(res_PC_moderate_scaled$summary.random)

names(res_PC_weak_scaled$summary.random)

names(res_PC_strong_scaled$marginals.random)

# ------------------------------------------------------------
#  Fixed-effect RR extraction
# ------------------------------------------------------------

extract_RR <- function(model, model_name) {
  
  out <- as.data.frame(model$summary.fixed)
  
  out$covariate <- rownames(out)
  
  out %>%
    mutate(
      Model = model_name,
      RR = exp(mean),
      RR_low = exp(`0.025quant`),
      RR_high = exp(`0.975quant`)
    ) %>%
    select(
      Model,
      covariate,
      RR,
      RR_low,
      RR_high
    )
}

rr_strong <- extract_RR(
  res_PC_strong_scaled,
  "Strong PC prior"
)

rr_moderate <- extract_RR(
  res_PC_moderate_scaled,
  "Moderate PC prior"
)

rr_weak <- extract_RR(
  res_PC_weak_scaled,
  "Weak PC prior"
)

rr_primary <- extract_RR(
  res_interaction_scaled,
  "Scaled final model"
)

RR_sensitivity_all <- bind_rows(
  rr_primary,
  rr_strong,
  rr_moderate,
  rr_weak
)

RR_sensitivity_all

#------------------------------------------------------------
# Function to transform posterior precision -> posterior variance
#------------------------------------------------------------

extract_variance_posterior <- function(model, model_name) {
  
  hyper_names <- c(
    "Precision for state_id",
    "Precision for year_id",
    "Precision for age_id",
    "Precision for st_id"
  )
  
  results <- lapply(hyper_names, function(hname) {
    
    marg <- model$marginals.hyperpar[[hname]]
    
    if (is.null(marg)) {
      return(NULL)
    }
    
    # Transform precision x to variance = 1/x
    variance_marg <- inla.tmarginal(
      function(x) 1 / x,
      marg
    )
    
    stats <- inla.zmarginal(
      variance_marg,
      silent = TRUE
    )
    
    data.frame(
      Model = model_name,
      Random_effect = hname,
      Variance_mean = stats$mean,
      Variance_sd = stats$sd,
      Variance_median = stats$quant0.5,
      Variance_low = stats$quant0.025,
      Variance_high = stats$quant0.975
    )
  })
  
  bind_rows(results)
}


#------------------------------------------------------------
# Extract for all four models
#------------------------------------------------------------

variance_primary <- extract_variance_posterior(
  res_interaction_scaled,
  "Scaled final model"
)

variance_strong <- extract_variance_posterior(
  res_PC_strong_scaled,
  "Strong PC prior"
)

variance_moderate <- extract_variance_posterior(
  res_PC_moderate_scaled,
  "Moderate PC prior"
)

variance_weak <- extract_variance_posterior(
  res_PC_weak_scaled,
  "Weak PC prior"
)


posterior_variance_all <- bind_rows(
  variance_primary,
  variance_strong,
  variance_moderate,
  variance_weak
)

posterior_variance_all

##### Compare the posterior variances

posterior_variance_all %>%
  mutate(
    Random_effect = recode(
      Random_effect,
      "Precision for state_id" = "Spatial",
      "Precision for year_id" = "Temporal",
      "Precision for age_id" = "Age",
      "Precision for st_id" = "Space-time interaction"
    )
  )

#------------------------------------------------------------
# CPO/PIT sensitivity summary
#------------------------------------------------------------

extract_cpo_pit <- function(model, model_name) {
  
  cpo <- model$cpo$cpo
  pit <- model$cpo$pit
  
  valid_cpo <- is.finite(cpo) & cpo > 0
  valid_pit <- is.finite(pit)
  
  neg_log_cpo <- -log(cpo[valid_cpo])
  pit_valid <- pit[valid_pit]
  
  data.frame(
    Model = model_name,
    Mean_neg_log_CPO = mean(neg_log_cpo),
    Median_neg_log_CPO = median(neg_log_cpo),
    PIT_mean = mean(pit_valid),
    PIT_sd = sd(pit_valid),
    PIT_central_10_90 = mean(
      pit_valid >= 0.10 & pit_valid <= 0.90
    ),
    PIT_extreme = mean(
      pit_valid < 0.05 | pit_valid > 0.95
    )
  )
}


cpo_pit_sensitivity <- bind_rows(
  extract_cpo_pit(
    res_interaction_scaled,
    "Scaled final model"
  ),
  extract_cpo_pit(
    res_PC_strong_scaled,
    "Strong PC prior"
  ),
  extract_cpo_pit(
    res_PC_moderate_scaled,
    "Moderate PC prior"
  ),
  extract_cpo_pit(
    res_PC_weak_scaled,
    "Weak PC prior"
  )
)

cpo_pit_sensitivity

#------------------------------------------------------------
# Quantify maximum change in RR relative to primary model
#------------------------------------------------------------

rr_primary_compare <- rr_primary %>%
  select(covariate, RR_primary = RR)

rr_sensitivity_change <- RR_sensitivity_all %>%
  filter(Model != "Scaled final model") %>%
  left_join(
    rr_primary_compare,
    by = "covariate"
  ) %>%
  mutate(
    Absolute_change = abs(RR - RR_primary),
    Percent_change = 100 * abs(RR - RR_primary) / RR_primary
  )

rr_sensitivity_change

rr_sensitivity_change %>%
  group_by(Model) %>%
  summarise(
    Maximum_absolute_change = max(Absolute_change),
    Maximum_percent_change = max(Percent_change)
  )


write.csv(
  posterior_variance_all,
  "Model_Result/R2_C7_posterior_variances.csv",
  row.names = FALSE
)

write.csv(
  cpo_pit_sensitivity,
  "Model_Result/R2_C7_CPO_PIT_sensitivity.csv",
  row.names = FALSE
)

write.csv(
  rr_sensitivity_change,
  "Model_Result/R2_C7_fixed_effect_sensitivity.csv",
  row.names = FALSE
)

write.csv(
  prior_fit_comparison,
  "Model_Result/R2_C7_model_fit_comparison.csv",
  row.names = FALSE
)



##################################################################
### SECOND REVISION - REVIEWER 2 COMMENTS
##################################################################

## Reviewer 2 – Comment 1: Predictive validation and consistency of 
## the modelling dataset


cat("Rows in model_data:", nrow(model_data), "\n")
cat("Rows in model_data_inla:", nrow(model_data_inla), "\n")

cat(
  "Unique state-age-year combinations in model_data_inla:",
  nrow(unique(
    model_data_inla[, c("state_id", "age_id", "year_id")]
  )),
  "\n"
)

cat(
  "Rows with missing outcomes:",
  sum(is.na(model_data_inla$births)),
  "\n"
)

## Next, check missing covariates in the 1,290-row dataset

# Variables used in the final model
model_vars <- c(
  "births",
  "exposure",
  "prop_urban",
  "prop_secondary",
  "prop_higher",
  "prop_middle",
  "prop_richer",
  "prop_richest",
  "prop_currently_married",
  "mean_age",
  "pop_density",
  "nightlights",
  "conflict_events",
  "hf_density"
)

# Check that all requested variables exist
setdiff(model_vars, names(model_data_inla))

# Count missing values in each variable
missing_by_variable <- sapply(
  model_data_inla[, model_vars, drop = FALSE],
  function(x) sum(is.na(x))
)

print(missing_by_variable)

# Count complete and incomplete rows
complete_rows <- complete.cases(
  model_data_inla[, model_vars, drop = FALSE]
)

cat("Total rows:", nrow(model_data_inla), "\n")
cat("Complete rows:", sum(complete_rows), "\n")
cat("Incomplete rows:", sum(!complete_rows), "\n")

# Check missing values in the original model_data as well
cat(
  "Complete rows in model_data:",
  sum(complete.cases(model_data[, model_vars, drop = FALSE])),
  "\n"
)


