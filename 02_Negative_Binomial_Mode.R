# ============================================================
# FERTILITY PROJECT — NEGATIVE BINOMIAL REVISION
# Candidate primary spatio-temporal model
# ============================================================

# ------------------------------------------------------------
#  Project directory
# ------------------------------------------------------------

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

results_dir <- file.path(getwd(), "Model_Result")

if (!dir.exists(results_dir)) {
  stop("Model_Result folder not found. Check the project directory.")
}

nb_results_dir <- file.path(results_dir, "Negative_Binomial")

dir.create(
  nb_results_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# Load packages
library(INLA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(purrr)
library(readr)



# ------------------------------------------------------------
#  Load the original saved data and spatial objects
# ------------------------------------------------------------

model_data <- readRDS(
  file.path(results_dir, "model_data.rds")
)

model_data_inla <- readRDS(
  file.path(results_dir, "model_data_inla.rds")
)

nga_states <- readRDS(
  file.path(results_dir, "nga_states.rds")
)

g <- readRDS(
  file.path(results_dir, "adjacency_graph.rds")
)

# ------------------------------------------------------------
#  Recreate the variables from the previous revision script
# ------------------------------------------------------------

# Health-facility density: standardize the original variable
if (!"hf_density_z" %in% names(model_data_inla)) {
  
  if (!"hf_density" %in% names(model_data_inla)) {
    stop("hf_density is absent; cannot recreate hf_density_z.")
  }
  
  model_data_inla$hf_density_z <- as.numeric(
    scale(model_data_inla$hf_density)
  )
}

# Population density: log-transform, then standardize
if (!"log_pop_density_z" %in% names(model_data_inla)) {
  
  if (!"pop_density" %in% names(model_data_inla)) {
    stop("pop_density is absent; cannot recreate log_pop_density_z.")
  }
  
  if (any(
    model_data_inla$pop_density <= 0,
    na.rm = TRUE
  )) {
    stop(
      "Population density contains zero or negative values. ",
      "Check the original data before applying log()."
    )
  }
  
  model_data_inla$log_pop_density <- log(
    model_data_inla$pop_density
  )
  
  model_data_inla$log_pop_density_z <- as.numeric(
    scale(model_data_inla$log_pop_density)
  )
}

# State-year interaction index
if (!"st_id" %in% names(model_data_inla)) {
  
  if (!all(c("state_id", "year_id") %in%
           names(model_data_inla))) {
    stop("state_id or year_id is absent.")
  }
  
  model_data_inla <- model_data_inla %>%
    mutate(
      st_id = as.integer(
        interaction(state_id, year_id, drop = TRUE)
      )
    )
}

# ------------------------------------------------------------
#  Check the reconstructed variables
# ------------------------------------------------------------

cat("Number of observations:",
    nrow(model_data_inla), "\n")

cat("Number of states:",
    length(unique(model_data_inla$state_id)), "\n")

cat("Number of survey years:",
    length(unique(model_data_inla$year_id)), "\n")

cat("Number of age groups:",
    length(unique(model_data_inla$age_id)), "\n")

cat("Number of state-year interaction levels:",
    length(unique(model_data_inla$st_id)), "\n")

cat("\nMissing values in reconstructed variables:\n")

print(
  sapply(
    model_data_inla[
      ,
      c("log_pop_density_z", "hf_density_z", "st_id"),
      drop = FALSE
    ],
    function(x) sum(is.na(x))
  )
)

# ------------------------------------------------------------
#  Check the complete set of model variables
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
  "hf_density_z",
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
    "Still missing required variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

if (nrow(model_data_inla) != 1290) {
  stop("Unexpected number of observations.")
}

if (anyNA(model_data_inla[, required_vars])) {
  stop("Missing values detected in the required model variables.")
}

if (any(model_data_inla$births < 0) ||
    any(model_data_inla$births !=
        floor(model_data_inla$births))) {
  stop("Birth counts must be non-negative integers.")
}

if (any(model_data_inla$exposure <= 0)) {
  stop("Exposure must be strictly positive.")
}

stopifnot(
  length(unique(model_data_inla$state_id)) == 37,
  length(unique(model_data_inla$year_id)) == 5,
  length(unique(model_data_inla$age_id)) == 7,
  length(unique(model_data_inla$st_id)) == 185
)

# ------------------------------------------------------------
#  Save the reconstructed data separately
# ------------------------------------------------------------

saveRDS(
  model_data_inla,
  file.path(
    nb_results_dir,
    "model_data_inla_NB_ready.rds"
  )
)

cat("\nSECTION 1 COMPLETED SUCCESSFULLY.\n")
cat("Reconstructed data saved in:\n")
cat(
  file.path(
    nb_results_dir,
    "model_data_inla_NB_ready.rds"
  ),
  "\n"
)

########## Additional Diagnosis

# Confirm every state-year combination has exactly one st_id
interaction_check <- model_data_inla %>%
  distinct(state_id, year_id, st_id) %>%
  count(state_id, year_id, name = "n_ids")

cat("Number of distinct state-year combinations:",
    nrow(interaction_check), "\n")

cat("Combinations assigned more than one ID:",
    sum(interaction_check$n_ids != 1), "\n")

# Check that each state-year combination has a unique ID
cat("Unique state-year IDs:",
    n_distinct(model_data_inla$st_id), "\n")

# Check the standardized variables
cat("Mean of standardized population density:",
    mean(model_data_inla$log_pop_density_z), "\n")

cat("SD of standardized population density:",
    sd(model_data_inla$log_pop_density_z), "\n")

cat("Mean of standardized health-facility density:",
    mean(model_data_inla$hf_density_z), "\n")

cat("SD of standardized health-facility density:",
    sd(model_data_inla$hf_density_z), "\n")


# ============================================================
#  FIT THREE NEGATIVE BINOMIAL MODELS
# ============================================================

# Directories
results_dir <- file.path(getwd(), "Model_Result")
nb_results_dir <- file.path(results_dir, "Negative_Binomial")

# Load the validated dataset
model_data_inla <- readRDS(
  file.path(
    nb_results_dir,
    "model_data_inla_NB_ready.rds"
  )
)

# Load the existing adjacency graph
g <- readRDS(
  file.path(results_dir, "adjacency_graph.rds")
)

# Confirm the dataset
stopifnot(
  nrow(model_data_inla) == 1290,
  length(unique(model_data_inla$state_id)) == 37,
  length(unique(model_data_inla$year_id)) == 5,
  length(unique(model_data_inla$age_id)) == 7,
  length(unique(model_data_inla$st_id)) == 185
)

cat("Data and adjacency graph loaded successfully.\n")

######### Define the fixed effects

# Fixed effects used in the full models
fixed_effects <- paste(
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
  sep = " + "
)

# Verify that all fixed-effect variables are available
fixed_vars <- c(
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

stopifnot(all(fixed_vars %in% names(model_data_inla)))


# ============================================================
# NEGATIVE BINOMIAL MODEL COMPARISON
# ============================================================

results_dir <- file.path(getwd(), "Model_Result")
nb_results_dir <- file.path(results_dir, "Negative_Binomial")

model_data_inla <- readRDS(
  file.path(nb_results_dir, "model_data_inla_NB_ready.rds")
)

g <- readRDS(
  file.path(results_dir, "adjacency_graph.rds")
)

# Common INLA fitting controls
fit_controls <- list(
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  )
)

# ------------------------------------------------------------
# MODEL 1: NULL MODEL
# ------------------------------------------------------------
# This uses the same random-effect structure as the
# original null model only if res0 used these effects.
# Verify the original res0 formula before treating this
# as an exact reproduction.

formula_nb_null <- births ~
  f(
    state_id,
    model = "besag",
    graph = g,
    scale.model = TRUE
  ) +
  f(year_id, model = "rw2") +
  f(age_id, model = "iid")

res_nb_null <- do.call(
  INLA::inla,
  c(
    list(
      formula = formula_nb_null,
      family = "nbinomial",
      data = model_data_inla,
      E = model_data_inla$exposure
    ),
    fit_controls
  )
)

saveRDS(
  res_nb_null,
  file.path(nb_results_dir, "res_nb_null.rds")
)


# ------------------------------------------------------------
# MODEL 2: FULL ADDITIVE MODEL
# ------------------------------------------------------------

formula_nb_full <- births ~
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
  f(age_id, model = "iid")

res_nb_full <- do.call(
  INLA::inla,
  c(
    list(
      formula = formula_nb_full,
      family = "nbinomial",
      data = model_data_inla,
      E = model_data_inla$exposure
    ),
    fit_controls
  )
)

saveRDS(
  res_nb_full,
  file.path(nb_results_dir, "res_nb_full.rds")
)


# ------------------------------------------------------------
# MODEL 3: FULL MODEL + STATE-YEAR INTERACTION
# ------------------------------------------------------------

formula_nb_interaction <- births ~
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

res_nb_interaction <- do.call(
  INLA::inla,
  c(
    list(
      formula = formula_nb_interaction,
      family = "nbinomial",
      data = model_data_inla,
      E = model_data_inla$exposure
    ),
    fit_controls
  )
)

saveRDS(
  res_nb_interaction,
  file.path(nb_results_dir, "res_nb_interaction.rds")
)

cat("All three Negative Binomial model fits completed.\n")

#########################################################
####  Compare DIC and WAIC
#########################################################

nb_model_comparison <- data.frame(
  Model = c(
    "Null",
    "Full additive",
    "Full + state-year interaction"
  ),
  DIC = c(
    res_nb_null$dic$dic,
    res_nb_full$dic$dic,
    res_nb_interaction$dic$dic
  ),
  pD = c(
    res_nb_null$dic$p.eff,
    res_nb_full$dic$p.eff,
    res_nb_interaction$dic$p.eff
  ),
  WAIC = c(
    res_nb_null$waic$waic,
    res_nb_full$waic$waic,
    res_nb_interaction$waic$waic
  ),
  pWAIC = c(
    res_nb_null$waic$p.eff,
    res_nb_full$waic$p.eff,
    res_nb_interaction$waic$p.eff
  )
)

print(nb_model_comparison)

write.csv(
  nb_model_comparison,
  file.path(
    nb_results_dir,
    "negative_binomial_model_comparison.csv"
  ),
  row.names = FALSE
)

saveRDS(
  nb_model_comparison,
  file.path(
    nb_results_dir,
    "negative_binomial_model_comparison.rds"
  )
)


# Extract PIT values from the NB interaction model
pit_nb <- res_nb_interaction$cpo$pit

# Remove missing or non-finite values
pit_nb <- pit_nb[is.finite(pit_nb)]

# Summary statistics
summary(pit_nb)
sd(pit_nb)

# Histogram
hist(
  pit_nb,
  breaks = seq(0, 1, by = 0.05),
  main = "PIT Histogram: Negative Binomial Interaction Model",
  xlab = "PIT",
  ylab = "Frequency",
  col = "grey80",
  border = "white"
)

# Reference line for a uniform distribution
abline(
  h = length(pit_nb) * 0.05,
  lty = 2,
  lwd = 2
)


# ============================================================
#  PREPARE NB MODEL DIAGNOSTICS
# ============================================================


results_dir <- file.path(getwd(), "Model_Result")
nb_results_dir <- file.path(
  results_dir, "Negative_Binomial"
)

# Load the saved NB interaction model if necessary
if (!exists("res_nb_interaction")) {
  res_nb_interaction <- readRDS(
    file.path(
      nb_results_dir,
      "res_nb_interaction.rds"
    )
  )
}

# Extract CPO and PIT
cpo_nb <- res_nb_interaction$cpo$cpo
pit_nb <- res_nb_interaction$cpo$pit

# Retain valid values
cpo_nb <- cpo_nb[
  is.finite(cpo_nb) & cpo_nb > 0
]

pit_nb <- pit_nb[is.finite(pit_nb)]

# Transform CPO values
neg_log_cpo_nb <- -log(cpo_nb)

# Display diagnostic summaries
cat("Valid CPO observations:", length(cpo_nb), "\n")
cat("Valid PIT observations:", length(pit_nb), "\n")

cat("\nSummary of -log(CPO):\n")
print(summary(neg_log_cpo_nb))

cat("\nSummary of PIT:\n")
print(summary(pit_nb))


# ============================================================
#  NB CPO HISTOGRAM
# ============================================================

png(
  filename = file.path(
    nb_results_dir,
    "CPOPlot_NB.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

hist(
  neg_log_cpo_nb,
  breaks = "Sturges",
  main = "Log-CPO Distribution",
  xlab = expression(-log(CPO)),
  ylab = "Frequency",
  col = "#A6D785",
  border = "white"
)

dev.off()


# ============================================================
#  NB PIT HISTOGRAM
# ============================================================

png(
  filename = file.path(
    nb_results_dir,
    "PITPlot_NB.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

hist(
  pit_nb,
  breaks = seq(0, 1, by = 0.05),
  main = "PIT Histogram",
  xlab = "PIT value",
  ylab = "Frequency",
  col = "#A6CEE3",
  border = "white"
)

# Expected frequency under a uniform distribution
abline(
  h = length(pit_nb) * 0.05,
  col = "red",
  lty = 2,
  lwd = 2
)

dev.off()


# ============================================================
#  SAVE NB DIAGNOSTIC SUMMARY
# ============================================================

cpo_summary_nb <- data.frame(
  Statistic = c(
    "Number of valid CPO values",
    "Mean negative log-CPO",
    "Median negative log-CPO",
    "Maximum negative log-CPO",
    "Number of valid PIT values",
    "Mean PIT",
    "Median PIT",
    "SD PIT"
  ),
  Value = c(
    length(cpo_nb),
    mean(neg_log_cpo_nb),
    median(neg_log_cpo_nb),
    max(neg_log_cpo_nb),
    length(pit_nb),
    mean(pit_nb),
    median(pit_nb),
    sd(pit_nb)
  )
)

write.csv(
  cpo_summary_nb,
  file.path(
    nb_results_dir,
    "NB_CPO_PIT_Summary.csv"
  ),
  row.names = FALSE
)

print(cpo_summary_nb)


# ============================================================
# COMPARATIVE CPO AND PIT DIAGNOSTICS :Poisson & Negative Binomial
# Both models are plotted using identical bin boundaries/scales.
# ============================================================

#  Extract diagnostics from the final fitted model objects
cpo_poisson <- res_interaction_scaled$cpo$cpo
pit_poisson <- res_interaction_scaled$cpo$pit

cpo_nb <- res_nb_interaction$cpo$cpo
pit_nb <- res_nb_interaction$cpo$pit

#  Retain valid values
cpo_poisson <- cpo_poisson[is.finite(cpo_poisson) & cpo_poisson > 0]
cpo_nb      <- cpo_nb[is.finite(cpo_nb) & cpo_nb > 0]

pit_poisson <- pit_poisson[is.finite(pit_poisson) &
                             pit_poisson >= 0 & pit_poisson <= 1]
pit_nb      <- pit_nb[is.finite(pit_nb) &
                        pit_nb >= 0 & pit_nb <= 1]

#  Transform CPO values
neg_log_cpo_poisson <- -log(cpo_poisson)
neg_log_cpo_nb      <- -log(cpo_nb)

#  Define COMMON CPO breaks and common plotting limits
combined_cpo <- c(neg_log_cpo_poisson, neg_log_cpo_nb)
cpo_breaks <- pretty(range(combined_cpo), n = 12)
cpo_xlim <- range(cpo_breaks)

# Determine a common vertical-axis limit from both histograms
h_cpo_poisson <- hist(neg_log_cpo_poisson, breaks = cpo_breaks, plot = FALSE)
h_cpo_nb      <- hist(neg_log_cpo_nb, breaks = cpo_breaks, plot = FALSE)
cpo_ylim <- c(0, max(h_cpo_poisson$counts, h_cpo_nb$counts))

#  Plot Poisson and NB CPO histograms using identical breaks and axes
png(file.path(nb_results_dir, "CPOPlot_Poisson_CommonScale.png"),
    width = 1800, height = 1200, res = 200)
hist(neg_log_cpo_poisson,
     breaks = cpo_breaks,
     xlim = cpo_xlim, ylim = cpo_ylim,
     main = "Log-CPO Distribution: Poisson Model",
     xlab = expression(-log(CPO)), ylab = "Frequency",
     col = "#A6CEE3", border = "white")
dev.off()

png(file.path(nb_results_dir, "CPOPlot_NB_CommonScale.png"),
    width = 1800, height = 1200, res = 200)
hist(neg_log_cpo_nb,
     breaks = cpo_breaks,
     xlim = cpo_xlim, ylim = cpo_ylim,
     main = "Log-CPO Distribution: Negative Binomial Model",
     xlab = expression(-log(CPO)), ylab = "Frequency",
     col = "#A6CEE3", border = "white")
dev.off()

#  Define COMMON PIT breaks and common vertical-axis limit
pit_breaks <- seq(0, 1, by = 0.05)
h_pit_poisson <- hist(pit_poisson, breaks = pit_breaks, plot = FALSE,
                      include.lowest = TRUE, right = FALSE)
h_pit_nb <- hist(pit_nb, breaks = pit_breaks, plot = FALSE,
                 include.lowest = TRUE, right = FALSE)
pit_ylim <- c(0, max(h_pit_poisson$counts, h_pit_nb$counts,
                     length(pit_poisson) * 0.05,
                     length(pit_nb) * 0.05))

#  Plot Poisson and NB PIT histograms using identical breaks and axes
png(file.path(nb_results_dir, "PITPlot_Poisson_CommonScale.png"),
    width = 1800, height = 1200, res = 200)
hist(pit_poisson,
     breaks = pit_breaks, include.lowest = TRUE, right = FALSE,
     xlim = c(0, 1), ylim = pit_ylim,
     main = "PIT Histogram: Poisson Model",
     xlab = "Probability Integral Transform (PIT)", ylab = "Frequency",
     col = "#A6CEE3", border = "white")
abline(h = length(pit_poisson) / 20, col = "red", lty = 2, lwd = 2)
dev.off()

png(file.path(nb_results_dir, "PITPlot_NB_CommonScale.png"),
    width = 1800, height = 1200, res = 200)
hist(pit_nb,
     breaks = pit_breaks, include.lowest = TRUE, right = FALSE,
     xlim = c(0, 1), ylim = pit_ylim,
     main = "PIT Histogram: Negative Binomial Model",
     xlab = "Probability Integral Transform (PIT)", ylab = "Frequency",
     col = "#A6CEE3", border = "white")
abline(h = length(pit_nb) / 20, col = "red", lty = 2, lwd = 2)
dev.off()

#  Print checks confirming the common plotting scales
cat("Poisson valid CPO values:", length(cpo_poisson), "\n")
cat("NB valid CPO values:", length(cpo_nb), "\n")
cat("Common CPO x-axis limits:", cpo_xlim, "\n")
cat("Common CPO y-axis limits:", cpo_ylim, "\n\n")

cat("Poisson valid PIT values:", length(pit_poisson), "\n")
cat("NB valid PIT values:", length(pit_nb), "\n")
cat("Common PIT x-axis limits: 0 to 1\n")
cat("Common PIT y-axis limits:", pit_ylim, "\n")
cat("Number of PIT bins:", length(pit_breaks) - 1, "\n")

#  Save a compact diagnostic summary
comparison_summary <- data.frame(
  Model = c("Poisson", "Negative Binomial"),
  Valid_CPO = c(length(cpo_poisson), length(cpo_nb)),
  Mean_negative_log_CPO = c(mean(neg_log_cpo_poisson), mean(neg_log_cpo_nb)),
  Median_negative_log_CPO = c(median(neg_log_cpo_poisson), median(neg_log_cpo_nb)),
  Valid_PIT = c(length(pit_poisson), length(pit_nb)),
  Mean_PIT = c(mean(pit_poisson), mean(pit_nb)),
  Median_PIT = c(median(pit_poisson), median(pit_nb)),
  SD_PIT = c(sd(pit_poisson), sd(pit_nb))
)

write.csv(comparison_summary,
          file.path(nb_results_dir, "Comparative_CPO_PIT_Summary.csv"),
          row.names = FALSE)
print(comparison_summary)


# ============================================================
#  NEGATIVE BINOMIAL LOSO VALIDATION
# Mirrors the original Poisson LOSO procedure
# ============================================================


# ------------------------------------------------------------
#  Directories and data
# ------------------------------------------------------------

results_dir <- file.path(getwd(), "Model_Result")

nb_results_dir <- file.path(
  results_dir, "Negative_Binomial"
)

model_data_inla <- readRDS(
  file.path(
    nb_results_dir,
    "model_data_inla_NB_ready.rds"
  )
)

g <- readRDS(
  file.path(results_dir, "adjacency_graph.rds")
)

# Preserve observed births separately
validation_data <- model_data_inla
validation_data$births_observed <- validation_data$births

# ------------------------------------------------------------
#  Structural checks
# ------------------------------------------------------------

stopifnot(nrow(validation_data) == 1290)
stopifnot(length(unique(validation_data$state_id)) == 37)
stopifnot(length(unique(validation_data$st_id)) == 185)
stopifnot(g$n == 37)
stopifnot(g$cc$n == 1)

required_vars <- c(
  "births", "births_observed", "exposure",
  "prop_urban", "prop_secondary", "prop_higher",
  "prop_middle", "prop_richer", "prop_richest",
  "prop_currently_married", "mean_age",
  "log_pop_density_z", "nightlights",
  "conflict_events", "hf_density_z",
  "state_id", "year_id", "age_id", "st_id"
)

stopifnot(all(required_vars %in% names(validation_data)))
stopifnot(all(validation_data$exposure > 0))

# ------------------------------------------------------------
#  Exact NB interaction model formula
# ------------------------------------------------------------

formula_loso_nb <- births ~
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
#  State lookup and storage
# ------------------------------------------------------------

state_lookup <- validation_data %>%
  select(state_id, REGCODE, REGNAME) %>%
  distinct() %>%
  arrange(state_id)

states <- sort(unique(validation_data$state_id))

loso_results_nb <- vector("list", length(states))

all_observed_nb <- numeric(0)
all_predicted_nb <- numeric(0)

# ------------------------------------------------------------
#  Fit the 37 held-out-state models
# ------------------------------------------------------------

for (j in seq_along(states)) {
  
  s <- states[j]
  
  state_name <- state_lookup$REGNAME[
    state_lookup$state_id == s
  ]
  
  cat(
    "\n----------------------------------------\n",
    "NB LOSO fold", j, "of", length(states),
    "| State", s, "|", state_name, "\n"
  )
  
  # Copy the complete data; retain the complete graph
  fold_data <- validation_data
  
  holdout <- fold_data$state_id == s
  
  # Hide all outcomes for the held-out state
  fold_data$births[holdout] <- NA
  
  # Fit the NB interaction model
  fit_fold_nb <- INLA::inla(
    formula_loso_nb,
    family = "nbinomial",
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
  
  # Posterior mean of the linear predictor
  eta_hat <- fit_fold_nb$summary.linear.predictor$mean[
    holdout
  ]
  
  # Convert to expected birth counts
  predicted <- exp(eta_hat) *
    validation_data$exposure[holdout]
  
  observed <- validation_data$births_observed[
    holdout
  ]
  
  # Validate predictions
  if (
    length(observed) != length(predicted) ||
    any(!is.finite(predicted)) ||
    anyNA(observed)
  ) {
    stop(
      paste("Invalid LOSO predictions for", state_name)
    )
  }
  
  # State-specific performance
  rmse <- sqrt(mean((observed - predicted)^2))
  mae <- mean(abs(observed - predicted))
  
  loso_results_nb[[j]] <- data.frame(
    state_id = s,
    REGCODE = state_lookup$REGCODE[
      state_lookup$state_id == s
    ],
    REGNAME = state_name,
    n_test = length(observed),
    RMSE = rmse,
    MAE = mae
  )
  
  # Pool out-of-sample predictions
  all_observed_nb <- c(all_observed_nb, observed)
  all_predicted_nb <- c(all_predicted_nb, predicted)
  
  cat(
    "Observations:", length(observed),
    "| RMSE:", round(rmse, 4),
    "| MAE:", round(mae, 4),
    "\n"
  )
  
  # Save progress after each completed state
  saveRDS(
    list(
      completed_folds = j,
      results = loso_results_nb,
      observed = all_observed_nb,
      predicted = all_predicted_nb
    ),
    file.path(nb_results_dir, "NB_LOSO_progress.rds")
  )
}

# ------------------------------------------------------------
#  Combine state-level results
# ------------------------------------------------------------

loso_results_df_nb <- bind_rows(loso_results_nb)

stopifnot(nrow(loso_results_df_nb) == 37)
stopifnot(length(all_observed_nb) == 1290)
stopifnot(length(all_predicted_nb) == 1290)

# ------------------------------------------------------------
#  Overall predictive performance
# ------------------------------------------------------------

overall_RMSE_nb <- sqrt(
  mean((all_observed_nb - all_predicted_nb)^2)
)

overall_MAE_nb <- mean(
  abs(all_observed_nb - all_predicted_nb)
)

mean_state_RMSE_nb <- mean(loso_results_df_nb$RMSE)
mean_state_MAE_nb <- mean(loso_results_df_nb$MAE)

median_state_RMSE_nb <- median(loso_results_df_nb$RMSE)
median_state_MAE_nb <- median(loso_results_df_nb$MAE)

correlation_nb <- cor(
  all_observed_nb,
  all_predicted_nb,
  method = "pearson"
)

# Match the sign convention in the Poisson script:
# positive bias means predictions exceed observed counts
bias_nb <- mean(all_predicted_nb - all_observed_nb)

loso_summary_nb <- data.frame(
  States = length(states),
  Observations = length(all_observed_nb),
  Overall_RMSE = overall_RMSE_nb,
  Overall_MAE = overall_MAE_nb,
  Mean_state_RMSE = mean_state_RMSE_nb,
  Mean_state_MAE = mean_state_MAE_nb,
  Median_state_RMSE = median_state_RMSE_nb,
  Median_state_MAE = median_state_MAE_nb,
  Pearson_correlation = correlation_nb,
  Mean_prediction_bias = bias_nb
)

print(loso_summary_nb)

# ------------------------------------------------------------
#  Save validation results
# ------------------------------------------------------------

write.csv(
  loso_results_df_nb,
  file.path(nb_results_dir, "NB_LOSO_State_Results.csv"),
  row.names = FALSE
)

write.csv(
  loso_summary_nb,
  file.path(nb_results_dir, "NB_LOSO_Summary.csv"),
  row.names = FALSE
)

loso_plot_data_nb <- data.frame(
  observed = all_observed_nb,
  predicted = all_predicted_nb
)

saveRDS(
  loso_plot_data_nb,
  file.path(nb_results_dir, "NB_LOSO_Predictions.rds")
)

# ------------------------------------------------------------
#  Observed-versus-predicted plot
# Retain the Poisson plot's turquoise colour
# ------------------------------------------------------------

p_loso_nb <- ggplot(
  loso_plot_data_nb,
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
      sprintf("%.3f", correlation_nb)
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

print(p_loso_nb)

ggsave(
  filename = file.path(
    nb_results_dir,
    "NB_LOSO_Observed_vs_Predicted.png"
  ),
  plot = p_loso_nb,
  width = 8,
  height = 6,
  dpi = 300
)

cat(
  "\nNB LOSO validation completed for",
  length(states), "states and",
  length(all_observed_nb), "observations.\n"
)


# ============================================================
#  NB FIXED EFFECTS AND RATE RATIOS
# ============================================================


results_dir <- file.path(getwd(), "Model_Result")
nb_results_dir <- file.path(
  results_dir, "Negative_Binomial"
)

# Load the fitted NB interaction model
res_nb_interaction <- readRDS(
  file.path(nb_results_dir, "res_nb_interaction.rds")
)

# Extract posterior summaries of fixed effects
fixed_nb <- as.data.frame(
  res_nb_interaction$summary.fixed
)

fixed_nb$Variable <- rownames(fixed_nb)
rownames(fixed_nb) <- NULL

# Calculate rate ratios and exponentiated credible intervals
nb_fixed_results <- data.frame(
  Variable = fixed_nb$Variable,
  Posterior_Mean = fixed_nb$mean,
  Posterior_SD = fixed_nb$sd,
  Lower_95_CrI = fixed_nb$`0.025quant`,
  Upper_95_CrI = fixed_nb$`0.975quant`,
  Rate_Ratio = exp(fixed_nb$mean),
  RR_Lower_95_CrI = exp(fixed_nb$`0.025quant`),
  RR_Upper_95_CrI = exp(fixed_nb$`0.975quant`)
)

# Round for presentation
numeric_cols <- setdiff(
  names(nb_fixed_results), "Variable"
)

nb_fixed_results[numeric_cols] <- lapply(
  nb_fixed_results[numeric_cols],
  round,
  digits = 3
)

# Display results
print(nb_fixed_results, row.names = FALSE)

# Save full results
write.csv(
  nb_fixed_results,
  file.path(
    nb_results_dir,
    "NB_Fixed_Effects_Rate_Ratios.csv"
  ),
  row.names = FALSE
)

cat(
  "\nNB fixed-effects table saved successfully.\n"
)




# 1. Load the saved fitted models
results_dir <- file.path(
  "C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData",
  "TRFPROJECT_EDIT/Model_Result"
)

nb_dir <- file.path(results_dir, "Negative_Binomial")

poisson_model <- readRDS(
  file.path(results_dir, "res_interaction_scaled.rds")
)

nb_model <- readRDS(
  file.path(nb_dir, "res_nb_interaction.rds")
)

# 2. Extract fixed effects and calculate rate ratios
extract_fixed <- function(model, model_name) {
  
  as.data.frame(model$summary.fixed) %>%
    rownames_to_column("Covariate") %>%
    transmute(
      Covariate,
      Model = model_name,
      Estimate = mean,
      Lower_CrI = `0.025quant`,
      Upper_CrI = `0.975quant`,
      Rate_Ratio = exp(mean),
      RR_Lower = exp(`0.025quant`),
      RR_Upper = exp(`0.975quant`)
    )
}

poisson_fixed <- extract_fixed(
  poisson_model, "Poisson"
)

nb_fixed <- extract_fixed(
  nb_model, "Negative Binomial"
)

# 3. Join the estimates side by side
comparison_table <- poisson_fixed %>%
  select(
    Covariate,
    Poisson_Estimate = Estimate,
    Poisson_Lower_CrI = Lower_CrI,
    Poisson_Upper_CrI = Upper_CrI,
    Poisson_RR = Rate_Ratio,
    Poisson_RR_Lower = RR_Lower,
    Poisson_RR_Upper = RR_Upper
  ) %>%
  inner_join(
    nb_fixed %>%
      select(
        Covariate,
        NB_Estimate = Estimate,
        NB_Lower_CrI = Lower_CrI,
        NB_Upper_CrI = Upper_CrI,
        NB_RR = Rate_Ratio,
        NB_RR_Lower = RR_Lower,
        NB_RR_Upper = RR_Upper
      ),
    by = "Covariate"
  )

# 4. Round for presentation
comparison_table <- comparison_table %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

print(comparison_table, row.names = FALSE)

# Save as CSV using base R
write.csv(
  comparison_table,
  file = file.path(
    nb_dir,
    "Poisson_vs_NB_Fixed_Effects_Comparison.csv"
  ),
  row.names = FALSE
)

# Confirm that the file exists
file.exists(
  file.path(
    nb_dir,
    "Poisson_vs_NB_Fixed_Effects_Comparison.csv"
  )
)



# ============================================================
# NEGATIVE BINOMIAL INTERACTION MODEL: FOREST PLOT
# ============================================================

library(ggplot2)

# Set directories
project_dir <- "C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT"

nb_dir <- file.path(
  project_dir,
  "Model_Result",
  "Negative_Binomial"
)

# Load the saved NB interaction model
res_nb_interaction <- readRDS(
  file.path(nb_dir, "res_nb_interaction.rds")
)

# Extract fixed-effect posterior summaries
fixed_nb <- as.data.frame(
  res_nb_interaction$summary.fixed
)

fixed_nb$Covariate <- rownames(fixed_nb)

# Calculate rate ratios and 95% credible intervals
fixed_nb$RR <- exp(fixed_nb$mean)
fixed_nb$RR_lower <- exp(fixed_nb$`0.025quant`)
fixed_nb$RR_upper <- exp(fixed_nb$`0.975quant`)

# Exclude intercept from the forest plot
forest_nb <- fixed_nb[
  fixed_nb$Covariate != "(Intercept)",
]

########
# Use readable labels
labels_nb <- c(
  prop_urban = "Urban residence",
  prop_secondary = "Secondary education",
  prop_higher = "Higher education",
  prop_middle = "Middle wealth",
  prop_richer = "Richer wealth",
  prop_richest = "Richest wealth",
  prop_currently_married = "Currently married",
  mean_age = "Mean age",
  log_pop_density_z = "Population density",
  nightlights = "Night-time lights",
  conflict_events = "Conflict events",
  hf_density_z = "Health facility density"
)

forest_nb$Label <- labels_nb[forest_nb$Covariate]

# Order variables for plotting
forest_nb$Label <- factor(
  forest_nb$Label,
  levels = rev(unname(labels_nb))
)

# Create forest plot
p_forest_nb <- ggplot(
  forest_nb,
  aes(x = RR, y = Label)
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(xmin = RR_lower, xmax = RR_upper),
    width = 0.18,
    orientation = "y",
    linewidth = 0.7,
    colour = "#238B45"
  ) +
  geom_point(
    size = 2.8,
    colour = "#238B45"
  ) +
  scale_x_log10() +
  labs(
    #title = "Forest Plot of Fixed Effects: Negative Binomial Model",
    #subtitle = "Posterior rate ratios with 95% credible intervals",
    x = "Rate ratio (log scale)",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(colour = "black"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(p_forest_nb)

# Save the plot
ggsave(
  filename = file.path(nb_dir, "ForestPlot_NB.png"),
  plot = p_forest_nb,
  width = 10,
  height = 7,
  dpi = 300
)

# Save the rate-ratio results for writing the manuscript
write.csv(
  forest_nb[
    , c("Covariate", "mean", "0.025quant", "0.975quant",
        "RR", "RR_lower", "RR_upper")
  ],
  file = file.path(nb_dir, "ForestPlot_NB_Results.csv"),
  row.names = FALSE
)

cat("NB forest plot and results table saved to:\n", nb_dir)

# ============================================================
# NB INTERACTION MODEL: ENABLE POSTERIOR SAMPLING FOR ASFR
# ============================================================

project_dir <- file.path(
  "C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData",
  "TRFPROJECT_EDIT"
)

nb_dir <- file.path(
  project_dir, "Model_Result", "Negative_Binomial"
)

# Load the model data and spatial adjacency graph
model_data_inla <- readRDS(
  file.path(nb_dir, "model_data_inla_NB_ready.rds")
)

g <- readRDS(
  file.path(project_dir, "Model_Result", "adjacency_graph.rds")
)

# Check required variables
stopifnot(nrow(model_data_inla) == 1290)
stopifnot(all(c(
  "births", "exposure", "state_id", "year_id",
  "age_id", "st_id", "survey_year", "age_group"
) %in% names(model_data_inla)))

################# Plot of ASFR with ASFR Computed ################

#### Start by Re-fit the NB interaction model with config = TRUE

res_nb_interaction_asfr <- INLA::inla(
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
    f(
      state_id,
      model = "besag",
      graph = g,
      scale.model = TRUE
    ) +
    f(year_id, model = "rw2") +
    f(age_id, model = "iid") +
    f(st_id, model = "iid"),
  
  family = "nbinomial",
  data = model_data_inla,
  E = exposure,
  
  control.predictor = list(compute = TRUE),
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)

# Save the refitted model
saveRDS(
  res_nb_interaction_asfr,
  file.path(nb_dir, "res_nb_interaction_config.rds")
)

# Check model criteria
res_nb_interaction_asfr$dic$dic
res_nb_interaction_asfr$waic$waic



# Extract posterior samples from the refitted NB model
set.seed(20261102)

n_samp <- 1000

lp_samp_nb <- INLA::inla.posterior.sample(
  n = n_samp,
  result = res_nb_interaction_asfr,
  selection = list(Predictor = 0)
)

# Extract the linear predictor for each observation
eta_mat_nb <- sapply(lp_samp_nb, function(x) {
  x$latent[
    grep("^Predictor", rownames(x$latent)),
    1
  ]
})

# Check dimensions
print(dim(eta_mat_nb))
print(nrow(model_data_inla))

stopifnot(nrow(eta_mat_nb) == nrow(model_data_inla))

# Transform to rates
lambda_mat_nb <- exp(eta_mat_nb)

# Save posterior rate samples
saveRDS(
  lambda_mat_nb,
  file.path(nb_dir, "lambda_mat_NB.rds")
)


# ============================================================
# NB ASFR SUMMARY, PLOT AND SUPPLEMENTARY TABLE
# ============================================================

# Link posterior rates to the model data
index_df <- model_data_inla %>%
  select(state_id, survey_year, age_id, age_group, exposure) %>%
  mutate(obs = row_number())

lambda_long_nb <- as.data.frame(lambda_mat_nb) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(
    cols = -obs,
    names_to = "draw",
    values_to = "lambda"
  ) %>%
  left_join(index_df, by = "obs")

# Calculate exposure-weighted ASFR by age and survey year
asfr_post_nb <- lambda_long_nb %>%
  group_by(survey_year, age_id, draw) %>%
  summarise(
    births_hat = sum(lambda * exposure),
    exposure_total = sum(exposure),
    ASFR = births_hat / exposure_total,
    .groups = "drop"
  )

# Posterior mean and 95% credible intervals
asfr_summary_nb <- asfr_post_nb %>%
  group_by(survey_year, age_id) %>%
  summarise(
    mean = mean(ASFR),
    low = quantile(ASFR, 0.025),
    high = quantile(ASFR, 0.975),
    .groups = "drop"
  )

# Add age-group labels
age_lookup <- model_data_inla %>%
  select(age_id, age_group) %>%
  distinct() %>%
  arrange(age_id)

asfr_summary_nb <- asfr_summary_nb %>%
  left_join(age_lookup, by = "age_id")

asfr_summary_nb$age_group <- factor(
  asfr_summary_nb$age_group,
  levels = c(
    "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49"
  )
)

# Generate the ASFR plot
p_asfr_nb <- ggplot(
  asfr_summary_nb,
  aes(
    x = age_group,
    y = mean,
    colour = factor(survey_year),
    group = factor(survey_year)
  )
) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c(
      "2003" = "#1b9e77",
      "2008" = "#d95f02",
      "2013" = "#7570b3",
      "2018" = "#e7298a",
      "2024" = "#66a61e"
    ),
    name = "Survey Year"
  ) +
  labs(
    x = "Age Group",
    y = "Age-Specific Fertility Rate (ASFR)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_asfr_nb)

# Save plot
ggsave(
  filename = file.path(nb_dir, "ASFRPlot_NB.png"),
  plot = p_asfr_nb,
  width = 9,
  height = 6,
  dpi = 300
)

# Create supplementary table of posterior mean ASFRs
asfr_table_nb <- asfr_summary_nb %>%
  select(survey_year, age_group, mean) %>%
  pivot_wider(
    names_from = survey_year,
    values_from = mean
  ) %>%
  mutate(across(-age_group, ~ round(.x, 3)))

print(asfr_table_nb)

# Save full posterior summary and supplementary table
write.csv(
  asfr_summary_nb,
  file.path(nb_dir, "ASFR_NB_Posterior_Summary.csv"),
  row.names = FALSE
)

write.csv(
  asfr_table_nb,
  file.path(nb_dir, "ASFR_NB_Supplementary_Table.csv"),
  row.names = FALSE
)

cat("ASFR plot and supplementary tables saved in:", nb_dir, "\n")

# ============================================================
# NEGATIVE BINOMIAL MODEL: TEMPORAL RANDOM EFFECT (RW2)
# ============================================================

# 1. Extract RW2 temporal effect from NB model
time_eff_nb <- res_nb_interaction_asfr$summary.random$year_id %>%
  mutate(
    survey_year = sort(unique(model_data_inla$survey_year))
  )

# 2. Inspect temporal effect estimates
print(time_eff_nb)

# 3. Plot posterior temporal effect
p_time_nb <- ggplot(
  time_eff_nb,
  aes(x = survey_year, y = mean)
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  geom_ribbon(
    aes(
      ymin = `0.025quant`,
      ymax = `0.975quant`
    ),
    fill = "#A6CEE3",
    alpha = 0.4
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  scale_x_continuous(
    breaks = sort(unique(model_data_inla$survey_year)),
    limits = c(
      min(time_eff_nb$survey_year),
      max(time_eff_nb$survey_year) + 0.5
    )
  ) +
  labs(
    title = "Posterior Temporal Effect",
    subtitle = "Negative Binomial spatio-temporal model (RW2)",
    x = "Survey Year",
    y = "Temporal Random Effect"
  ) +
  theme_minimal(base_size = 13)

# 4. Display plot
print(p_time_nb)

# 5. Save plot
ggsave(
  filename = file.path(
    nb_dir,
    "Temporal_Effect_NB.png"
  ),
  plot = p_time_nb,
  width = 9,
  height = 6,
  dpi = 300
)

cat(
  "NB temporal-effect plot saved in:",
  nb_dir,
  "\n"
)



# ============================================================
# NEGATIVE BINOMIAL MODEL: SPATIAL RANDOM EFFECTS (ICAR)
# State-level posterior mean map
# ============================================================

# 1. Extract spatial random effects from NB model
state_eff_nb <- res_nb_interaction_asfr$summary.random$state_id %>%
  mutate(
    REGCODE = as.integer(ID)
  )

# 2. Inspect spatial effect estimates
print(state_eff_nb)

# 3. Join spatial effects with Nigeria state shapefile
nga_states_plot_nb <- nga_states %>%
  left_join(
    state_eff_nb,
    by = "REGCODE"
  )

# 4. Calculate state centroids for labels
nga_states_plot_nb <- nga_states_plot_nb %>%
  mutate(
    centroid = st_centroid(geometry)
  ) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# 5. Generate state-level posterior mean map
p_spatial_nb <- ggplot(nga_states_plot_nb) +
  
  geom_sf(
    aes(fill = mean),
    color = "gray40",
    linewidth = 0.4
  ) +
  
  geom_text(
    aes(
      x = lon,
      y = lat,
      label = REGNAME
    ),
    size = 2.3,
    fontface = "plain",
    color = "black"
  ) +
  
  scale_fill_gradient2(
    low = "#D73027",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "Spatial Effect"
  ) +
  
  labs(
    #title = "Posterior Spatial Effects",
    #subtitle = "Negative Binomial spatio-temporal model",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

# 6. Display map
print(p_spatial_nb)

# 7. Save map
ggsave(
  filename = file.path(
    nb_dir,
    "Spatial_Effects_NB.png"
  ),
  plot = p_spatial_nb,
  width = 10,
  height = 8,
  dpi = 300
)

cat(
  "NB spatial effects map saved in:",
  nb_dir,
  "\n"
)



# ============================================================
# NEGATIVE BINOMIAL MODEL: STATE-YEAR INTERACTION EFFECTS
# ============================================================

# 1. Extract state-year interaction effects from NB model
st_eff_nb <- as.data.frame(
  res_nb_interaction_asfr$summary.random$st_id
) %>%
  dplyr::select(
    ID,
    mean,
    `0.025quant`,
    `0.975quant`
  )

# 2. Construct the state-year mapping
# Use the original st_id from the model data
st_index_nb <- model_data_inla %>%
  distinct(
    state_id,
    survey_year,
    st_id
  ) %>%
  mutate(
    st_id = as.integer(st_id)
  )

# 3. Merge interaction effects with state-year mapping
st_eff_nb <- st_eff_nb %>%
  mutate(
    ID = as.integer(ID)
  ) %>%
  left_join(
    st_index_nb,
    by = c("ID" = "st_id")
  )

# 4. Sanity checks
cat("Number of unique state-year combinations:",
    nrow(st_index_nb), "\n")

cat("Number of extracted interaction effects:",
    nrow(st_eff_nb), "\n")

cat("Unmatched interaction effects:",
    sum(is.na(st_eff_nb$state_id)), "\n")

stopifnot(
  nrow(st_eff_nb) == nrow(st_index_nb),
  !any(is.na(st_eff_nb$state_id)),
  !any(is.na(st_eff_nb$survey_year))
)

# 5. Attach interaction effects to state geometries
# REGCODE corresponds to state_id in the fitted model
nga_states_interaction_nb <- nga_states %>%
  mutate(
    REGCODE = as.integer(REGCODE)
  ) %>%
  left_join(
    st_eff_nb,
    by = c("REGCODE" = "state_id")
  )

# 6. Check the state-year map data
print(
  nga_states_interaction_nb %>%
    st_drop_geometry() %>%
    count(survey_year)
)

# 7. Generate state-year interaction maps
p_interaction_nb <- ggplot(
  nga_states_interaction_nb
) +
  
  geom_sf(
    aes(fill = mean),
    color = NA
  ) +
  
  facet_wrap(
    ~ survey_year,
    ncol = 3
  ) +
  
  scale_fill_gradient2(
    low = "#D73027",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "Interaction Effect"
  ) +
  
  labs(
    #title = "State-Year Interaction Effects",
    #subtitle = "Negative Binomial spatio-temporal model",
    fill = "Interaction Effect"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )

# 8. Display interaction maps
print(p_interaction_nb)

# 9. Save interaction maps
ggsave(
  filename = file.path(
    nb_dir,
    "State_Year_Interaction_NB.png"
  ),
  plot = p_interaction_nb,
  width = 12,
  height = 8,
  dpi = 300
)

cat(
  "NB state-year interaction maps saved in:",
  nb_dir,
  "\n"
)



# ============================================================
# NEGATIVE BINOMIAL MODEL: NATIONAL TFR
# Population-weighted trend and credible intervals
# ============================================================

# 1. Check required objects
stopifnot(exists("lambda_long_nb"))

# 2. Calculate population-weighted national TFR
tfr_nat_post_nb <- lambda_long_nb %>%
  group_by(survey_year, age_id, draw) %>%
  summarise(
    births_hat = sum(lambda * exposure, na.rm = TRUE),
    exposure_total = sum(exposure, na.rm = TRUE),
    asfr_nat = births_hat / exposure_total,
    .groups = "drop"
  ) %>%
  group_by(survey_year, draw) %>%
  summarise(
    TFR_nat = 5 * sum(asfr_nat),
    .groups = "drop"
  )

# 3. Calculate posterior national TFR summaries
tfr_national_nb <- tfr_nat_post_nb %>%
  group_by(survey_year) %>%
  summarise(
    mean = mean(TFR_nat, na.rm = TRUE),
    low = quantile(TFR_nat, 0.025, na.rm = TRUE),
    high = quantile(TFR_nat, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(survey_year)

# 4. Inspect national TFR estimates
print(tfr_national_nb)

# 5. Calculate percentage decline between 2003 and 2024
decline_post_nb <- tfr_nat_post_nb %>%
  filter(survey_year %in% c(2003, 2024)) %>%
  select(survey_year, draw, TFR_nat) %>%
  pivot_wider(
    names_from = survey_year,
    values_from = TFR_nat
  ) %>%
  mutate(
    pct_decline = 100 * (`2003` - `2024`) / `2003`
  )

# 6. Summarise percentage decline and 95% credible interval
decline_summary_nb <- decline_post_nb %>%
  summarise(
    mean_decline = mean(pct_decline, na.rm = TRUE),
    low_decline = quantile(pct_decline, 0.025, na.rm = TRUE),
    high_decline = quantile(pct_decline, 0.975, na.rm = TRUE)
  )

# 7. Print percentage decline
print(decline_summary_nb)

# 8. Create national TFR trend plot
p_tfr_national_nb <- ggplot(
  tfr_national_nb,
  aes(x = survey_year, y = mean)
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.5,
    color = "#2C7FB8"
  ) +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    fill = "#A6CEE3",
    alpha = 0.4
  ) +
  scale_x_continuous(
    breaks = c(2003, 2008, 2013, 2018, 2024)
  ) +
  labs(
    #title = "National Total Fertility Rate",
    #subtitle = "Negative Binomial spatio-temporal model",
    x = "Survey Year",
    y = "Population-weighted Total Fertility Rate"
  ) +
  theme_minimal(base_size = 13)

# 9. Display plot
print(p_tfr_national_nb)

# 10. Save national TFR plot
ggsave(
  filename = file.path(
    nb_dir,
    "National_TFR_NB.png"
  ),
  plot = p_tfr_national_nb,
  width = 9,
  height = 6,
  dpi = 300
)

# 11. Save national TFR posterior summary
write.csv(
  tfr_national_nb,
  file.path(
    nb_dir,
    "National_TFR_NB_Summary.csv"
  ),
  row.names = FALSE
)

# 12. Save percentage decline summary
write.csv(
  decline_summary_nb,
  file.path(
    nb_dir,
    "National_TFR_NB_Percentage_Decline.csv"
  ),
  row.names = FALSE
)

cat("\nNB national TFR analysis completed.\n")
cat("Outputs saved in:", nb_dir, "\n")


############################################################
# NEGATIVE BINOMIAL SENSITIVITY ANALYSIS: PRIOR SETTINGS
############################################################

# Function to construct the NB model using a specified PC prior
make_nb_prior_formula <- function(u) {
  
  births ~
    # Fixed effects
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
    f(state_id,
      model = "besag",
      graph = g,
      scale.model = TRUE,
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(u, 0.01)
        )
      )
    ) +
    
    # Temporal RW2 effect
    f(year_id,
      model = "rw2",
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(u, 0.01)
        )
      )
    ) +
    
    # Age effect
    f(age_id,
      model = "iid",
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(u, 0.01)
        )
      )
    ) +
    
    # Space-time interaction
    f(st_id,
      model = "iid",
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(u, 0.01)
        )
      )
    )
}


# Strong prior: c(1, 0.01)
formula_nb_strong <- make_nb_prior_formula(1)

res_nb_strong <- INLA::inla(
  formula_nb_strong,
  family = "nbinomial",
  data = model_data_inla,
  E = exposure,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  control.predictor = list(compute = TRUE)
)


# Moderate prior: c(2, 0.01)
formula_nb_moderate <- make_nb_prior_formula(2)

res_nb_moderate <- INLA::inla(
  formula_nb_moderate,
  family = "nbinomial",
  data = model_data_inla,
  E = exposure,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  control.predictor = list(compute = TRUE)
)


# Weak prior: c(5, 0.01)
formula_nb_weak <- make_nb_prior_formula(5)

res_nb_weak <- INLA::inla(
  formula_nb_weak,
  family = "nbinomial",
  data = model_data_inla,
  E = exposure,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  control.predictor = list(compute = TRUE)
)


############################################################
# MODEL COMPARISON: NB PRIOR SENSITIVITY
############################################################

prior_results_nb <- data.frame(
  
  Model = c(
    "Main NB model",
    "Strong prior",
    "Moderate prior",
    "Weak prior"
  ),
  
  DIC = c(
    res_nb_interaction_asfr$dic$dic,
    res_nb_strong$dic$dic,
    res_nb_moderate$dic$dic,
    res_nb_weak$dic$dic
  ),
  
  WAIC = c(
    res_nb_interaction_asfr$waic$waic,
    res_nb_strong$waic$waic,
    res_nb_moderate$waic$waic,
    res_nb_weak$waic$waic
  ),
  
  DIC_pD = c(
    res_nb_interaction_asfr$dic$p.eff,
    res_nb_strong$dic$p.eff,
    res_nb_moderate$dic$p.eff,
    res_nb_weak$dic$p.eff
  ),
  
  WAIC_pW = c(
    res_nb_interaction_asfr$waic$p.eff,
    res_nb_strong$waic$p.eff,
    res_nb_moderate$waic$p.eff,
    res_nb_weak$waic$p.eff
  )
)

print(prior_results_nb)

# Save the results
write.csv(
  prior_results_nb,
  file.path(nb_dir, "NB_Prior_Sensitivity_Results.csv"),
  row.names = FALSE
)



############################################################
# HEALTH-FACILITY SENSITIVITY ANALYSIS
# NB model excluding health-facility density
############################################################

formula_nb_no_hf <- births ~
  
  # Fixed effects
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
  
  # Spatial structured effect
  f(state_id,
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
  
  # Temporal RW2 effect
  f(year_id,
    model = "rw2",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  ) +
  
  # Age effect
  f(age_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  ) +
  
  # Space-time interaction
  f(st_id,
    model = "iid",
    hyper = list(
      prec = list(
        prior = "pc.prec",
        param = c(2, 0.01)
      )
    )
  )


# Fit the reduced NB model
res_nb_no_hf <- INLA::inla(
  formula_nb_no_hf,
  family = "nbinomial",
  data = model_data_inla,
  E = exposure,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  control.predictor = list(compute = TRUE)
)



############################################################
# COMPARISON: WITH AND WITHOUT HEALTH-FACILITY DENSITY
############################################################

hf_sensitivity_results <- data.frame(
  
  Model = c(
    "Full NB model (with HF density)",
    "Reduced NB model (without HF density)"
  ),
  
  DIC = c(
    res_nb_moderate$dic$dic,
    res_nb_no_hf$dic$dic
  ),
  
  WAIC = c(
    res_nb_moderate$waic$waic,
    res_nb_no_hf$waic$waic
  ),
  
  DIC_pD = c(
    res_nb_moderate$dic$p.eff,
    res_nb_no_hf$dic$p.eff
  ),
  
  WAIC_pW = c(
    res_nb_moderate$waic$p.eff,
    res_nb_no_hf$waic$p.eff
  )
)

print(hf_sensitivity_results)


# Save results
write.csv(
  hf_sensitivity_results,
  file.path(nb_dir, "NB_Health_Facility_Sensitivity.csv"),
  row.names = FALSE
)



############################################################
# MULTICOLLINEARITY DIAGNOSTIC
# VARIANCE INFLATION FACTOR (VIF)
# NEGATIVE BINOMIAL FERTILITY MODEL
############################################################

# Fixed-effect covariates in the final NB model

vif_vars_nb <- c(
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


# ----------------------------------------------------------
# Extract complete-case data
# ----------------------------------------------------------

vif_data_nb <- model_data_inla %>%
  dplyr::select(dplyr::all_of(vif_vars_nb)) %>%
  tidyr::drop_na()

# Check observations
cat(
  "Number of observations used for NB VIF:",
  nrow(vif_data_nb), "\n"
)


# ----------------------------------------------------------
# Calculate VIF manually for every predictor
# ----------------------------------------------------------

vif_values_nb <- sapply(vif_vars_nb, function(var) {
  
  other_vars <- setdiff(vif_vars_nb, var)
  
  auxiliary_formula <- stats::as.formula(
    paste(
      var,
      "~",
      paste(other_vars, collapse = " + ")
    )
  )
  
  auxiliary_model <- stats::lm(
    auxiliary_formula,
    data = vif_data_nb
  )
  
  R2 <- summary(auxiliary_model)$r.squared
  
  1 / (1 - R2)
})


# ----------------------------------------------------------
# Create VIF results table
# ----------------------------------------------------------

vif_results_nb <- data.frame(
  Covariate = vif_vars_nb,
  VIF = as.numeric(vif_values_nb)
)

# Sort from highest to lowest VIF

vif_results_nb <- vif_results_nb %>%
  dplyr::arrange(dplyr::desc(VIF))


# Display results

print(vif_results_nb)


# ----------------------------------------------------------
# Save VIF results
# ----------------------------------------------------------

saveRDS(
  vif_results_nb,
  file.path(nb_dir, "NB_VIF_results.rds")
)

write.csv(
  vif_results_nb,
  file.path(nb_dir, "NB_VIF_results.csv"),
  row.names = FALSE
)

cat("\nNB VIF results saved successfully.\n")


# ==========================================================
# VIF SUMMARY
# ==========================================================

cat("\nVIF SUMMARY:\n")

cat(
  "Minimum VIF:",
  min(vif_results_nb$VIF), "\n"
)

cat(
  "Maximum VIF:",
  max(vif_results_nb$VIF), "\n"
)


# Variables with VIF > 5

cat("\nVariables with VIF > 5:\n")

print(
  vif_results_nb %>%
    dplyr::filter(VIF > 5)
)


# Variables with VIF > 10

cat("\nVariables with VIF > 10:\n")

print(
  vif_results_nb %>%
    dplyr::filter(VIF > 10)
)


#####################################################################
## Computation of the random-effect variances for Table A4
####################################################################

### Extract the posterior mean variances
# Function to calculate posterior variance summaries
# directly from the precision marginal

summarise_variance_from_precision <- function(marg) {
  
  # Extract precision values and their posterior densities
  x <- marg[, 1]
  density <- marg[, 2]
  
  # Sort precision values in ascending order
  ord <- order(x)
  x <- x[ord]
  density <- density[ord]
  
  # Remove duplicated precision values, if any
  keep <- !duplicated(x)
  x <- x[keep]
  density <- density[keep]
  
  # Check that precision support is positive
  if (any(x <= 0)) {
    stop("Precision marginal contains non-positive values.")
  }
  
  # Calculate area under the density using trapezoidal integration
  dx <- diff(x)
  
  area <- sum(
    dx * (density[-1] + density[-length(density)]) / 2
  )
  
  # Normalise the density
  density <- density / area
  
  # Calculate cumulative distribution function
  cumulative <- c(
    0,
    cumsum(
      dx * (density[-1] + density[-length(density)]) / 2
    )
  )
  
  cumulative <- cumulative / tail(cumulative, 1)
  
  # Calculate precision quantiles by interpolation
  precision_quantile <- function(p) {
    approx(
      x = cumulative,
      y = x,
      xout = p,
      ties = "ordered",
      rule = 2
    )$y
  }
  
  # Posterior mean of variance: E(1 / precision)
  variance_mean <- sum(
    dx * (
      (density[-1] / x[-1]) +
        (density[-length(density)] / x[-length(x)])
    ) / 2
  )
  
  # Posterior second moment: E(1 / precision^2)
  variance_second_moment <- sum(
    dx * (
      (density[-1] / x[-1]^2) +
        (density[-length(density)] / x[-length(x)]^2)
    ) / 2
  )
  
  # Posterior standard deviation of variance
  variance_sd <- sqrt(
    max(0, variance_second_moment - variance_mean^2)
  )
  
  # Reciprocal transformation reverses quantile probabilities
  variance_low <- 1 / precision_quantile(0.975)
  variance_median <- 1 / precision_quantile(0.5)
  variance_high <- 1 / precision_quantile(0.025)
  
  data.frame(
    Variance_mean = variance_mean,
    Variance_sd = variance_sd,
    Variance_median = variance_median,
    Variance_low = variance_low,
    Variance_high = variance_high
  )
}


######### Apply to the four model

extract_variance_posterior_NB <- function(model, model_name) {
  
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
    
    stats <- summarise_variance_from_precision(marg)
    
    data.frame(
      Model = model_name,
      Random_effect = hname,
      stats
    )
  })
  
  dplyr::bind_rows(results)
}

variance_primary_NB <- extract_variance_posterior_NB(
  res_nb_interaction_asfr,
  "Main NB model"
)

variance_strong_NB <- extract_variance_posterior_NB(
  res_nb_strong,
  "Strong PC prior"
)

variance_moderate_NB <- extract_variance_posterior_NB(
  res_nb_moderate,
  "Moderate PC prior"
)

variance_weak_NB <- extract_variance_posterior_NB(
  res_nb_weak,
  "Weak PC prior"
)

posterior_variance_NB <- dplyr::bind_rows(
  variance_primary_NB,
  variance_strong_NB,
  variance_moderate_NB,
  variance_weak_NB
)

posterior_variance_NB <- posterior_variance_NB %>%
  dplyr::mutate(
    Random_effect = dplyr::recode(
      Random_effect,
      "Precision for state_id" = "Spatial",
      "Precision for year_id" = "Temporal",
      "Precision for age_id" = "Age",
      "Precision for st_id" = "Space-time interaction"
    )
  )

print(posterior_variance_NB, digits = 6)

##### Validation Check

# Check that all variance estimates and credible limits are positive
stopifnot(
  all(posterior_variance_NB$Variance_mean > 0),
  all(posterior_variance_NB$Variance_sd >= 0),
  all(posterior_variance_NB$Variance_low > 0),
  all(posterior_variance_NB$Variance_median > 0),
  all(posterior_variance_NB$Variance_high > 0)
)

# Check that credible limits are correctly ordered
stopifnot(
  all(
    posterior_variance_NB$Variance_low <
      posterior_variance_NB$Variance_median
  ),
  all(
    posterior_variance_NB$Variance_median <
      posterior_variance_NB$Variance_high
  )
)

cat("All variance estimates passed the positivity and ordering checks.\n")


#####################################################################
## SAVE NB RANDOM-EFFECT VARIANCE RESULTS
## Supplementary 
#####################################################################

# Create output directory if it does not already exist
if (!dir.exists(nb_dir)) {
  dir.create(nb_dir, recursive = TRUE)
}

# Save as CSV for supplementary table preparation
write.csv(
  posterior_variance_NB,
  file = file.path(
    nb_dir,
    "NB_Random_Effect_Variance.csv"
  ),
  row.names = FALSE
)

# Save as RDS to preserve the R data frame
saveRDS(
  posterior_variance_NB,
  file = file.path(
    nb_dir,
    "NB_Random_Effect_Variance.rds"
  )
)

cat(
  "\nNB random-effect variance results saved successfully.\n",
  "Files saved in:", nb_dir, "\n"
)


#####################################################################
## HEALTH-FACILITY SENSITIVITY ANALYSIS
## Comparison of fixed effects: Full and Reduced NB models
#####################################################################

# Extract fixed-effect posterior summaries from the full NB model
fixed_full_NB <- as.data.frame(
  res_nb_moderate$summary.fixed
)

fixed_full_NB$Covariate <- rownames(fixed_full_NB)

fixed_full_NB <- fixed_full_NB %>%
  dplyr::select(
    Covariate,
    mean,
    sd,
    `0.025quant`,
    `0.5quant`,
    `0.975quant`
  ) %>%
  dplyr::rename(
    Full_Mean = mean,
    Full_SD = sd,
    Full_Lower = `0.025quant`,
    Full_Median = `0.5quant`,
    Full_Upper = `0.975quant`
  )


# Extract fixed-effect posterior summaries from the reduced NB model
fixed_reduced_NB <- as.data.frame(
  res_nb_no_hf$summary.fixed
)

fixed_reduced_NB$Covariate <- rownames(fixed_reduced_NB)

fixed_reduced_NB <- fixed_reduced_NB %>%
  dplyr::select(
    Covariate,
    mean,
    sd,
    `0.025quant`,
    `0.5quant`,
    `0.975quant`
  ) %>%
  dplyr::rename(
    Reduced_Mean = mean,
    Reduced_SD = sd,
    Reduced_Lower = `0.025quant`,
    Reduced_Median = `0.5quant`,
    Reduced_Upper = `0.975quant`
  )


# Display the individual summaries
print(fixed_full_NB)
print(fixed_reduced_NB)


#####################################################################
## COMBINE FULL AND REDUCED MODEL FIXED-EFFECT ESTIMATES
#####################################################################

# Match covariates appearing in both models
fixed_effect_comparison_NB <- dplyr::full_join(
  fixed_full_NB,
  fixed_reduced_NB,
  by = "Covariate"
) %>%
  dplyr::mutate(
    Mean_Difference = Full_Mean - Reduced_Mean,
    
    Full_RR = exp(Full_Mean),
    Full_RR_Lower = exp(Full_Lower),
    Full_RR_Upper = exp(Full_Upper),
    
    Reduced_RR = exp(Reduced_Mean),
    Reduced_RR_Lower = exp(Reduced_Lower),
    Reduced_RR_Upper = exp(Reduced_Upper)
  ) %>%
  dplyr::arrange(
    match(
      Covariate,
      fixed_full_NB$Covariate
    )
  )

# Display the combined comparison
print(
  fixed_effect_comparison_NB,
  digits = 5
)


#####################################################################
## VALIDATION OF FIXED-EFFECT COMPARISON
#####################################################################

# Identify covariates in the full model but absent from the reduced model
setdiff(
  fixed_full_NB$Covariate,
  fixed_reduced_NB$Covariate
)

# Identify covariates in the reduced model but absent from the full model
setdiff(
  fixed_reduced_NB$Covariate,
  fixed_full_NB$Covariate
)

# Check the number of fixed effects in each model
nrow(fixed_full_NB)
nrow(fixed_reduced_NB)

#####################################################################
## SAVE HEALTH-FACILITY DENSITY SENSITIVITY ANALYSIS
## Supplementary Table A6
#####################################################################

# Save the complete comparison as CSV
write.csv(
  fixed_effect_comparison_NB,
  file = file.path(
    nb_dir,
    "NB_Health_Facility_Sensitivity_Comparison.csv"
  ),
  row.names = FALSE
)

# Save the complete comparison as RDS
saveRDS(
  fixed_effect_comparison_NB,
  file = file.path(
    nb_dir,
    "NB_Health_Facility_Sensitivity_Comparison.rds"
  )
)

# Save the individual full and reduced model summaries
saveRDS(
  fixed_full_NB,
  file = file.path(
    nb_dir,
    "NB_Full_Model_Fixed_Effects.rds"
  )
)

saveRDS(
  fixed_reduced_NB,
  file = file.path(
    nb_dir,
    "NB_Reduced_Model_Fixed_Effects.rds"
  )
)

cat(
  "\nHealth-facility sensitivity analysis saved successfully.\n",
  "Output directory:", nb_dir, "\n"
)

files_to_check <- c(
  "NB_Health_Facility_Sensitivity_Comparison.csv",
  "NB_Health_Facility_Sensitivity_Comparison.rds",
  "NB_Full_Model_Fixed_Effects.rds",
  "NB_Reduced_Model_Fixed_Effects.rds"
)

file.exists(
  file.path(nb_dir, files_to_check)
)


##############################################################
# Extract posterior interaction estimates by state and year
############################################################

# Join posterior estimates to the state-year index
interaction_state_year_NB <- interaction_posterior_NB %>%
  left_join(st_index_nb, by = "st_id") %>%
  left_join(state_lookup, by = "state_id") %>%
  select(
    state_id,
    REGNAME,
    survey_year,
    posterior_mean,
    posterior_sd,
    lower_95,
    median,
    upper_95
  ) %>%
  arrange(survey_year, state_id)

# Inspect the complete results
print(interaction_state_year_NB)

## Arrange the posterior means in a state-by-year table

interaction_means_wide_NB <- interaction_state_year_NB %>%
  select(REGNAME, survey_year, posterior_mean) %>%
  pivot_wider(
    names_from = survey_year,
    values_from = posterior_mean,
    names_prefix = "Year_"
  ) %>%
  arrange(REGNAME)

# Display the table
print(interaction_means_wide_NB, n = 37)

## Identify states with positive and negative posterior means for each year

interaction_direction_NB <- interaction_state_year_NB %>%
  mutate(
    interaction_direction = case_when(
      posterior_mean > 0 ~ "Positive",
      posterior_mean < 0 ~ "Negative",
      TRUE ~ "Zero"
    )
  ) %>%
  select(
    survey_year,
    REGNAME,
    posterior_mean,
    lower_95,
    upper_95,
    interaction_direction
  ) %>%
  arrange(survey_year, desc(posterior_mean))

# Display the results
print(interaction_direction_NB)

## Identify states whose 95% credible intervals exclude zero

interaction_significance_NB <- interaction_state_year_NB %>%
  mutate(
    credible_interval_status = case_when(
      lower_95 > 0 ~ "Credible positive interaction",
      upper_95 < 0 ~ "Credible negative interaction",
      TRUE ~ "Credible interval includes zero"
    )
  ) %>%
  select(
    survey_year,
    REGNAME,
    posterior_mean,
    lower_95,
    upper_95,
    credible_interval_status
  ) %>%
  arrange(survey_year, REGNAME)

print(interaction_significance_NB)

## Save the results as CSV files

write.csv(
  interaction_state_year_NB,
  file.path(nb_dir, "NB_State_Year_Interaction_Estimates.csv"),
  row.names = FALSE
)

write.csv(
  interaction_means_wide_NB,
  file.path(nb_dir, "NB_Interaction_Posterior_Means_Wide.csv"),
  row.names = FALSE
)

write.csv(
  interaction_significance_NB,
  file.path(nb_dir, "NB_Interaction_Credible_Interval_Status.csv"),
  row.names = FALSE
)

## This code extract and include the 2024 interaction estimates 
interaction_significance_NB %>%
  dplyr::filter(survey_year == 2024) %>%
  dplyr::arrange(REGNAME) %>%
  print()

























