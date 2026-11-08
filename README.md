# bayesian-spatiotemporal-fertility-nigeria
Bayesian spatio-temporal modelling of fertility patterns across Nigeria using NDHS 2003–2024 birth histories. Implements Poisson and Negative Binomial models in R-INLA, incorporating spatial, temporal, age-specific and space–time interactions, with model comparison, diagnostics, sensitivity analyses and predictive validation.


# Bayesian Spatio-Temporal Modelling of Fertility in Nigeria (2003–2024)

## 1. Project Overview

This repository contains the R codes and supporting materials for the Bayesian spatio-temporal analysis of fertility patterns in Nigeria using data from the Nigeria Demographic and Health Surveys (NDHS) conducted in 2003, 2008, 2013, 2018 and 2024.

The study investigates spatial and temporal variations in fertility across Nigerian states, accounting for demographic, socioeconomic and contextual factors. The modelling framework uses Bayesian hierarchical count models incorporating spatial, temporal, age-specific and space–time random effects.

The analysis was implemented using R and the Integrated Nested Laplace Approximation (INLA) framework.

## 2. Development of the Modelling Strategy

The analysis initially commenced with a Bayesian Poisson spatio-temporal model. The Poisson model served as the starting point for the investigation of fertility patterns and the assessment of spatial, temporal and demographic effects.

Following the initial manuscript submission, the first set of reviewers' comments was addressed using the Poisson modelling framework. These revisions involved additional model diagnostics, assessment of multicollinearity, examination and transformation of contextual covariates, sensitivity analyses and other model validation procedures.

In a subsequent round of peer review, a reviewer requested a comparison between the Poisson model and an alternative Negative Binomial specification. This comparison was undertaken to assess whether allowing for additional dispersion in the count data would improve model performance.

The comparative assessment indicated that the Negative Binomial model provided better overall model fit and predictive calibration based on the criteria considered. Consequently, the modelling strategy was revised, and the complete analysis was repeated using the Negative Binomial framework.

The revised Negative Binomial analysis incorporated the main model estimation, fixed-effect assessment, spatial and temporal effects, space–time interaction, model diagnostics, predictive validation, posterior fertility estimates and sensitivity analyses. The reviewer-requested analyses previously conducted under the Poisson framework were also revisited using the Negative Binomial model where applicable.

The final revised manuscript therefore adopts the Bayesian Negative Binomial spatio-temporal model with a space–time interaction as the preferred modelling framework. The original Poisson analysis is retained in this repository to document the development of the analysis and the model-comparison process.

## 3. Repository Structure

The project is organised into three main R scripts corresponding to the stages of model development and revision.

### Script 1: `01_Poisson_Model.R`

This script contains the initial Poisson modelling framework and the associated data preparation procedures.

Major components include:

- Importation and harmonisation of the NDHS data across survey years.
- Preparation of birth and exposure data.
- Construction and integration of demographic, socioeconomic and contextual covariates.
- Preparation of state-level spatial data and adjacency structures.
- Estimation of baseline and extended Bayesian Poisson models.
- Incorporation of spatial, temporal, age-specific and space–time random effects.
- Model comparison using the Deviance Information Criterion (DIC) and Watanabe–Akaike Information Criterion (WAIC).
- Model diagnostics and posterior estimation of fertility indicators.
- Sensitivity assessment using alternative prior specifications.

This script represents the initial modelling stage and provides the foundation for the subsequent Negative Binomial analysis.

### Script 2: `02_Negative_Binomial_Model.R`

This script contains the Negative Binomial modelling framework developed following the comparative assessment of the Poisson and Negative Binomial models.

Major components include:

- Preparation of the data required for Negative Binomial estimation.
- Estimation of baseline, additive and space–time interaction Negative Binomial models.
- Model comparison and assessment of goodness of fit.
- Comparison of Poisson and Negative Binomial model diagnostics.
- Posterior estimation of fixed effects and rate ratios.
- Extraction and visualisation of spatial, temporal and space–time random effects.
- Leave-one-state-out (LOSO) predictive validation.
- Estimation of age-specific fertility rates and state-specific and national total fertility rates.
- Assessment of prior sensitivity.
- Sensitivity analysis involving health-facility density.
- Multicollinearity assessment and posterior variance comparisons.

The space–time interaction Negative Binomial model constitutes the preferred model in the revised analysis.

### Script 3: `03_Model_Revision_and_Validation.R`

This script contains additional analyses undertaken in response to the reviewers' comments and subsequent manuscript revisions.

It includes the additional diagnostic, sensitivity and validation procedures initially developed during the Poisson revision stage, together with the corresponding revised analyses under the Negative Binomial framework where applicable.

The script covers:

- Assessment of multicollinearity.
- Examination and standardisation of contextual covariates.
- Comparison of models with and without health-facility density.
- Additional model diagnostics, including PIT and dispersion assessments.
- Predictive validation using leave-one-state-out procedures.
- Sensitivity assessment under alternative penalised complexity (PC) prior specifications.
- Checks of dataset dimensions, completeness and model structure.
- Supporting analyses for the revised manuscript.

This script documents the additional revision and validation work undertaken during the peer-review process.

## 4. Order of Execution

The scripts should be executed in the following order:

1. `01_Poisson_Model.R`
2. `02_Negative_Binomial_Model.R`
3. `03_Model_Revision_and_Validation.R`

The Poisson script establishes the initial data preparation and modelling workflow. The Negative Binomial script uses the processed data and supporting objects generated during the earlier stage. The revision and validation script subsequently uses saved datasets, model objects and results from the preceding analyses.

Users should therefore preserve the execution order and ensure that the required intermediate datasets and model outputs are available before proceeding to subsequent scripts.

## 5. Data Sources

The study uses data from five rounds of the Nigeria Demographic and Health Surveys:

- 2003 NDHS
- 2008 NDHS
- 2013 NDHS
- 2018 NDHS
- 2024 NDHS

Additional contextual data were incorporated from relevant sources, including population estimates, satellite-derived night-time lights, conflict events, health-facility information and other spatial covariates.

The DHS datasets are subject to access and use conditions. The original individual-level DHS files are not included in this repository. Researchers wishing to reproduce the analysis should obtain the required datasets through the appropriate authorised data-access procedures.

Supporting spatial and contextual datasets should be obtained from their respective sources, subject to applicable access conditions.

## 6. Working Directory and File Paths

The scripts were originally developed and executed on a Windows computer. Consequently, some file paths reflect the author's original working environment.

**Important:** Before executing any script, users must locate the `setwd()` statement at the beginning of the script and replace the existing directory with the full path to the project folder on their own computer.

For example:

```r
setwd("C:/Users/YourName/Documents/TRFPROJECT_EDIT")
```

The example path should be replaced with the actual location of the project directory.

Users should also ensure that the datasets, spatial files, processed data and supporting folders are organised in the locations expected by the scripts.

## 7. Software and Packages

The analyses were conducted using R, with the INLA framework for Bayesian hierarchical modelling.

The scripts require several R packages for data management, spatial analysis, statistical modelling and visualisation. These include, but are not limited to:

- `INLA`
- `tidyverse`
- `dplyr`
- `sf`
- `spdep`
- `ggplot2`
- `haven`
- `terra`
- `raster`
- `readxl`
- `patchwork`
- `RColorBrewer`
- `exactextractr`

Some packages may require additional system dependencies. INLA should be installed using the official installation instructions.

Researchers should ensure that the required packages are installed and compatible with their R environment before executing the scripts.

## 8. Reproducibility and Interpretation

The repository is intended to support transparency and reproducibility of the statistical analyses reported in the associated research manuscript.

The original Poisson analysis is retained to document the initial modelling strategy and the subsequent model-comparison process. The Negative Binomial analysis represents the final preferred modelling framework adopted in the revised manuscript.

Differences between intermediate results and final manuscript results should be interpreted in the context of the documented model revisions and the transition from the Poisson to the Negative Binomial framework.

Researchers reproducing the results should follow the execution sequence and use the appropriate survey datasets, supporting covariates and software environment.

## 9. Citation

If you use the code or adapt the modelling framework in your research, please cite the associated research article or preprint.

**Manuscript title:** [{Spatio–Temporal Dynamics of Fertility in Nigeria: A Bayesian Negative Binomial Analysis of DHS Birth Histories]

**Authors:** [Insert author names]

**Journal/Preprint:** [Insert publication or preprint details when available]

**Repository:** [Insert GitHub repository link]

## 10. Contact

For questions concerning the code, modelling framework or reproducibility of the analyses, please contact the corresponding author through the contact details provided in the associated manuscript.
