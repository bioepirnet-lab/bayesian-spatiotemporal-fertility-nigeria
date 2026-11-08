#################################################################
#load libraries
################################################################
library(dplyr)
library(sf)
library(INLA)
library(sp)
library(leaflet)
library(ggplot2)
library(gridExtra)
library(lattice)
library(RColorBrewer)
library(splancs)
library(fields)
library(readxl)  # For reading Excel files
library(tidyr)
library(raster)
library(tmap)
library(spdep)
library(patchwork)
library(haven)
library(tidyverse)
library(purrr)
library(terra)
library(readr)

#################################################################
#### Step 1: Define a Standard Harmonized State List (1–37)
state_master <- data.frame(
  state_code = 1:37,
  state_name = c(
    "Sokoto","Zamfara","Katsina","Jigawa","Yobe","Borno","Adamawa","Gombe","Bauchi",
    "Kano","Kaduna","Kebbi","Niger","FCT","Nassarawa","Plateau","Taraba","Benue",
    "Kogi","Kwara","Oyo","Osun","Ekiti","Ondo","Edo","Anambra","Enugu","Ebonyi",
    "Cross River","Akwa Ibom","Abia","Imo","Rivers","Bayelsa","Delta","Lagos","Ogun"
  )
)

View(state_master)

#### Step 2: Create State-Mapping Tables for Each Survey Year

### A. 2024 mapping (1–37 and 96)

map_2024 <- data.frame(
  raw = c(1:37, 96),
  state_code = c(1:37, NA)  # 96 = Abroad → remove later
)

View(map_2024)

#### B. 2018, 2013, 2008 mapping (10–370)

### Each state is “code ÷ 10”.

map_2018_2013_2008 <- data.frame(
  raw = seq(10, 370, by = 10),
  state_code = seq(1, 37)    # simply raw / 10
)
View(map_2018_2013_2008)

#### C. 2003 mapping (1–37) different order

### We must manually map the 2003 codes to the master codes.

### Your 2003 mapping (raw → master code):

map_2003 <- data.frame(
  raw = 1:37,
  state_code = c(
    30, # 1 – Akwa Ibom
    26, # 2 – Anambra
    9, # 3 – Bauchi
    25, # 4 – Edo
    18, # 5 – Benue
    6, # 6 – Borno
    29, # 7 – Cross River
    7, # 8 – Adamawa
    32, # 9 – Imo
    11, # 10 – Kaduna
    10, # 11 – Kano
    3, # 12 – Katsina
    20, # 13 – Kwara
    36, # 14 – Lagos
    13, # 15 – Niger
    37, # 16 – Ogun
    24, # 17 – Ondo
    21, # 18 – Oyo
    16, # 19 – Plateau
    33, # 20 – Rivers
    1, # 21 – Sokoto
    31, # 22 – Abia
    35, # 23 – Delta
    27, # 24 – Enugu
    4, # 25 – Jigawa
    12, # 26 – Kebbi
    19, # 27 – Kogi
    22, # 28 – Osun
    17, # 29 – Taraba
    5, # 30 – Yobe
    34, # 31 – Bayelsa
    28, # 32 – Ebonyi
    23, # 33 – Ekiti
    8, # 34 – Gombe
    15, # 35 – Nassarawa
    2, # 36 – Zamfara
    14  # 37 – Abuja (FCT)
  )
)
View(map_2003)

#### Step 3: Import DHS Data Year-by-Year in R

needed_vars <- c(
  "v001","v002","v003","v005","v021","v022","v024","v102","v130","v106","v190","v501",
  "v012","v008","v007",
  paste0("bidx", sprintf("%02d", 1:20)),
  paste0("b3",   sprintf("%02d", 1:20)),
  paste0("b2",   sprintf("%02d", 1:20)),
  paste0("b0",   sprintf("%02d", 1:20)),
  "sstate"  # state variable
)

### Step 4: R Function to Import and Harmonize State Codes

read_dhs_year <- function(path, year) {
  
  df <- haven::read_sav(path)
  names(df) <- tolower(names(df))
  
  # select only variables that exist in this file
  vars <- needed_vars[tolower(needed_vars) %in% names(df)]
  df <- dplyr::select(df, dplyr::all_of(vars))
  
  # rename state
  df <- dplyr::rename(df, raw_state = sstate)
  
  # state matching
  if (year == 2024) {
    df <- dplyr::left_join(df, map_2024, by = c("raw_state" = "raw"))
  }
  
  if (year %in% c(2018, 2013, 2008)) {
    df <- dplyr::left_join(df, map_2018_2013_2008, by = c("raw_state" = "raw"))
  }
  
  if (year == 2003) {
    df <- dplyr::left_join(df, map_2003, by = c("raw_state" = "raw"))
  }
  
  df$survey_year <- year
  
  return(df)
}

#### Step 5: Import Each Year Manually

### Replace the file paths with your own:

d2003 <- read_dhs_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGIR4BFL.sav", 2003)
d2008 <- read_dhs_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGIR52FL.sav", 2008)
d2013 <- read_dhs_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGIR6AFL.sav", 2013)
d2018 <- read_dhs_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGIR7AFL.sav", 2018)
d2024 <- read_dhs_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGIR8BFL.sav", 2024)


### Create a list and combine them

ir_list <- list(d2003, d2008, d2013, d2018, d2024)

# Add survey year identifier already inside each object from read_dhs_year
ir_all <- bind_rows(ir_list)

View(ir_all)
head(ir_all)

#################################################################
## Load Nigeria shapefile and Prepare a clean state lookup table.
#################################################################

# Nigeria shapefile with state boundaries
nga_states <- st_read("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/sdr_subnational_boundaries_2023-08-20/shps/sdr_subnational_boundaries2.shp")

state_lookup <- nga_states %>%
  st_drop_geometry() %>%
  dplyr::select(REGCODE, REGNAME)
head(state_lookup)

## merge ir_all with REGCODE AND REGNAME

ir_all <- ir_all %>%
  left_join(state_lookup, by = c("state_code" = "REGCODE"))

ir_all <- ir_all %>%
  mutate(REGCODE = state_code)
View(ir_all)

###############################################################
## Define the variables to import from BR files
###############################################################

# Core identifiers
id_vars <- c("v001", "v002", "v003", "v005", 
             "v012", "v007", "v008")

# Birth history variables: 20 slots each
birth_vars <- c(
  paste0("bidx_", sprintf("%02d", 1:20)),
  paste0("b2_",   sprintf("%02d", 1:20)),
  paste0("b3_",   sprintf("%02d", 1:20)),
  paste0("b5_",   sprintf("%02d", 1:20)),  # optional (survival)
  paste0("b0_",   sprintf("%02d", 1:20)),  # optional (twin)
  paste0("b1_",   sprintf("%02d", 1:20))   # optional (sex)
)

needed_br_vars <- c(id_vars, birth_vars)

### Create a function to import each BR file. This function:
### Converts names to lowercase

## Selects only variables that exist

## Adds the survey year

read_br_year <- function(path, year) {
  
  df <- haven::read_sav(path)
  names(df) <- tolower(names(df))
  
  # identify all birth-history variables 
  birth_vars <- grep("^b", names(df), value = TRUE)
  
  needed <- c(
    "v001", "v002", "v003", "v005", "v007", "v008",
    birth_vars
  )
  
  df <- df %>%
    dplyr::select(dplyr::any_of(needed)) %>%
    dplyr::mutate(survey_year = year)
  
  return(df)
}


# -----------------------------------------
# READ ALL BR FILES
# -----------------------------------------

br2003 <- read_br_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGBR4BFL.sav", 2003)
br2008 <- read_br_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGBR52FL.sav", 2008)
br2013 <- read_br_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGBR6AFL.sav", 2013)
br2018 <- read_br_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGBR7AFL.sav", 2018)
br2024 <- read_br_year("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/TRFPROJECT_EDIT/NGBR8BFL.sav", 2024)

## Add survey year to BR data before merging

br2003$survey_year <- 2003
br2008$survey_year <- 2008
br2013$survey_year <- 2013
br2018$survey_year <- 2018
br2024$survey_year <- 2024

View(br2003)

br_all <- bind_rows(br2003, br2008, br2013, br2018, br2024)
View(br_all)

head(br_all)
head(ir_all)

#### Merge with IR covariates 

br_ir <- br_all %>%
  left_join(
    ir_all %>% 
      dplyr::select(v001, v002, v003, survey_year, REGCODE, REGNAME,
             v012, v024, v102, v130, v106, v190, v501), 
    by = c("v001", "v002", "v003", "survey_year")
  )

View(br_ir)

table(br_ir$REGNAME)
table(br_ir$REGCODE)

nrow(br_ir)

names(br_ir)

#### Saved br_ir data

saveRDS(br_ir, "br_ir_clean_data.rds")

###### To reload

br_ir <- readRDS("br_ir_clean_data.rds")

#################################################################
#### AREA-LEVEL COVARIATES

### 1. Population Density (WorldPop) - Source: WorldPop
###Extract mean population density per state for each survey year

### For survey year 2003

pop2003_raster <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/worldpop_2003.tif")

### Extract the data
pop2003_state <- terra::extract(pop2003_raster, nga_states, fun = mean, na.rm = TRUE)

pop2003_state$year <- 2003    # assign year manually

head(pop2003_state)

### For survey year 2008

pop2008_raster <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/worldpop_2008.tif")


pop2008_state <- terra::extract(pop2008_raster, nga_states, fun = mean, na.rm = TRUE)

pop2008_state$year <- 2008    # assign year manually

head(pop2008_state)

### For survey year 2013

pop2013_raster <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/worldpop_2013.tif")

pop2013_state <- terra::extract(pop2013_raster, nga_states, fun = mean, na.rm = TRUE)

pop2013_state$year <- 2013    # assign year manually

head(pop2013_state)

### For survey year 2018

pop2018_raster <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/worldpop_2018.tif")

pop2018_state <- terra::extract(pop2018_raster, nga_states, fun = mean, na.rm = TRUE)

pop2018_state$year <- 2018    # assign year manually
head(pop2018_state)
### For survey year 2024

pop2024_raster <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/worldpop_2024.tif")

pop2024_state <- terra::extract(pop2024_raster, nga_states, fun = mean, na.rm = TRUE)

pop2024_state$year <- 2024    # assign year manually

head(pop2024_state)

##### Add REGCODE + REGNAME and clean each dataset

add_state_info <- function(df, nga_states) {
  df %>%
    mutate(
      REGCODE = nga_states$REGCODE[ID],
      REGNAME = nga_states$REGNAME[ID]
    ) %>%
    dplyr::select(REGCODE, REGNAME, year, pop_density = 2)   # rename density column
}

#### Apply function to all years

pop2003_clean <- add_state_info(pop2003_state, nga_states)
pop2008_clean <- add_state_info(pop2008_state, nga_states)
pop2013_clean <- add_state_info(pop2013_state, nga_states)
pop2018_clean <- add_state_info(pop2018_state, nga_states)
pop2024_clean <- add_state_info(pop2024_state, nga_states)

#### Combine into a population density panel

pop_state_panel <- bind_rows(
  pop2003_clean,
  pop2008_clean,
  pop2013_clean,
  pop2018_clean,
  pop2024_clean
)

head(pop_state_panel)

################################################################
### Importing light data into R

# 2003 (F14)
nl_2003 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/nightlight_2003.tif")

# 2008 (F16)
nl_2008 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/nightlight_2008.tif")

# 2013 VIIRS
nl_2013 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/nightlight_2013.tif")

# 2018 VIIRS
nl_2018 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/nightlight_2018.tif")

# 2024 VIIRS 
nl_2024 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/nightlight_2024.tif")

#### Crop and masked with Nigeria map

crop_mask <- function(r, boundary){
  r <- crop(r, boundary)
  r <- mask(r, boundary)
  return(r)
}

nl03 <- crop_mask(nl_2003, nga_states)
nl08 <- crop_mask(nl_2008, nga_states)
nl13 <- crop_mask(nl_2013, nga_states)
nl18 <- crop_mask(nl_2018, nga_states)
nl24 <- crop_mask(nl_2024, nga_states)

### Plot to verify

plot(nl03, main="Night Lights 2003")
plot(nl13, main="Night Lights 2013")
plot(nl24, main="Night Lights 2024")

#### Extraction function

extract_state_mean <- function(raster, shp, year) {
  
  m <- terra::extract(raster, shp, fun = mean, na.rm = TRUE)
  
  # rename raster column to nightlights (usually lyr1 or mean)
  raster_col <- setdiff(names(m), "ID")
  names(m)[names(m) == raster_col] <- "nightlights"
  
  m$year <- year
  
  out <- cbind(
    shp[, c("REGCODE", "REGNAME")],
    m[, c("nightlights", "year")]
  )
  
  return(as.data.frame(out))
}

#### Extract night light for each state

nl_state_2003 <- extract_state_mean(nl03, nga_states, 2003)
nl_state_2008 <- extract_state_mean(nl08, nga_states, 2008)
nl_state_2013 <- extract_state_mean(nl13, nga_states, 2013)
nl_state_2018 <- extract_state_mean(nl18, nga_states, 2018)
nl_state_2024 <- extract_state_mean(nl24, nga_states, 2024)

###### Add year

nl_state_2003$year <- 2003
nl_state_2008$year <- 2008
nl_state_2013$year <- 2013
nl_state_2018$year <- 2018
nl_state_2024$year <- 2024


night_all_states <- bind_rows(
  nl_state_2003,
  nl_state_2008,
  nl_state_2013,
  nl_state_2018,
  nl_state_2024
) %>%
  dplyr::select(REGCODE, REGNAME, year, everything())

View(night_all_states)
nrow(night_all_states)
head(night_all_states)

##################################################################
#### Area Covariates - 3.	Conflict Index (ACLED)

#### Load the ACLED CSV into R
acled <- read.csv("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/conflictData.csv", stringsAsFactors = FALSE)

View(acled)
head(acled)

### Convert ACLED to sf points

acled_sf <- st_as_sf(
  acled,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

#### Ensure your Nigeria state shapefile is sf

# nga_states must be sf with a REGCODE or state_code for joining
st_geometry(nga_states)

### Spatial join: assign each event to a state

acled_joined <- st_join(acled_sf, nga_states, join = st_within)

##### Compute annual conflict counts per state

conflict_state_year <- acled_joined %>%
  st_drop_geometry() %>%
  group_by(REGCODE, REGNAME, year) %>%
  summarise(
    conflict_events = n(),
    civilian_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  ungroup()

head(conflict_state_year)
table(conflict_state_year$year)


## Create the full 37 × 22 state-year panel inline with the survey year

dhs_years <- c(2003, 2008, 2013, 2018, 2024)

# List of all REGCODE–REGNAME from your Nigeria shapefile 
state_lookup <- nga_states %>% 
  st_drop_geometry() %>%
  dplyr::select(REGCODE, REGNAME) %>% 
  distinct()

# Years covered in your ACLED download
years <- dhs_years

# Create complete state-year panel
state_year_panel <- expand_grid(
  REGCODE = state_lookup$REGCODE,
  year = years
) %>%
  left_join(state_lookup, by = "REGCODE") %>% 
  left_join(conflict_state_year, by = c("REGCODE", "year", "REGNAME")) %>%
  mutate(
    conflict_events = replace_na(conflict_events, 0),
    civilian_fatalities = replace_na(civilian_fatalities, 0)
  ) %>%
  arrange(REGCODE, year)

head(state_year_panel)
nrow(state_year_panel)
View(state_year_panel)


### FULL R SCRIPT TO GENERATE HEALTH FACILITY DENSITY (2003–2024)


# ================================
# 2. Load NHFR 2024 health facilities
# ================================
hf_2024 <- read_csv("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/healthDensityData.csv")

# Convert to spatial points
hf_sf <- st_as_sf(
  hf_2024,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# ================================
# 4. Attach each health facility to a state
# ================================
hf_state <- st_join(hf_sf, nga_states) %>%
  st_drop_geometry() %>%
  count(REGCODE, REGNAME, name = "hf_count_2024")

# ================================
# 5. Load population panel
# ================================
# You prepared: popCount_2003, popCount_2008, popCount_2013, popCount_2018, popCount_2024
# Combine them first:

popCount_2003 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/popCount_2003.tif")
popCount_2008 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/popCount_2008.tif")
popCount_2013 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/popCount_2013.tif")
popCount_2018 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/popCount_2018.tif")
popCount_2024 <- rast("C:/Users/DELL/Desktop/LaTeX-1/FERTILITY/TFRData/External_Covariates/popCount_2024.tif")

### Extract population per state for each raster 
library(exactextractr)

extract_pop <- function(raster, year) {
  raw <- exact_extract(raster, nga_states, 'sum')
  data.frame(population = raw,
             REGCODE = nga_states$REGCODE,
             REGNAME = nga_states$REGNAME,
             year = year)
}

pop2003_state <- extract_pop(popCount_2003, 2003)
pop2008_state <- extract_pop(popCount_2008, 2008)
pop2013_state <- extract_pop(popCount_2013, 2013)
pop2018_state <- extract_pop(popCount_2018, 2018)
pop2024_state <- extract_pop(popCount_2024, 2024)


state_pop <- bind_rows(
  pop2003_state,
  pop2008_state,
  pop2013_state,
  pop2018_state,
  pop2024_state
)

View(state_pop)
# state_pop must contain:
# REGCODE | REGNAME | year | population

# ================================
# 6. Extract 2024 population for interpolation reference
# ================================
pop_2024 <- state_pop %>%
  filter(year == 2024) %>%
  dplyr::select(REGCODE, pop_2024 = population)

# ================================
# 7. Merge facility counts + population + pop_2024
# ================================
df <- state_pop %>%
  left_join(hf_state, by = c("REGCODE", "REGNAME")) %>%
  left_join(pop_2024, by = "REGCODE")

# ================================
# 8. INTERPOLATE health facility density
# ================================

df <- df %>%
  mutate(
    hf_density = (hf_count_2024 * (population / pop_2024)) / 100000
  )

# ================================
# 9. Final dataset (your panel)
# ================================
health_facility_density <- df %>%
  dplyr::select(
    REGCODE, REGNAME, year, 
    population,
    hf_count_2024,
    hf_density
  ) %>%
  arrange(REGCODE, year)

# Preview
print(head(health_facility_density))

# Save to CSV
write_csv(
  health_facility_density,
  "health_facility_density_2003_2024.csv"
)

######## Next is to build compute TFR by building an 
## age × state × survey-year fertility table where we can
## obtain the birth count which serve as response variable.

### Define age groups

br_ir <- br_ir %>%
  mutate(
    age_group = cut(v012,
                    breaks = c(15,20,25,30,35,40,45,50),
                    right = FALSE,
                    labels = c("15-19","20-24","25-29","30-34",
                               "35-39","40-44","45-49"))
  )

View(br_ir)

#Compute birth counts Y_{s,a,t}-Births in the last 5 years (weighted)

# 1. Weighted birth counts by state × age group × survey year

# Births in the last 0–59 months, weighted by women's sampling weight
## Note: There was no birth record =<59(below 5 yrs) for 2003, 2008 &
## 2013. so will computed it as follow.
br_ir <- br_ir %>%
  mutate(
    b19_calc = v008 - b3     # age in months at interview
  )
 
## And then compute the birth count.

br_births <- br_ir %>%
  filter(b19_calc <= 59) %>%     # births in last 5 years
  group_by(REGCODE, REGNAME, survey_year, age_group) %>%
  summarise(
    births = n(),                #  INTEGER COUNTS
    .groups = "drop"
  )

head(br_births)


#################################################################

## obtain Women exposure (5-year window) from IR

### First add REGCODE and REGNAME to ir file

ir_all <- ir_all %>%
  mutate(
    age_group = cut(v012,
                    breaks = c(15,20,25,30,35,40,45,50),
                    right = FALSE,
                    labels = c("15-19","20-24","25-29","30-34",
                               "35-39","40-44","45-49"))
  )

View(ir_all)
# Women exposure (5-year) by state × age group × survey year
women_exposure <- ir_all %>%
  mutate(
    weight = v005 / 1e6                   # DHS women’s weight
  ) %>%
  group_by(REGCODE, REGNAME, survey_year, age_group) %>%
  summarise(
    exposure = 5 * sum(weight, na.rm = TRUE),  # 5 years × number of women
    .groups = "drop"
  )

head(women_exposure)

#### Merge births + exposure → ASFR

# Merge births and exposure

fertility_data <- br_births %>%
  left_join(
    women_exposure,
    by = c("REGCODE", "REGNAME", "survey_year", "age_group")
  ) %>%
  mutate(
    asfr = births / exposure
  )


##### Compute TFR
TFR_state_year <- fertility_data %>%
  group_by(REGCODE, REGNAME, survey_year) %>%
  summarise(
    TFR = 5 * sum(asfr, na.rm = TRUE),
    .groups = "drop"
  )

summary(TFR_state_year$TFR)
print(head(TFR_state_year))

###################################################################
#### Aggregate the Individual level covariates

### Start by Prepare weights

ir_all <- ir_all %>%
  mutate(weight = v005 / 1e6)  # scale DHS weights

## Define a helper function for weighted proportions

## This avoids repeating code.

weighted_prop <- function(x, w) {
  sum(w * x, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

##FULL SCRIPT — Individual-level covariate aggregation

individual_covariates <- ir_all %>%
  group_by(REGCODE, REGNAME, survey_year, age_group) %>%
  summarise(
    
    # ---- Urbanicity ----
    prop_urban = weighted_prop(v102 == 1, weight),
    
    # ---- Education ----
    prop_noedu     = weighted_prop(v106 == 0, weight),
    prop_primary   = weighted_prop(v106 == 1, weight),
    prop_secondary = weighted_prop(v106 == 2, weight),
    prop_higher    = weighted_prop(v106 == 3, weight),
    
    # ---- Wealth ----
    prop_poorer1 = weighted_prop(v190 == 1, weight),
    prop_poorer2 = weighted_prop(v190 == 2, weight),
    prop_middle  = weighted_prop(v190 == 3, weight),
    prop_richer  = weighted_prop(v190 == 4, weight),
    prop_richest = weighted_prop(v190 == 5, weight),
    
    # ---- Marital Status ----
    prop_never_married     = weighted_prop(v501 == 0, weight),
    prop_currently_married = weighted_prop(v501 == 1, weight),
    prop_former_married    = weighted_prop(v501 == 2, weight),
    
    # ---- Continuous covariate ----
    mean_age = sum(weight * v012, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    
    .groups = "drop"
  )

names(individual_covariates)
view(individual_covariates)

### Merge Individual level data with area data.

## Harmonize the year variable

## fertility_data uses survey_year, but the area covariates use year.

pop_state_panel  <- pop_state_panel  %>% rename(survey_year = year)
night_all_states <- night_all_states %>% rename(survey_year = year)
state_year_panel <- state_year_panel %>% rename(survey_year = year)
health_facility_density <- health_facility_density %>% rename(survey_year = year)

view(pop_state_panel)
view(state_year_panel)
view(health_facility_density)

head(pop_state_panel)

#### Merge fertility_data with all area-level covariates

model_data <- fertility_data %>%
  left_join(pop_state_panel,
            by = c("REGCODE", "REGNAME", "survey_year")) %>%
  left_join(night_all_states %>% st_drop_geometry(),
            by = c("REGCODE", "REGNAME", "survey_year")) %>%
  left_join(state_year_panel,
            by = c("REGCODE", "REGNAME", "survey_year")) %>%
  left_join(health_facility_density,
            by = c("REGCODE", "REGNAME", "survey_year"))

View(model_data)

### Add TFR (if needed)
model_data <- model_data %>%
  left_join(TFR_state_year,
            by = c("REGCODE", "REGNAME", "survey_year"))


head(model_data)

View(model_data)

### add individual covriates with model_data
 model_data <- model_data %>%
left_join(individual_covariates,
          by = c("REGCODE", "REGNAME", "survey_year", "age_group"))

 ### Verify the merge
 
 head(model_data)
 summary(model_data$prop_secondary)
 summary(model_data$prop_urban)
 
View(model_data)

nrow(model_data)
model_data=na.omit(model_data)

############## Saved Model Data ##########################

saveRDS(model_data, "model_data_clean_data.rds")

###### To reload

model_data <- readRDS("model_data_clean_data.rds")


################# End of Data Preparation #######################

####### Beginning of  Bayesian Analysis -  Create ID

model_data_inla <- model_data %>%
  st_drop_geometry() %>%     # critical
  na.omit() %>%              # OK (loss < 1%)
  mutate(
    state_id = as.integer(factor(REGCODE)),
    year_id  = as.integer(factor(survey_year)),
    age_id   = as.integer(factor(age_group))
  )

head(model_data_inla)
View(model_data_inla)


### create adjacent matrix

nb <- poly2nb(nga_states)
nb2INLA("adjacency.graph", nb)

g <- inla.read.graph("adjacency.graph")
g

##############################################################
### Baseline Model
#############################################################

res0 <- inla(
  births ~ 1,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,   # ✅ correct way, NOT offset
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

summary(res0)

######################################################
### Baseline demographic model (age + time)
######################################################

formula_1 <- births ~ 1 +
  f(age_id,  model = "iid") +
  f(year_id, model = "rw2")

res1 <- inla(
  formula_1,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

summary(res1)

##########################################################
### Add individual-level aggregated covariates
##########################################################

formula_2 <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  
  f(age_id,  model = "iid") +
  f(year_id, model = "rw2")

res2 <- inla(
  formula_2,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

summary(res2)

#############################################################
### Add area-level covariates
#############################################################

formula_3 <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  f(age_id,  model = "iid") +
  f(year_id, model = "rw2")

res3 <- inla(
  formula_3,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

summary(res3)

######################################################
### Full spatial–temporal model 
######################################################

formula_full <- births ~
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  f(state_id, model = "besag", graph = g, scale.model = TRUE) +
  f(year_id,  model = "rw2") +
  f(age_id,   model = "iid")

res_full <- inla(
  formula_full,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(res_full)

### Interaction Model
## First prepare the indices for interation
model_data_inla <- model_data_inla %>%
  mutate(
    st_id = as.integer(interaction(state_id, year_id, drop = TRUE))
  )

### Quick check
length(unique(model_data_inla$st_id))
# should be 37 × 5 = 185 (or fewer if some state-years absent)

formula_interaction <- births ~
  # Fixed effects
  prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  # Spatial structured effect
  f(state_id, model = "besag", graph = g, scale.model = TRUE) +
  
  # Temporal smooth effect
  f(year_id, model = "rw2") +
  
  # Age-group effect
  f(age_id, model = "iid") +
  
  # Space–time interaction (iid)
  f(st_id, model = "iid")

res_interaction <- inla(
  formula_interaction,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(res_interaction)

#### Comparisim Matrics

data.frame(
  Model = c("Null", "Full", "Full + Interaction"),
  DIC   = c(res0$dic$dic,
            res_full$dic$dic,
            res_interaction$dic$dic),
  WAIC  = c(res0$waic$waic,
            res_full$waic$waic,
            res_interaction$waic$waic)
)

##################################################################
# forest plot of posterior rate ratios with 95% credible intervals
# Extract fixed effects and compute rate ratios
#################################################################

# Extract fixed effects summary
fixef <- as.data.frame(res_interaction$summary.fixed)

fixef <- fixef %>%
  tibble::rownames_to_column("covariate") %>%
  mutate(
    RR      = exp(mean),
    RR_low  = exp(`0.025quant`),
    RR_high = exp(`0.975quant`)
  ) %>%
  # Remove intercept
  filter(covariate != "(Intercept)")

### Relabel covariates for readability 

fixef$covariate <- factor(
  fixef$covariate,
  levels = rev(fixef$covariate),
  labels = rev(c(
    "Urban residence (prop)",
    "Secondary education (prop)",
    "Higher education (prop)",
    "Middle wealth (prop)",
    "Richer wealth (prop)",
    "Richest wealth (prop)",
    "Currently married (prop)",
    "Mean age",
    "Population density",
    "Night-time lights",
    "Conflict events",
    "Health facility density"
  ))
)

### Forest plot of posterior rate ratios

ggplot(fixef, aes(x = RR, y = covariate)) +
  geom_point(size = 3, color = "#1F78B4") +
  geom_errorbarh(
    aes(xmin = RR_low, xmax = RR_high),
    height = 0.2,
    color = "#A6CEE3",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  scale_x_log10() +
  labs(
    x = "Rate Ratio (log scale)",
    y = NULL,
    #title = "Posterior Rate Ratios for Fertility Determinants",
    #subtitle = "Bayesian Poisson spatio-temporal model with space–time interaction"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(color = "black")
  )

##########################################################
### Temporal Random Effect (RW2) — Smooth Trend Over Time
##########################################################

# Extract RW2 temporal effect
time_eff <- res_interaction$summary.random$year_id %>%
  mutate(
    survey_year = sort(unique(model_data_inla$survey_year))
  )

# Plot temporal smooth

ggplot(time_eff, aes(x = survey_year, y = mean)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = `0.025quant`, ymax = `0.975quant`),
    fill = "#A6CEE3", alpha = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(
    limits = c(min(time_eff$survey_year),
               max(time_eff$survey_year) + 0.5)
  ) +
  labs(
    x = "Survey Year",
    y = "Temporal Random Effect"
  ) +
  theme_minimal(base_size = 13)

#### Plot of the survey year only
ggplot(time_eff, aes(x = factor(survey_year), y = mean, group = 1)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = `0.025quant`, ymax = `0.975quant`),
    fill = "#A6CEE3", alpha = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Survey Year",
    y = "Temporal Random Effect"
  ) +
  theme_minimal(base_size = 13)


#############################################################
#### Age-Group Random Effects — Fertility Life-Course Pattern
#############################################################

# Extract age random effects
age_eff <- res_interaction$summary.random$age_id %>%
  mutate(
    age_group = levels(model_data_inla$age_group)
  )

# Plot age pattern
ggplot(age_eff, aes(x = age_group, y = mean)) +
  geom_point(size = 3, color = "#1B9E77") +
  geom_errorbar(
    aes(ymin = `0.025quant`, ymax = `0.975quant`),
    width = 0.15, color = "#1B9E77"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Age Group",
    y = "Age-Specific Random Effect",
   # title = "Age Pattern of Fertility",
    #subtitle = "Random effects net of covariates and spatial structure"
  ) +
  theme_minimal(base_size = 13)

##############################################################
### Spatial Random Effects (ICAR) — State-Level Heterogeneity
##############################################################

# Extract spatial random effects
state_eff <- res_interaction$summary.random$state_id %>%
  mutate(REGCODE = as.integer(ID))

# Join with shapefile
nga_states_plot <- nga_states %>%
  left_join(state_eff, by = "REGCODE")

# Map posterior mean

nga_states_plot <- nga_states_plot %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )


ggplot(nga_states_plot) +
  geom_sf(aes(fill = mean), color = "gray40", linewidth = 0.4) +
  
  geom_text(
    aes(x = lon, y = lat, label = REGNAME),
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
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    axis.text = element_text(color = "black")
  )

###############################################################
############ Space -Time Interaction Effect 
################################################################

model_data_inla_full <- model_data_inla %>%
  mutate(
    survey_year = as.integer(survey_year)
  )

##### Create Interaction index

model_data_inla_full <- model_data_inla_full %>%
  mutate(
    st_id = as.integer(interaction(state_id, survey_year, drop = TRUE))
  )

##### Extract Interaction effect

st_eff <- as.data.frame(res_interaction$summary.random$st_id) %>%
  dplyr::select(ID, mean, `0.025quant`, `0.975quant`)

#### Construct state year mapping

st_index <- model_data_inla_full %>%
  distinct(state_id, survey_year, st_id)

### Merge effect with index
st_eff <- st_eff %>%
  left_join(st_index, by = c("ID" = "st_id"))

### Attcahed geomtry
nga_states_plot <- nga_states %>%
  mutate(state_id = as.integer(factor(REGCODE))) %>%
  left_join(st_eff, by = "state_id")

### Sanity Check

length(unique(st_index$st_id))
nrow(st_eff)


### Intraction plot

ggplot(nga_states_plot) +
  geom_sf(aes(fill = mean), color = NA) +   # keep Plot 1 clean look
  facet_wrap(~ survey_year, ncol = 3) +    # 3 maps on top row
  scale_fill_gradient2(
    low = "#D73027",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "Interaction Effect"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

#############################################################
##### Model Checking Diagnostics
############################################################

## 1. Conditional Predictive Ordinate (CPO) Failures

# Extract CPO values
cpo_vals <- res_interaction$cpo$cpo

# Summary of CPO
summary(cpo_vals)

# Identify potential CPO failures (very small values)
cpo_failures <- sum(cpo_vals < 1e-6, na.rm = TRUE)
cpo_failures

# Histogram of log-CPO
hist(
  -log(cpo_vals),
  breaks = 40,
  col = "#B2DF8A",
  border = "white",
   main = "Log-CPO Distribution",
  xlab = expression(-log(CPO))
)


#### 2. Probability Integral Transform (PIT) Histogram

# Extract PIT values
pit_vals <- res_interaction$cpo$pit

# PIT histogram
hist(
  pit_vals,
  breaks = 20,
  col = "#A6CEE3",
  border = "white",
  main = "PIT Histogram",
  xlab = "PIT value",
  ylab = "Frequency"
)

# Reference line for uniformity
abline(h = length(pit_vals)/20, lty = 2, col = "red")



################# Plot of TFR with ASFR Computed ################

#### Start by Re-fit the interaction model with config = TRUE
res_interaction <- inla(
  formula_interaction,
  family = "poisson",
  data   = model_data_inla,
  E      = exposure,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE   #  REQUIRED
  )
)

##### Extract posterior fitted ASFR from INLA

# Number of posterior samples
n_samp <- 1000

# Draw posterior samples of the linear predictor
lp_samp <- inla.posterior.sample(
  n = n_samp,
  result = res_interaction,
  selection = list(Predictor = 0)
)

# Convert to matrix: rows = samples, cols = observations

eta_mat <- sapply(lp_samp, function(x) {
  x$latent[grep("^Predictor", rownames(x$latent)), ]
})

# Convert to ASFR samples (rate)
lambda_mat <- exp(eta_mat)

########## Save ASFR samples( lambda_mat)

saveRDS(lambda_mat, "lambda_mat.rds")

lambda_mat <- readRDS("lambda_mat.rds")


#### Attach indices (state, year, age)

index_df <- model_data_inla %>%
  dplyr::select(state_id, survey_year, age_id)

### Compute posterior TFR by state and year

lambda_long <- as.data.frame(lambda_mat) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(
    cols = -obs,
    names_to = "draw",
    values_to = "lambda"
  ) %>%
  left_join(
    index_df %>% mutate(obs = row_number()),
    by = "obs"
  )

# Compute TFR per draw
tfr_post <- lambda_long %>%
  group_by(state_id, survey_year, draw) %>%
  summarise(
    TFR = 5 * sum(lambda),
    .groups = "drop"
  )

#### Posterior summaries of TFR

tfr_summary <- tfr_post %>%
  group_by(state_id, survey_year) %>%
  summarise(
    mean = mean(TFR),
    low  = quantile(TFR, 0.025),
    high = quantile(TFR, 0.975),
    .groups = "drop"
  )

summary(tfr_summary)


tfr_summary$survey_year <- factor(
  tfr_summary$survey_year,
  levels = c(2003, 2008, 2013, 2018, 2024)
)

### Join with Geometry

tfr_map_all <- nga_states %>%
  left_join(tfr_summary, by = c("REGCODE" = "state_id"))


### Compute a meaningful mid point for the colour divergence for the
### TFR plot which is the  mean summary across state. 
## Note: for our interaction plot, the midpoint is zero for the 
## colour divergenc

mid_tfr <- mean(tfr_summary$mean, na.rm = TRUE)


#### Faceted TFR maps across survey years 

ggplot(tfr_map_all) +
  geom_sf(aes(fill = mean), color = NA) +
  
  scale_fill_gradient2(
    low = "#D73027",
    mid = "white",
    high = "#1A9850",
    midpoint = mid_tfr,
    name = "TFR\n(posterior mean)",
    limits = range(tfr_summary$mean, na.rm = TRUE)
  ) +
  
  facet_wrap(~ survey_year, ncol = 3) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )


################################################################
#### National TFR trend 
################################################################

## Build Exposure vector
exposure_df <- model_data_inla %>%
  dplyr::select(state_id, survey_year, age_id, exposure) %>%
  mutate(obs = row_number())

## Attached exposure to posterior lambda

lambda_long <- as.data.frame(lambda_mat) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(
    cols = -obs,
    names_to = "draw",
    values_to = "lambda"
  ) %>%
  left_join(
    exposure_df,
    by = "obs"
  )

### Compute population-weighted national TRF

tfr_nat_post <- lambda_long %>%
  group_by(survey_year, age_id, draw) %>%
  summarise(
    births_hat = sum(lambda * exposure),
    exposure_total = sum(exposure),
    asfr_nat = births_hat / exposure_total,
    .groups = "drop"
  ) %>%
  group_by(survey_year, draw) %>%
  summarise(
    TFR_nat = 5 * sum(asfr_nat),
    .groups = "drop"
  )

#### Compute National TFR summary

tfr_national <- tfr_nat_post %>%
  group_by(survey_year) %>%
  summarise(
    mean = mean(TFR_nat),
    low  = quantile(TFR_nat, 0.025),
    high = quantile(TFR_nat, 0.975),
    .groups = "drop"
  )

### % decline (2003 - 2024) with credible interval

decline_post <- tfr_nat_post %>%
  filter(survey_year %in% c(2003, 2024)) %>%
  dplyr::select(survey_year, draw, TFR_nat) %>%
  pivot_wider(
    names_from = survey_year,
    values_from = TFR_nat
  ) %>%
  mutate(
    pct_decline = 100 * ( `2003` - `2024` ) / `2003`
  )

#### Summarize

decline_summary <- decline_post %>%
  summarise(
    mean_decline = mean(pct_decline),
    low_decline  = quantile(pct_decline, 0.025),
    high_decline = quantile(pct_decline, 0.975)
  )

### national Trend Plot

ggplot(tfr_national, aes(x = survey_year, y = mean)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(size = 2.5, color = "#2C7FB8") +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    fill = "#A6CEE3",
    alpha = 0.4
  ) +
  scale_x_continuous(
    breaks = c(2003, 2008, 2013, 2018, 2024)
  ) +
  labs(
    x = "Survey Year",
    y = "Population-weighted Total Fertility Rate"
  ) +
  theme_minimal(base_size = 13)



#################################################################
################### Sensitivity Analysis ###############
#################################################################

##Sensitivity analysis on the precision priors of the random effects

### Build the model with strong prior - param = c(1, 0.01)

formula_interaction_PC_strong <- births ~
  
  # ---- Fixed effects ----
prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  # ---- Spatial structured ----
f(state_id,
  model = "besag",
  graph = g,
  scale.model = TRUE,
  hyper = list(
    prec = list(prior = "pc.prec", param = c(1, 0.01))
  )
) +
  
  # ---- Temporal RW2 ----
f(year_id,
  model = "rw2",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(1, 0.01))
  )
) +
  
  # ---- Age effect ----
f(age_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(1, 0.01))
  )
) +
  
  # ---- Space–time interaction ----
f(st_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(1, 0.01))
  )
)

res_strong <- inla(
  formula_interaction_PC_strong,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE)
)


#### Model with Moderate priors (baseline) - param = c(2, 0.01)

formula_interaction_PC_moderate <- births ~
  
  # ---- Fixed effects ----
prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  # ---- Spatial structured ----
f(state_id,
  model = "besag",
  graph = g,
  scale.model = TRUE,
  hyper = list(
    prec = list(prior = "pc.prec", param = c(2, 0.01))
  )
) +
  
  # ---- Temporal RW2 ----
f(year_id,
  model = "rw2",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(2, 0.01))
  )
) +
  
  # ---- Age effect ----
f(age_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(2, 0.01))
  )
) +
  
  # ---- Space–time interaction ----
f(st_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(2, 0.01))
  )
)

res_moderate <- inla(
  formula_interaction_PC_moderate,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE)
)


##### Model with week prior - param = c(5, 0.01)

formula_interaction_PC_weak <- births ~
  
  # ---- Fixed effects ----
prop_urban +
  prop_secondary +
  prop_higher +
  prop_middle +
  prop_richer +
  prop_richest +
  prop_currently_married +
  mean_age +
  pop_density +
  nightlights +
  conflict_events +
  hf_density +
  
  # ---- Spatial structured ----
f(state_id,
  model = "besag",
  graph = g,
  scale.model = TRUE,
  hyper = list(
    prec = list(prior = "pc.prec", param = c(5, 0.01))
  )
) +
  
  # ---- Temporal RW2 ----
f(year_id,
  model = "rw2",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(5, 0.01))
  )
) +
  
  # ---- Age effect ----
f(age_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(5, 0.01))
  )
) +
  
  # ---- Space–time interaction ----
f(st_id,
  model = "iid",
  hyper = list(
    prec = list(prior = "pc.prec", param = c(5, 0.01))
  )
)

res_weak <- inla(
  formula_interaction_PC_weak,
  family = "poisson",
  data = model_data_inla,
  E = exposure,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE)
)

summary(res_strong)
summary(res_moderate)
summary(res_weak)

#### Extracting the result.

prior_results <- data.frame(
  Model = c("Baseline", "Strong prior", "Moderate prior", "Weak prior"),
  DIC   = c(res_interaction$dic$dic,
            res_strong$dic$dic,
            res_moderate$dic$dic,
            res_weak$dic$dic),
  WAIC  = c(res_interaction$waic$waic,
            res_strong$waic$waic,
            res_moderate$waic$waic,
            res_weak$waic$waic)
)

prior_results

### effective number of parameters

prior_results$DIC_pD <- c(res_interaction$dic$p.eff,
                          res_strong$dic$p.eff,
                          res_moderate$dic$p.eff,
                          res_weak$dic$p.eff)

prior_results$WAIC_pW <- c(res_interaction$waic$p.eff,
                           res_strong$waic$p.eff,
                           res_moderate$waic$p.eff,
                           res_weak$waic$p.eff)

prior_results



############################################################
# SAVE ALL ORIGINAL RESULTS FOR MANUSCRIPT REVISION
############################################################

getwd()

results_dir <- file.path(getwd(), "Model_Result")

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# A. DATA
# ------------------------------------------------------------

saveRDS(
  model_data,
  file.path(results_dir, "model_data.rds")
)

saveRDS(
  model_data_inla,
  file.path(results_dir, "model_data_inla.rds")
)

saveRDS(
  br_ir,
  file.path(results_dir, "br_ir.rds")
)

saveRDS(
  nga_states,
  file.path(results_dir, "nga_states.rds")
)

saveRDS(
  nb,
  file.path(results_dir, "nb.rds")
)

saveRDS(
  g,
  file.path(results_dir, "adjacency_graph.rds")
)


# ------------------------------------------------------------
# B. MODEL OBJECTS
# ------------------------------------------------------------

saveRDS(res0,            file.path(results_dir, "res0.rds"))
saveRDS(res1,            file.path(results_dir, "res1.rds"))
saveRDS(res2,            file.path(results_dir, "res2.rds"))
saveRDS(res3,            file.path(results_dir, "res3.rds"))
saveRDS(res_full,        file.path(results_dir, "res_full.rds"))
saveRDS(res_interaction, file.path(results_dir, "res_interaction.rds"))

saveRDS(
  res_strong,
  file.path(results_dir, "res_strong.rds")
)

saveRDS(
  res_moderate,
  file.path(results_dir, "res_moderate.rds")
)

saveRDS(
  res_weak,
  file.path(results_dir, "res_weak.rds")
)


# ------------------------------------------------------------
# C. MODEL FORMULAS
# ------------------------------------------------------------

saveRDS(formula_1, file.path(results_dir, "formula_1.rds"))
saveRDS(formula_2, file.path(results_dir, "formula_2.rds"))
saveRDS(formula_3, file.path(results_dir, "formula_3.rds"))
saveRDS(formula_full, file.path(results_dir, "formula_full.rds"))
saveRDS(
  formula_interaction,
  file.path(results_dir, "formula_interaction.rds")
)


# ------------------------------------------------------------
# D. MODEL COMPARISON
# ------------------------------------------------------------

model_comparison <- data.frame(
  Model = c(
    "Null",
    "Full",
    "Full + Interaction"
  ),
  DIC = c(
    res0$dic$dic,
    res_full$dic$dic,
    res_interaction$dic$dic
  ),
  WAIC = c(
    res0$waic$waic,
    res_full$waic$waic,
    res_interaction$waic$waic
  ),
  pD = c(
    res0$dic$p.eff,
    res_full$dic$p.eff,
    res_interaction$dic$p.eff
  ),
  pWAIC = c(
    res0$waic$p.eff,
    res_full$waic$p.eff,
    res_interaction$waic$p.eff
  )
)

saveRDS(
  model_comparison,
  file.path(results_dir, "model_comparison.rds")
)

write.csv(
  model_comparison,
  file.path(results_dir, "model_comparison.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# E. FIXED EFFECTS
# ------------------------------------------------------------

fixed_effects <- as.data.frame(res_interaction$summary.fixed) %>%
  tibble::rownames_to_column("covariate") %>%
  mutate(
    RR = exp(mean),
    RR_low = exp(`0.025quant`),
    RR_high = exp(`0.975quant`)
  )

saveRDS(
  fixed_effects,
  file.path(results_dir, "fixed_effects.rds")
)

write.csv(
  fixed_effects,
  file.path(results_dir, "fixed_effects.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# F. RANDOM EFFECTS
# ------------------------------------------------------------

saveRDS(
  res_interaction$summary.random$state_id,
  file.path(results_dir, "spatial_effect.rds")
)

saveRDS(
  res_interaction$summary.random$year_id,
  file.path(results_dir, "temporal_effect.rds")
)

saveRDS(
  res_interaction$summary.random$age_id,
  file.path(results_dir, "age_effect.rds")
)

saveRDS(
  res_interaction$summary.random$st_id,
  file.path(results_dir, "interaction_effect_raw.rds")
)


# ------------------------------------------------------------
# G. DIAGNOSTICS
# ------------------------------------------------------------

cpo_vals <- res_interaction$cpo$cpo
pit_vals <- res_interaction$cpo$pit

diagnostic_summary <- list(
  cpo = cpo_vals,
  pit = pit_vals,
  cpo_failures = sum(cpo_vals < 1e-6, na.rm = TRUE)
)

saveRDS(
  diagnostic_summary,
  file.path(results_dir, "diagnostic_summary.rds")
)

saveRDS(
  cpo_vals,
  file.path(results_dir, "cpo_values.rds")
)

saveRDS(
  pit_vals,
  file.path(results_dir, "pit_values.rds")
)


# ------------------------------------------------------------
# H. POSTERIOR FERTILITY RESULTS
# ------------------------------------------------------------

saveRDS(
  lambda_mat,
  file.path(results_dir, "lambda_mat.rds")
)

saveRDS(
  asfr_summary,
  file.path(results_dir, "asfr_summary.rds")
)

write.csv(
  asfr_summary,
  file.path(results_dir, "asfr_summary.csv"),
  row.names = FALSE
)

saveRDS(
  tfr_summary,
  file.path(results_dir, "tfr_summary.rds")
)

write.csv(
  tfr_summary,
  file.path(results_dir, "tfr_summary.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# I. PRIOR SENSITIVITY
# ------------------------------------------------------------

saveRDS(
  prior_results,
  file.path(results_dir, "prior_results.rds")
)

write.csv(
  prior_results,
  file.path(results_dir, "prior_results.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# J. MASTER OBJECT
# ------------------------------------------------------------

analysis_objects <- list(
  model_data = model_data,
  model_data_inla = model_data_inla,
  res0 = res0,
  res1 = res1,
  res2 = res2,
  res3 = res3,
  res_full = res_full,
  res_interaction = res_interaction,
  prior_results = prior_results,
  fixed_effects = fixed_effects,
  cpo_vals = cpo_vals,
  pit_vals = pit_vals,
  lambda_mat = lambda_mat,
  asfr_summary = asfr_summary,
  tfr_summary = tfr_summary
)

saveRDS(
  analysis_objects,
  file.path(results_dir, "original_analysis_objects.rds")
)

cat("\n====================================================\n")
cat("Original analysis results successfully saved.\n")
cat("Location:", results_dir, "\n")
cat("====================================================\n")


# ============================================================
# SOFTWARE INFORMATION
# ============================================================

software_info <- list(
  R_version = R.version.string,
  INLA_version = as.character(packageVersion("INLA")),
  dplyr_version = as.character(packageVersion("dplyr")),
  sf_version = as.character(packageVersion("sf")),
  ggplot2_version = as.character(packageVersion("ggplot2")),
  spdep_version = as.character(packageVersion("spdep")),
  terra_version = as.character(packageVersion("terra")),
  tidyverse_version = as.character(packageVersion("tidyverse")),
  session_info = sessionInfo()
)

saveRDS(
  software_info,
  file.path(results_dir, "software_info.rds")
)

capture.output(
  sessionInfo(),
  file = file.path(results_dir, "sessionInfo.txt")
)



# 1. Operating system
Sys.info()

# 2. R version
R.version.string

# 3. Processor information
system("wmic cpu get name", intern = TRUE)

# 4. Installed RAM
system("wmic computersystem get TotalPhysicalMemory", intern = TRUE)


