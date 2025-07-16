## REGRESSIONS

## Load packages    ------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require(lubridate, quietly = TRUE)) {
  install.packages("lubridate")
  library(lubridate)}

if (!require(fuzzyjoin, quietly = TRUE)) {
  install.packages("fuzzyjoin")
  library(fuzzyjoin)}

if (!require(easystats, quietly = TRUE)) {
  install.packages("easystats")
  library(easystats)}

if (!require(performance, quietly = TRUE)) {
  install.packages("performance")
  library(performance)}

if (!require(nortest, quietly = TRUE)) {
  install.packages("nortest")
  library(nortest)}

if (!require(forecast, quietly = TRUE)) {
  install.packages("forecast")
  library(forecast)}

if (!require(MASS, quietly = TRUE)) {
  install.packages("MASS")
  library(MASS)}

if (!require(car, quietly = TRUE)) {
  install.packages("car")
  library(car)}

## Import data -----------------------------------------------------------------
# Water quality
wq_file_path <- "./data/water_quality/PWQMN_Mill_Creek_Data.xlsx"
wq_sheets <- excel_sheets(wq_file_path) # list all sheet names

# Precipitation
precip_file_path <- "./data/precipitation/CSM_raw_precip.xlsx"
precip_sheet_names <- excel_sheets(precip_file_path) # list all sheet names
precip_list <- lapply(precip_sheet_names, function(precip_sheet_names) {
  read_excel(precip_file_path, sheet = precip_sheet_names)
}) # read sheet contents
precip_combined_data <- bind_rows(precip_list) # combine data

# Water temp
wt_file_path <- "./data/water_temp/SR10_raw_water_temp.xlsx"
wt_sheet_names <- excel_sheets(wt_file_path) # list all sheet names
wt_list <- lapply(wt_sheet_names, function(wt_sheet_names) {
  read_excel(wt_file_path, sheet = wt_sheet_names)
}) # read sheet contents
wt_combined_data <- bind_rows(wt_list) # combine data

# Air temp
at_file_path <- "./data/air_temp/CSM_raw_air_temp.xlsx"
at_sheet_names <- excel_sheets(at_file_path) # list all sheet names
at_list <- lapply(at_sheet_names, function(sheet_name) {
  read_excel(at_file_path, sheet = sheet_name)
}) # read sheet contents
at_combined_data <- bind_rows(at_list) # combine data

## Define data prep functions --------------------------------------------------
# Data prep functions take 20-30 mins to run, can skip to using prepared data

wq_precip_data_prep <- function(wq_file_path, sheet_name, precip_combined_data) {
  # load the water quality data
  parameter_data <- read_excel(wq_file_path, sheet = sheet_name) %>%
    dplyr::select(`Collection Timestamp`, Result, Units) %>%
    rename(Collection_Timestamp = `Collection Timestamp`)
  
  # Define 36 hr period before sample was taken
  parameter_data <- parameter_data %>%
    mutate(Start_36hr_Period = Collection_Timestamp - hours(36))
  
  # Define year range in the dataset
  start_year <- year(min(parameter_data$Collection_Timestamp))
  end_year <- year(max(parameter_data$Collection_Timestamp))
  
  # Extract unique months from water quality data
  unique_months <- unique(month(parameter_data$Collection_Timestamp))
  
  # Filter precip data based on year range and unique months
  relevant_precip <- precip_combined_data %>%
    filter(year(Timestamp) >= start_year & year(Timestamp) <= end_year & month(Timestamp) %in% unique_months) %>%
    rename(Precip_Value = Value)
  
  # Join the datasets
  joined_data <- parameter_data %>%
    fuzzy_left_join(relevant_precip, by = c("Start_36hr_Period" = "Timestamp", "Collection_Timestamp" = "Timestamp"), match_fun = list(`<=`, `>=`))
  
  # Calculate the total precipitation for each sample's 36-hour period
  parameter_data <- joined_data %>%
    group_by(Collection_Timestamp, Result, Units, Start_36hr_Period) %>%
    summarise(Total_Precipitation = sum(Precip_Value, na.rm = TRUE), .groups = 'drop')
  
  return(parameter_data)
}

wq_temp_data_prep <- function(wt_file_path, sheet_name, wt_combined_data) {
  # load the water quality data
  parameter_data <- read_excel(wq_file_path, sheet = sheet_name) %>%
    dplyr::select(`Collection Timestamp`, Result, Units) %>%
    rename(Collection_Timestamp = `Collection Timestamp`)
  
  # Define 36 hr period before sample was taken
  parameter_data <- parameter_data %>%
    mutate(Start_36hr_Period = Collection_Timestamp - hours(36))
  
  # Define year range in the dataset
  start_year <- year(min(parameter_data$Collection_Timestamp))
  end_year <- year(max(parameter_data$Collection_Timestamp))
  
  # Extract unique months from water quality data
  unique_months <- unique(month(parameter_data$Collection_Timestamp))
  
  # Filter water temp data based on year range and unique months
  relevant_temp <- wt_combined_data %>%
    filter(year(Timestamp) >= start_year & year(Timestamp) <= end_year & month(Timestamp) %in% unique_months) %>%
    rename(WT_Value = Value)
  
  # Join the datasets
  joined_data <- parameter_data %>%
    fuzzy_left_join(relevant_temp, by = c("Start_36hr_Period" = "Timestamp", "Collection_Timestamp" = "Timestamp"), match_fun = list(`<=`, `>=`))
  
  # Calculate the mean temperature for each sample's 36-hour period
  parameter_data <- joined_data %>%
    group_by(Collection_Timestamp, Result, Units, Start_36hr_Period) %>%
    summarise(Mean_Water_Temp = mean(WT_Value, na.rm = TRUE), .groups = 'drop')
  
  return(parameter_data)
}

## Ammonium data prep ----------------------------------------------------------
# Integrate precip data
ammonium_precip <- wq_precip_data_prep(wq_file_path, "Ammonium", precip_combined_data)

# Select specific columns of precip data
ammonium_precip <- ammonium_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
ammonium_temp <- wq_temp_data_prep(wq_file_path, "Ammonium", wt_combined_data)

# Remove blank value rows
ammonium_temp <- filter(ammonium_temp, Mean_Water_Temp!="NaN")

# Join temp and precip
ammonium <- left_join(ammonium_temp, ammonium_precip, by = "Collection_Timestamp")

# Ln transformation of ammonium
ammonium$log_transformed <- log(ammonium$Result)
ammonium$log_transformed[ammonium$log_transformed=="-Inf"]<-0

# Log transformation of ammonium
ammonium$log_transformed <- log10(ammonium$Result)
ammonium$log_transformed[ammonium$log_transformed=="-Inf"]<-0

# Square root transformation of ammonium
ammonium$sqrt_transformed <- sqrt(ammonium$Result)

# Save prepared data
write.csv(ammonium, "./data/water_quality/ammonium.csv")

## Chloride data prep ----------------------------------------------------------
# Integrate precip data
chloride_precip <- wq_precip_data_prep(wq_file_path, "Chloride", precip_combined_data)

# Select specific columns of precip data
chloride_precip <- chloride_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
chloride_temp <- wq_temp_data_prep(wq_file_path, "Chloride", wt_combined_data)

# Remove blank value rows
chloride_temp <- filter(chloride_temp, Mean_Water_Temp!="NaN")

# Join temp and precip
chloride <- left_join(chloride_temp, chloride_precip, by = "Collection_Timestamp")

# Ln transformation of chloride
chloride$ln_transformed <- log(chloride$Result)
chloride$ln_transformed[chloride$ln_transformed=="-Inf"]<-0

# Log transformation of chloride
chloride$log_transformed <- log10(chloride$Result)
chloride$log_transformed[chloride$log_transformed=="-Inf"]<-0

# Square root transformation of chloride
chloride$sqrt_transformed <- sqrt(chloride$Result)

# Save prepared data
write.csv(chloride, "./data/water_quality/chloride.csv")


## Conductivity data prep ------------------------------------------------------
# Integrate precip data
conductivity_precip <- wq_precip_data_prep(wq_file_path, "Conductivity", precip_combined_data)

# Select specific columns of precip data
conductivity_precip <- conductivity_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
conductivity_temp <- wq_temp_data_prep(wq_file_path, "Conductivity", wt_combined_data)

# Remove blank value rows
conductivity_temp <- filter(conductivity_temp, Mean_Water_Temp!="NaN")

# Delete duplicated value
conductivity_temp <- conductivity_temp[-53,]

# Join temp and precip
conductivity <- left_join(conductivity_temp, conductivity_precip, by = "Collection_Timestamp")

# Ln transformation of conductivity
conductivity$ln_transformed <- log(conductivity$Result)
conductivity$ln_transformed[conductivity$log_transformed=="-Inf"]<-0

# Log transformation of conductivity
conductivity$log_transformed <- log10(conductivity$Result)
conductivity$log_transformed[conductivity$log_transformed=="-Inf"]<-0

# Square root transformation of conductivity
conductivity$sqrt_transformed <- sqrt(conductivity$Result)

# Save prepared data
write.csv(conductivity, "./data/water_quality/conductivity.csv")


## Dissolved oxygen data prep --------------------------------------------------
# Integrate precip data
do_precip <- wq_precip_data_prep(wq_file_path, "Dissolved Oxygen", precip_combined_data)

# Select specific columns of precip data
do_precip <- do_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
do_temp <- wq_temp_data_prep(wq_file_path, "Dissolved Oxygen", wt_combined_data)

# Remove blank value rows
do_temp <- filter(do_temp, Mean_Water_Temp!="NaN")

# Join temp and precip
do <- left_join(do_temp, do_precip, by = "Collection_Timestamp")

# Ln transformation of do
do$ln_transformed <- log(do$Result)
do$ln_transformed[do$ln_transformed=="-Inf"]<-0

# Log transformation of do
do$log_transformed <- log10(do$Result)
do$log_transformed[do$log_transformed=="-Inf"]<-0

# Square root transformation of do
do$sqrt_transformed <- sqrt(do$Result)

# Save prepared data
write.csv(do, "./data/water_quality/do.csv")

## Nitrate data prep -----------------------------------------------------------
# Integrate precip data
nitrate_precip <- wq_precip_data_prep(wq_file_path, "Nitrate", precip_combined_data)

# Select specific columns of precip data
nitrate_precip <- nitrate_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
nitrate_temp <- wq_temp_data_prep(wq_file_path, "Nitrate", wt_combined_data)

# Remove blank value rows
nitrate_temp <- filter(nitrate_temp, Mean_Water_Temp!="NaN")

# Join temp and precip
nitrate <- left_join(nitrate_temp, nitrate_precip, by = "Collection_Timestamp")

# Ln transformation of nitrate
nitrate$ln_transformed <- log(nitrate$Result)
nitrate$ln_transformed[nitrate$ln_transformed=="-Inf"]<-0

# Log transformation of nitrate
nitrate$log_transformed <- log10(nitrate$Result)
nitrate$log_transformed[nitrate$log_transformed=="-Inf"]<-0

# Square root transformation of nitrate
nitrate$sqrt_transformed <- sqrt(nitrate$Result)

# Save prepared data
write.csv(nitrate, "./data/water_quality/nitrate.csv")

## Nitrite data prep -----------------------------------------------------------
# Integrate precip data
nitrite_precip <- wq_precip_data_prep(wq_file_path, "Nitrite", precip_combined_data)

# Select specific columns of precip data
nitrite_precip <- nitrite_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
nitrite_temp <- wq_temp_data_prep(wq_file_path, "Nitrite", wt_combined_data)

# Remove blank value rows
nitrite_temp <- filter(nitrite_temp, Mean_Water_Temp!="NaN")

# Join temp and precip
nitrite <- left_join(nitrite_temp, nitrite_precip, by = "Collection_Timestamp")

# Ln transformation of nitrite
nitrite$ln_transformed <- log(nitrite$Result)
nitrite$ln_transformed[nitrite$ln_transformed=="-Inf"]<-0

# Log transformation of nitrite
nitrite$log_transformed <- log10(nitrite$Result)
nitrite$log_transformed[nitrite$log_transformed=="-Inf"]<-0

# Square root transformation of nitrite
nitrite$sqrt_transformed <- sqrt(nitrite$Result)

# Save prepared data
write.csv(nitrite, "./data/water_quality/nitrite.csv")

## pH data prep ----------------------------------------------------------------
# Integrate precip data
ph_precip <- wq_precip_data_prep(wq_file_path, "pH", precip_combined_data)

# Select specific columns of precip data
ph_precip <- ph_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
ph_temp <- wq_temp_data_prep(wq_file_path, "pH", wt_combined_data)

# Remove blank value rows
ph_temp <- filter(ph_temp, Mean_Water_Temp!="NaN")

# Delete duplicated value
ph_temp <- ph_temp[-53,]

# Join temp and precip
ph <- left_join(ph_temp, ph_precip, by = "Collection_Timestamp")

# Ln transformation of pH
ph$ln_transformed <- log(ph$Result)
ph$ln_transformed[ph$ln_transformed=="-Inf"]<-0

# Log transformation of pH
ph$log_transformed <- log10(ph$Result)
ph$log_transformed[ph$log_transformed=="-Inf"]<-0

#Square root transformation of pH
ph$sqrt_transformed <- sqrt(ph$Result)

# Save prepared data
write.csv(ph, "./data/water_quality/ph.csv")

## Phosphate data prep ---------------------------------------------------------
# Integrate precip data
phosphate_precip <- wq_precip_data_prep(wq_file_path, "Phosphate", precip_combined_data)

# Select specific columns of precip data
phosphate_precip <- phosphate_precip %>%
  select(Collection_Timestamp, Total_Precipitation)

# Integrate water temp data
phosphate_temp <- wq_temp_data_prep(wq_file_path, "Phosphate", wt_combined_data)

# Remove blank value rows
phosphate_temp <- filter(phosphate_temp, Mean_Water_Temp!="NaN")

# Delete negative value
phosphate_temp <- phosphate_temp[-10,]

# Join temp and precip
phosphate <- left_join(phosphate_temp, phosphate_precip, by = "Collection_Timestamp")

# Ln transformation of phosphate
phosphate$ln_transformed <- log(phosphate$Result)
phosphate$ln_transformed[phosphate$ln_transformed=="-Inf"]<-0

# Log transformation of phosphate
phosphate$log_transformed <- log10(phosphate$Result)
phosphate$log_transformed[phosphate$log_transformed=="-Inf"]<-0

# Square root transformation of phosphate
phosphate$sqrt_transformed <- sqrt(phosphate$Result)

# Save prepared data
write.csv(phosphate, "./data/water_quality/phosphate.csv")

## Temperature data prep -------------------------------------------------------
wt_combined_data$date <- as.Date(ymd_hms(wt_combined_data$Timestamp, truncated = 6)) # add date column

water_daily_avg <- wt_combined_data %>%
  group_by(date) %>%
  summarise(daily_avg_water_temp = mean(Value, na.rm = TRUE)) # get daily avg water temp

at_combined_data$date <- as.Date(ymd_hms(at_combined_data$Timestamp, truncated = 6)) # add date column

air_daily_avg <- at_combined_data %>%
  group_by(date) %>%
  summarise(daily_avg_air_temp = mean(Value, na.rm = TRUE)) # get daily avg air temp

# Combine water and air temp
combined_daily_avg <- left_join(water_daily_avg, air_daily_avg, by = "date")

# Delete rows with NA values
combined_daily_avg <- drop_na(combined_daily_avg)

# Ln transformations
combined_daily_avg$ln_transformed_air <- log(combined_daily_avg$daily_avg_air_temp)
combined_daily_avg$ln_transformed_air[combined_daily_avg$ln_transformed_air=="-Inf"]<-0

combined_daily_avg$ln_transformed_water <- log(combined_daily_avg$daily_avg_water_temp)
combined_daily_avg$ln_transformed_water[combined_daily_avg$ln_transformed_water=="-Inf"]<-0

# Log transformations
combined_daily_avg$log_transformed_air <- log10(combined_daily_avg$daily_avg_air_temp)
combined_daily_avg$log_transformed_air[combined_daily_avg$log_transformed_air=="-Inf"]<-0

combined_daily_avg$log_transformed_water <- log10(combined_daily_avg$daily_avg_water_temp)
combined_daily_avg$log_transformed_water[combined_daily_avg$log_transformed_water=="-Inf"]<-0

# Square root transformations
combined_daily_avg$sqrt_transformed_air <- sqrt(combined_daily_avg$daily_avg_air_temp)
combined_daily_avg$sqrt_transformed_air[combined_daily_avg$sqrt_transformed_air=="-Inf"]<-0

combined_daily_avg$sqrt_transformed_water <- sqrt(combined_daily_avg$daily_avg_water_temp)
combined_daily_avg$sqrt_transformed_water[combined_daily_avg$sqrt_transformed_water=="-Inf"]<-0

# Clean data
combined_daily_avg <- na.omit(combined_daily_avg)

View(combined_daily_avg)

## Ammonium regression ---------------------------------------------------------
ammonium <- read.csv("./data/water_quality/ammonium.csv")

# Visualize data
ammonium %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Ammonium concentration (mg/L)") +
  theme_minimal()

ammonium %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Ammonium concentration (mg/L)") +
  theme_minimal()

# Summary statistics
summary(ammonium$Result)
summary(ammonium$Total_Precipitation)
summary(ammonium$Mean_Water_Temp)

# Histograms
ammonium %>% # raw - right skewed
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 0.005) +
  labs(x = "Ammonium concentration (mg/L)", y = "Frequency")

ammonium %>% # ln transformed - slight right skew, almost normal
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed ammonium concentration (ln[mg/L])", y = "Frequency")

ammonium %>% # log transformed - right skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed ammonium concentration (log[mg/L])", y = "Frequency")

ammonium %>% # sqrt transformed - normal
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Sqrt transformed ammonium concentration (sqrt[mg/L])", y = "Frequency")

# Check normality
shapiro_ammonium <- shapiro.test(ammonium$Result) %>% print() # non-normal, p = 5.035e-09
shapiro_ammonium_ln <- shapiro.test(ammonium$ln_transformed) %>% print() # non-normal, p = 6.557e-06
shapiro_ammonium_log <- shapiro.test(ammonium$log_transformed) %>% print() # non-normal, p = 6.557e-06
shapiro_ammonium_sqrt <- shapiro.test(ammonium$sqrt_transformed) %>% print() # borderline normal, p = 0.02306

# Regressions - raw ammonium
ammonium_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = ammonium) # linear model with both predictors and "*" interaction
summary(ammonium_model_synergy)
check_normality(ammonium_model_synergy) # non-normality of residuals detected (p < .001)

ammonium_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = ammonium) # linear model with both predictors and "+" interaction
summary(ammonium_model_additive)
check_normality(ammonium_model_additive) # non-normality of residuals detected (p < .001)

ammonium_model_precip <- lm(Result ~ Total_Precipitation, data = ammonium) # linear model with only precip
summary(ammonium_model_precip)
check_normality(ammonium_model_precip) # non-normality of residuals detected (p < .001)

ammonium_model_temp <- lm(Result ~ Mean_Water_Temp, data = ammonium) # linear model with only water temp
summary(ammonium_model_temp)
check_normality(ammonium_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ammonium_model_synergy, ammonium_model_additive, ammonium_model_precip, ammonium_model_temp) # temp model has lowest AIC

# Regressions - ln transformed ammonium
ln_ammonium_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ammonium) # linear model with both predictors and "*" interaction
summary(ln_ammonium_model_synergy)
check_normality(ln_ammonium_model_synergy) # non-normality of residuals detected (p < .001)

ln_ammonium_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ammonium) # linear model with both predictors and "+" interaction
summary(ln_ammonium_model_additive)
check_normality(ln_ammonium_model_additive) # non-normality of residuals detected (p = 0.005)

ln_ammonium_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = ammonium) # linear model with only precip
summary(ln_ammonium_model_precip)
check_normality(ln_ammonium_model_precip) # non-normality of residuals detected (p = 0.004)

ln_ammonium_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = ammonium) # linear model with only water temp
summary(ln_ammonium_model_temp)
check_normality(ln_ammonium_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_ammonium_model_synergy, ln_ammonium_model_additive, ln_ammonium_model_precip, ln_ammonium_model_temp) # synergistic model has lowest AIC

# Regressions - log transformed ammonium
log_ammonium_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ammonium) # linear model with both predictors and "*" interaction
summary(log_ammonium_model_synergy)
check_normality(log_ammonium_model_synergy) # non-normality of residuals detected (p < .001)

log_ammonium_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ammonium) # linear model with both predictors and "+" interaction
summary(log_ammonium_model_additive)
check_normality(log_ammonium_model_additive) # non-normality of residuals detected (p = 0.005)

log_ammonium_model_precip <- lm(log_transformed ~ Total_Precipitation, data = ammonium) # linear model with only precip
summary(log_ammonium_model_precip)
check_normality(log_ammonium_model_precip) # non-normality of residuals detected (p = 0.004)

log_ammonium_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = ammonium) # linear model with only water temp
summary(log_ammonium_model_temp)
check_normality(log_ammonium_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_ammonium_model_synergy, log_ammonium_model_additive, log_ammonium_model_precip, log_ammonium_model_temp) # synergistic model has lowest AIC

# Regressions - sqrt transformed ammonium
sqrt_ammonium_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ammonium) # linear model with both predictors and "*" interaction
summary(sqrt_ammonium_model_synergy)
check_normality(sqrt_ammonium_model_synergy) # residuals appear as normally distributed (p = 0.167)

sqrt_ammonium_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ammonium) # linear model with both predictors and "+" interaction
summary(sqrt_ammonium_model_additive)
check_normality(sqrt_ammonium_model_additive) # residuals appear as normally distributed (p = 0.342)

sqrt_ammonium_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = ammonium) # linear model with only precip
summary(sqrt_ammonium_model_precip)
check_normality(sqrt_ammonium_model_precip) # non-normality of residuals detected (p = 0.017)

sqrt_ammonium_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = ammonium) # linear model with only water temp
summary(sqrt_ammonium_model_temp)
check_normality(sqrt_ammonium_model_temp) # residuals appear as normally distributed (p = 0.420)

compare_performance(sqrt_ammonium_model_synergy, sqrt_ammonium_model_additive, sqrt_ammonium_model_precip, sqrt_ammonium_model_temp) # temp model has lowest AIC, where temp is a borderline significant predictor

par(mfrow = c(2, 2))
plot(sqrt_ammonium_model_temp)

# Plot ammonium
ggplot(ammonium, aes(x = Mean_Water_Temp, y = sqrt_transformed)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Mean Water Temperature (C)", y = "Square root transformed ammonium concentration (sqrt[mg/L])") +
  theme_minimal()

## Chloride regression ---------------------------------------------------------
chloride <- read.csv("./data/water_quality/chloride.csv")

# Visualize data
chloride %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Chloride concentration (mg/L)") +
  theme_minimal()

chloride %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Chloride concentration (mg/L)") +
  theme_minimal()

# Summary statistics
summary(chloride$Result)
summary(chloride$Total_Precipitation)
summary(chloride$Mean_Water_Temp)

# Histograms
chloride %>% # raw - multimodal
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Chloride concentration (mg/L)", y = "Frequency")

chloride %>% # ln transformed - normal
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Ln transformed chloride concentration (ln[mg/L])", y = "Frequency")

chloride %>% # log transformed - normal
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Log transformed chloride concentration (log[mg/L])", y = "Frequency")

chloride %>% # sqrt transformed - normal
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Sqrt transformed chloride concentration (sqrt[mg/L])", y = "Frequency")

# Check normality
shapiro_chloride <- shapiro.test(chloride$Result) %>% print() # borderline normal, p = 0.01516
shapiro_chloride_ln <- shapiro.test(chloride$ln_transformed) %>% print() # normal, p = 0.07253
shapiro_chloride_log <- shapiro.test(chloride$log_transformed) %>% print() # normal, p = 0.07253
shapiro_chloride_sqrt <- shapiro.test(chloride$sqrt_transformed) %>% print() # borderline normal, p = 0.04934

# Regressions - raw chloride
chloride_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = chloride) # linear model with both predictors and "*" interaction
summary(chloride_model_synergy)
check_normality(chloride_model_synergy) # non-normality of residuals detected (p = 0.002)

chloride_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = chloride) # linear model with both predictors and "+" interaction
summary(chloride_model_additive)
check_normality(chloride_model_additive) # non-normality of residuals detected (p = 0.003)

chloride_model_precip <- lm(Result ~ Total_Precipitation, data = chloride) # linear model with only precip
summary(chloride_model_precip)
check_normality(chloride_model_precip) # non-normality of residuals detected (p = 0.005)

chloride_model_temp <- lm(Result ~ Mean_Water_Temp, data = chloride) # linear model with only water temp
summary(chloride_model_temp)
check_normality(chloride_model_temp) # non-normality of residuals detected (p = 0.007)

compare_performance(chloride_model_synergy, chloride_model_additive, chloride_model_precip, chloride_model_temp) # temp model has lowest AIC

# Regressions - ln transformed chloride
ln_chloride_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = chloride) # linear model with both predictors and "*" interaction
summary(ln_chloride_model_synergy)
check_normality(ln_chloride_model_synergy) # residuals appear as normally distributed (p = 0.155)

ln_chloride_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = chloride) # linear model with both predictors and "+" interaction
summary(ln_chloride_model_additive)
check_normality(ln_chloride_model_additive) # residuals appear as normally distributed (p = 0.201)

ln_chloride_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = chloride) # linear model with only precip
summary(ln_chloride_model_precip)
check_normality(ln_chloride_model_precip) # residuals appear as normally distributed (p = 0.085)

ln_chloride_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = chloride) # linear model with only water temp
summary(ln_chloride_model_temp)
check_normality(ln_chloride_model_temp) # residuals appear as normally distributed (p = 0.236)

compare_performance(ln_chloride_model_synergy, ln_chloride_model_additive, ln_chloride_model_precip, ln_chloride_model_temp) # precip model has lowest AIC

# Regressions - log transformed chloride
log_chloride_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = chloride) # linear model with both predictors and "*" interaction
summary(log_chloride_model_synergy)
check_normality(log_chloride_model_synergy) # residuals appear as normally distributed (p = 0.155)

log_chloride_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = chloride) # linear model with both predictors and "+" interaction
summary(log_chloride_model_additive)
check_normality(log_chloride_model_additive) # residuals appear as normally distributed (p = 0.201)

log_chloride_model_precip <- lm(log_transformed ~ Total_Precipitation, data = chloride) # linear model with only precip
summary(log_chloride_model_precip)
check_normality(log_chloride_model_precip) # residuals appear as normally distributed (p = 0.085)

log_chloride_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = chloride) # linear model with only water temp
summary(log_chloride_model_temp)
check_normality(log_chloride_model_temp) # residuals appear as normally distributed (p = 0.236)

compare_performance(log_chloride_model_synergy, log_chloride_model_additive, log_chloride_model_precip, log_chloride_model_temp) # precip model has lowest AIC

# Regressions - sqrt transformed chloride
sqrt_chloride_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = chloride) # linear model with both predictors and "*" interaction
summary(sqrt_chloride_model_synergy)
check_normality(sqrt_chloride_model_synergy) # non-normality of residuals detected (p = 0.022)

sqrt_chloride_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = chloride) # linear model with both predictors and "+" interaction
summary(sqrt_chloride_model_additive)
check_normality(sqrt_chloride_model_additive) # non-normality of residuals detected (p = 0.034)

sqrt_chloride_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = chloride) # linear model with only precip
summary(sqrt_chloride_model_precip)
check_normality(sqrt_chloride_model_precip) # non-normality of residuals detected (p = 0.031)

sqrt_chloride_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = chloride) # linear model with only water temp
summary(sqrt_chloride_model_temp)
check_normality(sqrt_chloride_model_temp) # residuals appear as normally distributed (p = 0.057)

compare_performance(sqrt_chloride_model_synergy, sqrt_chloride_model_additive, sqrt_chloride_model_precip, sqrt_chloride_model_temp) # temp model has lowest AIC

# Compare all models with normal residuals
compare_performance(ln_chloride_model_synergy, ln_chloride_model_additive, ln_chloride_model_precip, ln_chloride_model_temp, log_chloride_model_synergy, log_chloride_model_additive, log_chloride_model_precip, log_chloride_model_temp, sqrt_chloride_model_temp) # log_chloride_model_temp has the best fit

# Plot chloride
ggplot(chloride, aes(x = Mean_Water_Temp, y = log_transformed)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Mean Water Temperature (C)", y = "Log transformed chloride concentration (log[mg/L])") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(log_chloride_model_temp)

## Nitrate regression ----------------------------------------------------------
nitrate <- read.csv("./data/water_quality/nitrate.csv")

# Visualize data
nitrate %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Nitrate concentration (mg/L)") +
  theme_minimal()

nitrate %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Nitrate concentration (mg/L)") +
  theme_minimal()

# Summary statistics
summary(nitrate$Result)
summary(nitrate$Total_Precipitation)
summary(nitrate$Mean_Water_Temp)

# Histograms
nitrate %>% # raw - right skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Nitrate concentration (mg/L)", y = "Frequency")

nitrate %>% # ln transformed - right skew
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed nitrate concentration (ln[mg/L])", y = "Frequency")

nitrate %>% # log transformed - right skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Log transformed nitrate concentration (log[mg/L])", y = "Frequency")

nitrate %>% # sqrt transformed - right skew
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Sqrt transformed nitrate concentration (sqrt[mg/L])", y = "Frequency")

# Check normality
shapiro_nitrate <- shapiro.test(nitrate$Result) %>% print() # non-normal, p = 1e-15
shapiro_nitrate_ln <- shapiro.test(nitrate$ln_transformed) %>% print() # non-normal, p = 4.227e-05
shapiro_nitrate_log <- shapiro.test(nitrate$log_transformed) %>% print() # non-normal, p = 4.227e-05
shapiro_nitrate_sqrt <- shapiro.test(nitrate$sqrt_transformed) %>% print() # non-normal, p = 1.334e-10

# Regressions - raw nitrate
nitrate_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # linear model with both predictors and "*" interaction
summary(nitrate_model_synergy)
check_normality(nitrate_model_synergy) # non-normality of residuals detected (p < .001)

nitrate_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # linear model with both predictors and "+" interaction
summary(nitrate_model_additive)
check_normality(nitrate_model_additive) # non-normality of residuals detected (p < .001)

nitrate_model_precip <- lm(Result ~ Total_Precipitation, data = nitrate) # linear model with only precip
summary(nitrate_model_precip)
check_normality(nitrate_model_precip) # non-normality of residuals detected (p < .001)

nitrate_model_temp <- lm(Result ~ Mean_Water_Temp, data = nitrate) # linear model with only water temp
summary(nitrate_model_temp)
check_normality(nitrate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(nitrate_model_synergy, nitrate_model_additive, nitrate_model_precip, nitrate_model_temp) # temp model has lowest AIC

# Regressions - ln transformed nitrate
ln_nitrate_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # linear model with both predictors and "*" interaction
summary(ln_nitrate_model_synergy)
check_normality(ln_nitrate_model_synergy) # non-normality of residuals detected (p < .001)

ln_nitrate_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # linear model with both predictors and "+" interaction
summary(ln_nitrate_model_additive)
check_normality(ln_nitrate_model_additive) # non-normality of residuals detected (p < .001)

ln_nitrate_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = nitrate) # linear model with only precip
summary(ln_nitrate_model_precip)
check_normality(ln_nitrate_model_precip) # non-normality of residuals detected (p < .001)

ln_nitrate_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = nitrate) # linear model with only water temp
summary(ln_nitrate_model_temp)
check_normality(ln_nitrate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_nitrate_model_synergy, ln_nitrate_model_additive, ln_nitrate_model_precip, ln_nitrate_model_temp) # additive model has lowest AIC

# Regressions - log transformed nitrate
log_nitrate_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # linear model with both predictors and "*" interaction
summary(log_nitrate_model_synergy)
check_normality(log_nitrate_model_synergy) # non-normality of residuals detected (p < .001)

log_nitrate_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # linear model with both predictors and "+" interaction
summary(log_nitrate_model_additive)
check_normality(log_nitrate_model_additive) # non-normality of residuals detected (p < .001)

log_nitrate_model_precip <- lm(log_transformed ~ Total_Precipitation, data = nitrate) # linear model with only precip
summary(log_nitrate_model_precip)
check_normality(log_nitrate_model_precip) # non-normality of residuals detected (p < .001)

log_nitrate_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = nitrate) # linear model with only water temp
summary(log_nitrate_model_temp)
check_normality(log_nitrate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_nitrate_model_synergy, log_nitrate_model_additive, log_nitrate_model_precip, log_nitrate_model_temp) # additive model has lowest AIC

# Regressions - sqrt transformed nitrate
sqrt_nitrate_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # linear model with both predictors and "*" interaction
summary(sqrt_nitrate_model_synergy)
check_normality(sqrt_nitrate_model_synergy) # non-normality of residuals detected (p < .001)

sqrt_nitrate_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # linear model with both predictors and "+" interaction
summary(sqrt_nitrate_model_additive)
check_normality(sqrt_nitrate_model_additive) # non-normality of residuals detected (p < .001)

sqrt_nitrate_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = nitrate) # linear model with only precip
summary(sqrt_nitrate_model_precip)
check_normality(sqrt_nitrate_model_precip) # non-normality of residuals detected (p < .001)

sqrt_nitrate_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = nitrate) # linear model with only water temp
summary(sqrt_nitrate_model_temp)
check_normality(sqrt_nitrate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(sqrt_nitrate_model_synergy, sqrt_nitrate_model_additive, sqrt_nitrate_model_precip, sqrt_nitrate_model_temp) # additive model has lowest AIC

# Box cox transformation
lambda_nitrate <- BoxCox.lambda(nitrate$Result) # find the optimal lambda
transformed_nitrate <- BoxCox(nitrate$Result, lambda_nitrate) # transform raw values using lambda
nitrate$box_cox_transformed = transformed_nitrate # make new column in dataset
View(nitrate)

shapiro.test(nitrate$box_cox_transformed) # non-normal, p = 0.0001404 

# Regressions - box cox transformed nitrate
box_cox_nitrate_model_synergy <- lm(box_cox_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # linear model with both predictors and "*" interaction
summary(box_cox_nitrate_model_synergy)
check_normality(box_cox_nitrate_model_synergy) # non-normality of residuals detected (p < .001)

box_cox_nitrate_model_additive <- lm(box_cox_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # linear model with both predictors and "+" interaction
summary(box_cox_nitrate_model_additive)
check_normality(box_cox_nitrate_model_additive) # non-normality of residuals detected (p < .001)

box_cox_nitrate_model_precip <- lm(box_cox_transformed ~ Total_Precipitation, data = nitrate) # linear model with only precip
summary(box_cox_nitrate_model_precip)
check_normality(box_cox_nitrate_model_precip) # non-normality of residuals detected (p < .001)

box_cox_nitrate_model_temp <- lm(box_cox_transformed ~ Mean_Water_Temp, data = nitrate) # linear model with only water temp
summary(box_cox_nitrate_model_temp)
check_normality(box_cox_nitrate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(box_cox_nitrate_model_synergy, box_cox_nitrate_model_additive, box_cox_nitrate_model_precip, box_cox_nitrate_model_temp) # synergistic model has lowest AIC

# Robust regression
nitrate_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = nitrate) # rlm model with both predictors and "*" interaction
summary(nitrate_rlm_synergy)

nitrate_rlm_additive <- rlm(Result ~ Total_Precipitation + Mean_Water_Temp, data = nitrate) # rlm model with both predictors and "+" interaction
summary(nitrate_rlm_additive)

nitrate_rlm_precip <- rlm(Result ~ Total_Precipitation, data = nitrate) # rlm model with only precip
summary(nitrate_rlm_precip)

nitrate_rlm_temp <- rlm(Result ~ Mean_Water_Temp, data = nitrate) # rlm model with only temp
summary(nitrate_rlm_temp)

compare_performance(nitrate_rlm_synergy, nitrate_rlm_additive, nitrate_rlm_precip, nitrate_rlm_temp) # temp model has lowest AIC

# Plot nitrate
ggplot(nitrate, aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Mean Water Temperature (C)", y = "Nitrate concentration (mg/L)") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(nitrate_rlm_temp)

## Nitrite regression ----------------------------------------------------------
nitrite <- read.csv("./data/water_quality/nitrite.csv")

# Visualize data
nitrite %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Nitrite concentration (mg/L)") +
  theme_minimal()

nitrite %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Nitrite concentration (mg/L)") +
  theme_minimal()

# Summary statistics
summary(nitrite$Result)
summary(nitrite$Total_Precipitation)
summary(nitrite$Mean_Water_Temp)

# Histograms
nitrite %>% # raw - right skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 0.001) +
  labs(x = "Nitrite concentration (mg/L)", y = "Frequency")

nitrite %>% # ln transformed - right skew
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed nitrite concentration (ln[mg/L])", y = "Frequency")

nitrite %>% # log transformed - right skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed nitrite concentration (log[mg/L])", y = "Frequency")

nitrite %>% # sqrt transformed - normal
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Sqrt transformed nitrite concentration (sqrt[mg/L])", y = "Frequency")

# Check normality
shapiro_nitrite <- shapiro.test(nitrite$Result) %>% print() # non-normal, p = 1.98e-06
shapiro_nitrite_ln <- shapiro.test(nitrite$ln_transformed) %>% print() # non-normal, p = 4.638e-13
shapiro_nitrite_log <- shapiro.test(nitrite$log_transformed) %>% print() # non-normal, p = 4.638e-13
shapiro_nitrite_sqrt <- shapiro.test(nitrite$sqrt_transformed) %>% print() # non-normal, p = 3.252e-06

# Regressions - raw nitrite
nitrite_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # linear model with both predictors and "*" interaction
summary(nitrite_model_synergy)
check_normality(nitrite_model_synergy) # non-normality of residuals detected (p < .001)

nitrite_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # linear model with both predictors and "+" interaction
summary(nitrite_model_additive)
check_normality(nitrite_model_additive) # non-normality of residuals detected (p < .001)

nitrite_model_precip <- lm(Result ~ Total_Precipitation, data = nitrite) # linear model with only precip
summary(nitrite_model_precip)
check_normality(nitrite_model_precip) # non-normality of residuals detected (p < .001)

nitrite_model_temp <- lm(Result ~ Mean_Water_Temp, data = nitrite) # linear model with only water temp
summary(nitrite_model_temp)
check_normality(nitrite_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(nitrite_model_synergy, nitrite_model_additive, nitrite_model_precip, nitrite_model_temp) # precip model has lowest AIC

# Regressions - ln transformed nitrite
ln_nitrite_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # linear model with both predictors and "*" interaction
summary(ln_nitrite_model_synergy)
check_normality(ln_nitrite_model_synergy) # non-normality of residuals detected (p < .001)

ln_nitrite_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # linear model with both predictors and "+" interaction
summary(ln_nitrite_model_additive)
check_normality(ln_nitrite_model_additive) # non-normality of residuals detected (p < .001)

ln_nitrite_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = nitrite) # linear model with only precip
summary(ln_nitrite_model_precip)
check_normality(ln_nitrite_model_precip) # non-normality of residuals detected (p < .001)

ln_nitrite_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = nitrite) # linear model with only water temp
summary(ln_nitrite_model_temp)
check_normality(ln_nitrite_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_nitrite_model_synergy, ln_nitrite_model_additive, ln_nitrite_model_precip, ln_nitrite_model_temp) # precip model has lowest AIC

# Regressions - log transformed nitrite
log_nitrite_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # linear model with both predictors and "*" interaction
summary(log_nitrite_model_synergy)
check_normality(log_nitrite_model_synergy) # non-normality of residuals detected (p < .001)

log_nitrite_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # linear model with both predictors and "+" interaction
summary(log_nitrite_model_additive)
check_normality(log_nitrite_model_additive) # non-normality of residuals detected (p < .001)

log_nitrite_model_precip <- lm(log_transformed ~ Total_Precipitation, data = nitrite) # linear model with only precip
summary(log_nitrite_model_precip)
check_normality(log_nitrite_model_precip) # non-normality of residuals detected (p < .001)

log_nitrite_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = nitrite) # linear model with only water temp
summary(log_nitrite_model_temp)
check_normality(log_nitrite_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_nitrite_model_synergy, log_nitrite_model_additive, log_nitrite_model_precip, log_nitrite_model_temp) # precip model has lowest AIC

# Regressions - sqrt transformed nitrite
sqrt_nitrite_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # linear model with both predictors and "*" interaction
summary(sqrt_nitrite_model_synergy)
check_normality(sqrt_nitrite_model_synergy) # non-normality of residuals detected (p < .001)

sqrt_nitrite_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # linear model with both predictors and "+" interaction
summary(sqrt_nitrite_model_additive)
check_normality(sqrt_nitrite_model_additive) # non-normality of residuals detected (p < .001)

sqrt_nitrite_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = nitrite) # linear model with only precip
summary(sqrt_nitrite_model_precip)
check_normality(sqrt_nitrite_model_precip) # non-normality of residuals detected (p < .001)

sqrt_nitrite_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = nitrite) # linear model with only water temp
summary(sqrt_nitrite_model_temp)
check_normality(sqrt_nitrite_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(sqrt_nitrite_model_synergy, sqrt_nitrite_model_additive, sqrt_nitrite_model_precip, sqrt_nitrite_model_temp) # precip and temp models have lowest AIC

# Box cox transformation
lambda_nitrite <- BoxCox.lambda(nitrite$Result) # find the optimal lambda
transformed_conductivity <- BoxCox(nitrite$Result, lambda_nitrite) # transform raw values using lambda
nitrite$box_cox_transformed = transformed_nitrite # make new column in dataset
View(nitrite)

shapiro.test(nitrite$box_cox_transformed) # non-normal, p = 4.262e-06 

# Regressions - box cox transformed nitrite
box_cox_nitrite_model_synergy <- lm(box_cox_transformed ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # linear model with both predictors and "*" interaction
summary(box_cox_nitrite_model_synergy)
check_normality(box_cox_nitrite_model_synergy) # non-normality of residuals detected (p < .001)

box_cox_nitrite_model_additive <- lm(box_cox_transformed ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # linear model with both predictors and "+" interaction
summary(box_cox_nitrite_model_additive)
check_normality(box_cox_nitrite_model_additive) # non-normality of residuals detected (p < .001)

box_cox_nitrite_model_precip <- lm(box_cox_transformed ~ Total_Precipitation, data = nitrite) # linear model with only precip
summary(box_cox_nitrite_model_precip)
check_normality(box_cox_nitrite_model_precip) # non-normality of residuals detected (p < .001)

box_cox_nitrite_model_temp <- lm(box_cox_transformed ~ Mean_Water_Temp, data = nitrite) # linear model with only water temp
summary(box_cox_nitrite_model_temp)
check_normality(box_cox_nitrite_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(box_cox_nitrite_model_synergy, box_cox_nitrite_model_additive, box_cox_nitrite_model_precip, box_cox_nitrite_model_temp) # precip and temp models have lowest AIC

# Robust regression
nitrite_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = nitrite) # rlm model with both predictors and "*" interaction
summary(nitrite_rlm_synergy)

nitrite_rlm_additive <- rlm(Result ~ Total_Precipitation + Mean_Water_Temp, data = nitrite) # rlm model with both predictors and "+" interaction
summary(nitrite_rlm_additive)

nitrite_rlm_precip <- rlm(Result ~ Total_Precipitation, data = nitrite) # rlm model with only precip
summary(nitrite_rlm_precip)

nitrite_rlm_temp <- rlm(Result ~ Mean_Water_Temp, data = nitrite) # rlm model with only temp
summary(nitrite_rlm_temp)

compare_performance(nitrite_rlm_synergy, nitrite_rlm_additive, nitrite_rlm_precip, nitrite_rlm_temp) # precip model has lowest AIC

# Plot nitrite
ggplot(nitrite, aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Total precipitation (mm)", y = "Nitrite concentration (mg/L)") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(nitrite_rlm_precip)

## Phosphate regression ----------------------------------------------------------
phosphate <- read.csv("./data/water_quality/phosphate.csv")

# Visualize data
phosphate %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Phosphate concentration (mg/L)") +
  theme_minimal()

phosphate %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Phosphate concentration (mg/L)") +
  theme_minimal()

# Summary statistics
summary(phosphate$Result)
summary(phosphate$Total_Precipitation)
summary(phosphate$Mean_Water_Temp)

# Histograms
phosphate %>% # raw - right skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 0.005) +
  labs(x = "Phosphate concentration (mg/L)", y = "Frequency")

phosphate %>% # ln transformed - right skew
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed phosphate concentration (ln[mg/L])", y = "Frequency")

phosphate %>% # log transformed - right skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed phosphate concentration (log[mg/L])", y = "Frequency")

phosphate %>% # sqrt transformed - right skew
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Sqrt transformed phosphate concentration (sqrt[mg/L])", y = "Frequency")

# Check normality
shapiro_phosphate <- shapiro.test(phosphate$Result) %>% print() # non-normal, p = 7.303e-12
shapiro_phosphate_ln <- shapiro.test(phosphate$ln_transformed) %>% print() # non-normal, p = 2.116e-10
shapiro_phosphate_log <- shapiro.test(phosphate$log_transformed) %>% print() # non-normal, p = 2.116e-10
shapiro_phosphate_sqrt <- shapiro.test(phosphate$sqrt_transformed) %>% print() # non-normal, p = 0.0005937

# Regressions - raw phosphate
phosphate_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # linear model with both predictors and "*" interaction
summary(phosphate_model_synergy)
check_normality(phosphate_model_synergy) # non-normality of residuals detected (p < .001)

phosphate_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # linear model with both predictors and "+" interaction
summary(phosphate_model_additive)
check_normality(phosphate_model_additive) # non-normality of residuals detected (p < .001)

phosphate_model_precip <- lm(Result ~ Total_Precipitation, data = phosphate) # linear model with only precip
summary(phosphate_model_precip)
check_normality(phosphate_model_precip) # non-normality of residuals detected (p < .001)

phosphate_model_temp <- lm(Result ~ Mean_Water_Temp, data = phosphate) # linear model with only water temp
summary(phosphate_model_temp)
check_normality(phosphate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(phosphate_model_synergy, phosphate_model_additive, phosphate_model_precip, phosphate_model_temp) # precip model has lowest AIC

# Regressions - ln transformed phosphate
ln_phosphate_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # linear model with both predictors and "*" interaction
summary(ln_phosphate_model_synergy)
check_normality(ln_phosphate_model_synergy) # non-normality of residuals detected (p < .001)

ln_phosphate_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # linear model with both predictors and "+" interaction
summary(ln_phosphate_model_additive)
check_normality(ln_phosphate_model_additive) # non-normality of residuals detected (p < .001)

ln_phosphate_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = phosphate) # linear model with only precip
summary(ln_phosphate_model_precip)
check_normality(ln_phosphate_model_precip) # non-normality of residuals detected (p < .001)

ln_phosphate_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = phosphate) # linear model with only water temp
summary(ln_phosphate_model_temp)
check_normality(ln_phosphate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_phosphate_model_synergy, ln_phosphate_model_additive, ln_phosphate_model_precip, ln_phosphate_model_temp) # precip model has lowest AIC

# Regressions - log transformed phosphate
log_phosphate_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # linear model with both predictors and "*" interaction
summary(log_phosphate_model_synergy)
check_normality(log_phosphate_model_synergy) # non-normality of residuals detected (p < .001)

log_phosphate_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # linear model with both predictors and "+" interaction
summary(log_phosphate_model_additive)
check_normality(log_phosphate_model_additive) # non-normality of residuals detected (p < .001)

log_phosphate_model_precip <- lm(log_transformed ~ Total_Precipitation, data = phosphate) # linear model with only precip
summary(log_phosphate_model_precip)
check_normality(log_phosphate_model_precip) # non-normality of residuals detected (p < .001)

log_phosphate_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = phosphate) # linear model with only water temp
summary(log_phosphate_model_temp)
check_normality(log_phosphate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_phosphate_model_synergy, log_phosphate_model_additive, log_phosphate_model_precip, log_phosphate_model_temp) # precip model has lowest AIC

# Regressions - sqrt transformed phosphate
sqrt_phosphate_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # linear model with both predictors and "*" interaction
summary(sqrt_phosphate_model_synergy)
check_normality(sqrt_phosphate_model_synergy) # non-normality of residuals detected (p = 0.002)

sqrt_phosphate_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # linear model with both predictors and "+" interaction
summary(sqrt_phosphate_model_additive)
check_normality(sqrt_phosphate_model_additive) # non-normality of residuals detected (p = 0.003)

sqrt_phosphate_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = phosphate) # linear model with only precip
summary(sqrt_phosphate_model_precip)
check_normality(sqrt_phosphate_model_precip) # non-normality of residuals detected (p = 0.005)

sqrt_phosphate_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = phosphate) # linear model with only water temp
summary(sqrt_phosphate_model_temp)
check_normality(sqrt_phosphate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(sqrt_phosphate_model_synergy, sqrt_phosphate_model_additive, sqrt_phosphate_model_precip, sqrt_phosphate_model_temp) # precip model has lowest AIC

# Box cox transformation
lambda_phosphate <- BoxCox.lambda(phosphate$Result) # find the optimal lambda
transformed_conductivity <- BoxCox(phosphate$Result, lambda_phosphate) # transform raw values using lambda
phosphate$box_cox_transformed = transformed_phosphate # make new column in dataset
View(phosphate)

shapiro.test(phosphate$box_cox_transformed) # non-normal, p < 2.2e-16

# Regressions - box cox transformed phosphate
box_cox_phosphate_model_synergy <- lm(box_cox_transformed ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # linear model with both predictors and "*" interaction
summary(box_cox_phosphate_model_synergy)
check_normality(box_cox_phosphate_model_synergy) # non-normality of residuals detected (p < .001)

box_cox_phosphate_model_additive <- lm(box_cox_transformed ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # linear model with both predictors and "+" interaction
summary(box_cox_phosphate_model_additive)
check_normality(box_cox_phosphate_model_additive) # non-normality of residuals detected (p < .001)

box_cox_phosphate_model_precip <- lm(box_cox_transformed ~ Total_Precipitation, data = phosphate) # linear model with only precip
summary(box_cox_phosphate_model_precip)
check_normality(box_cox_phosphate_model_precip) # non-normality of residuals detected (p < .001)

box_cox_phosphate_model_temp <- lm(box_cox_transformed ~ Mean_Water_Temp, data = phosphate) # linear model with only water temp
summary(box_cox_phosphate_model_temp)
check_normality(box_cox_phosphate_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(box_cox_phosphate_model_synergy, box_cox_phosphate_model_additive, box_cox_phosphate_model_precip, box_cox_phosphate_model_temp) # precip models has lowest AIC

# Robust regression
phosphate_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = phosphate) # rlm model with both predictors and "*" interaction
phosphate_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = phosphate, maxit = 50) # need more steps
summary(phosphate_rlm_synergy)

phosphate_rlm_additive <- rlm(Result ~ Total_Precipitation + Mean_Water_Temp, data = phosphate) # rlm model with both predictors and "+" interaction
summary(phosphate_rlm_additive)

phosphate_rlm_precip <- rlm(Result ~ Total_Precipitation, data = phosphate) # rlm model with only precip
summary(phosphate_rlm_precip)

phosphate_rlm_temp <- rlm(Result ~ Mean_Water_Temp, data = phosphate) # rlm model with only temp
summary(phosphate_rlm_temp)

compare_performance(phosphate_rlm_synergy, phosphate_rlm_additive, phosphate_rlm_precip, phosphate_rlm_temp) # precip model has lowest AIC

# Plot phosphate
ggplot(phosphate, aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Total precipitation (mm)", y = "Phosphate concentration (mg/L)") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(phosphate_rlm_precip)

## Conductivity regression ----------------------------------------------------------
conductivity <- read.csv("./data/water_quality/conductivity.csv")

# Visualize data
conductivity %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Conductivity (uS/cm)") +
  theme_minimal()

conductivity %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Conductivity (uS/cm)") +
  theme_minimal()

# Summary statistics
summary(conductivity$Result)
summary(conductivity$Total_Precipitation)
summary(conductivity$Mean_Water_Temp)

# Histograms
conductivity %>% # raw - slight left skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 30) +
  labs(x = "Conductivity (uS/cm)", y = "Frequency")

conductivity %>% # ln transformed - left skew
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Ln transformed conductivity (uS/cm)", y = "Frequency")

conductivity %>% # log transformed - left skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Log transformed conductivity (uS/cm)", y = "Frequency")

conductivity %>% # sqrt transformed - left skew
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Sqrt transformed conductivity (uS/cm)", y = "Frequency")

# Check normality
shapiro_conductivity <- shapiro.test(conductivity$Result) %>% print() # non-normal, p = 1.937e-06
shapiro_conductivity_ln <- shapiro.test(conductivity$ln_transformed) %>% print() # non-normal, p = 4.744e-09
shapiro_conductivity_log <- shapiro.test(conductivity$log_transformed) %>% print() # non-normal, p = 4.744e-09
shapiro_conductivity_sqrt <- shapiro.test(conductivity$sqrt_transformed) %>% print() # non-normal, p = 1.044e-07

# Regressions - raw conductivity
conductivity_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # linear model with both predictors and "*" interaction
summary(conductivity_model_synergy)
check_normality(conductivity_model_synergy) # non-normality of residuals detected (p < .001)

conductivity_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # linear model with both predictors and "+" interaction
summary(conductivity_model_additive)
check_normality(conductivity_model_additive) # non-normality of residuals detected (p < .001)

conductivity_model_precip <- lm(Result ~ Total_Precipitation, data = conductivity) # linear model with only precip
summary(conductivity_model_precip)
check_normality(conductivity_model_precip) # non-normality of residuals detected (p < .001)

conductivity_model_temp <- lm(Result ~ Mean_Water_Temp, data = conductivity) # linear model with only water temp
summary(conductivity_model_temp)
check_normality(conductivity_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(conductivity_model_synergy, conductivity_model_additive, conductivity_model_precip, conductivity_model_temp) # additive model has lowest AIC

# Regressions - ln transformed conductivity
ln_conductivity_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # linear model with both predictors and "*" interaction
summary(ln_conductivity_model_synergy)
check_normality(ln_conductivity_model_synergy) # non-normality of residuals detected (p < .001)

ln_conductivity_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # linear model with both predictors and "+" interaction
summary(ln_conductivity_model_additive)
check_normality(ln_conductivity_model_additive) # non-normality of residuals detected (p < .001)

ln_conductivity_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = conductivity) # linear model with only precip
summary(ln_conductivity_model_precip)
check_normality(ln_conductivity_model_precip) # non-normality of residuals detected (p < .001)

ln_conductivity_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = conductivity) # linear model with only water temp
summary(ln_conductivity_model_temp)
check_normality(ln_conductivity_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_conductivity_model_synergy, ln_conductivity_model_additive, ln_conductivity_model_precip, ln_conductivity_model_temp) # precip model has lowest AIC

# Regressions - log transformed conductivity
log_conductivity_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # linear model with both predictors and "*" interaction
summary(log_conductivity_model_synergy)
check_normality(log_conductivity_model_synergy) # non-normality of residuals detected (p < .001)

log_conductivity_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # linear model with both predictors and "+" interaction
summary(log_conductivity_model_additive)
check_normality(log_conductivity_model_additive) # non-normality of residuals detected (p < .001)

log_conductivity_model_precip <- lm(log_transformed ~ Total_Precipitation, data = conductivity) # linear model with only precip
summary(log_conductivity_model_precip)
check_normality(log_conductivity_model_precip) # non-normality of residuals detected (p < .001)

log_conductivity_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = conductivity) # linear model with only water temp
summary(log_conductivity_model_temp)
check_normality(log_conductivity_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_conductivity_model_synergy, log_conductivity_model_additive, log_conductivity_model_precip, log_conductivity_model_temp) # additive model has lowest AIC

# Regressions - sqrt transformed conductivity
sqrt_conductivity_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # linear model with both predictors and "*" interaction
summary(sqrt_conductivity_model_synergy)
check_normality(sqrt_conductivity_model_synergy) # non-normality of residuals detected (p < .001)

sqrt_conductivity_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # linear model with both predictors and "+" interaction
summary(sqrt_conductivity_model_additive)
check_normality(sqrt_conductivity_model_additive) # non-normality of residuals detected (p < .001)

sqrt_conductivity_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = conductivity) # linear model with only precip
summary(sqrt_conductivity_model_precip)
check_normality(sqrt_conductivity_model_precip) # non-normality of residuals detected (p < .001)

sqrt_conductivity_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = conductivity) # linear model with only water temp
summary(sqrt_conductivity_model_temp)
check_normality(sqrt_conductivity_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(sqrt_conductivity_model_synergy, sqrt_conductivity_model_additive, sqrt_conductivity_model_precip, sqrt_conductivity_model_temp) # additive model has lowest AIC

# Box cox transformation
lambda_conductivity <- BoxCox.lambda(conductivity$Result) # find the optimal lambda
transformed_conductivity <- BoxCox(conductivity$Result, lambda_conductivity) # transform raw values using lambda
conductivity$box_cox_transformed = transformed_conductivity # make new column in dataset
View(conductivity)

shapiro.test(conductivity$box_cox_transformed) # non-normal, p = 0.000164 

# Regressions - box cox transformed conductivity
box_cox_conductivity_model_synergy <- lm(box_cox_transformed ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # linear model with both predictors and "*" interaction
summary(box_cox_conductivity_model_synergy)
check_normality(box_cox_conductivity_model_synergy) # non-normality of residuals detected (p < .001)

box_cox_conductivity_model_additive <- lm(box_cox_transformed ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # linear model with both predictors and "+" interaction
summary(box_cox_conductivity_model_additive)
check_normality(box_cox_conductivity_model_additive) # non-normality of residuals detected (p < .001)

box_cox_conductivity_model_precip <- lm(box_cox_transformed ~ Total_Precipitation, data = conductivity) # linear model with only precip
summary(box_cox_conductivity_model_precip)
check_normality(box_cox_conductivity_model_precip) # non-normality of residuals detected (p < .001)

box_cox_conductivity_model_temp <- lm(box_cox_transformed ~ Mean_Water_Temp, data = conductivity) # linear model with only water temp
summary(box_cox_conductivity_model_temp)
check_normality(box_cox_conductivity_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(box_cox_conductivity_model_synergy, box_cox_conductivity_model_additive, box_cox_conductivity_model_precip, box_cox_conductivity_model_temp) # additive model has lowest AIC

# Robust regression
conductivity_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = conductivity) # rlm model with both predictors and "*" interaction
summary(conductivity_rlm_synergy)

conductivity_rlm_additive <- rlm(Result ~ Total_Precipitation + Mean_Water_Temp, data = conductivity) # rlm model with both predictors and "+" interaction
summary(conductivity_rlm_additive)

conductivity_rlm_precip <- rlm(Result ~ Total_Precipitation, data = conductivity) # rlm model with only precip
summary(conductivity_rlm_precip)

conductivity_rlm_temp <- rlm(Result ~ Mean_Water_Temp, data = conductivity) # rlm model with only temp
summary(conductivity_rlm_temp)

compare_performance(conductivity_rlm_synergy, conductivity_rlm_additive, conductivity_rlm_precip, conductivity_rlm_temp) # additive model has lowest AIC

# Plot conductivity
ggplot(conductivity, aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Mean water temperature (C)", y = "Conductivity (uS/cm)") +
  theme_minimal()

ggplot(conductivity, aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Total precipitation (mm)", y = "Conductivity (uS/cm)") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(conductivity_rlm_additive)

## Dissolved oxygen regression -------------------------------------------------
do <- read.csv("./data/water_quality/do.csv")

# Visualize data
do %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "Dissolved oxygen (mg/L)") +
  theme_minimal()

do %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "Dissolved oxygen (mg/L)") +
  theme_minimal()

# Summary statistics
summary(do$Result)
summary(do$Total_Precipitation)
summary(do$Mean_Water_Temp)

# Histograms
do %>% # raw - right skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Dissolved oxygen (mg/L)", y = "Frequency")

do %>% # ln transformed - normal
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Ln transformed dissolved oxygen (mg/L)", y = "Frequency")

do %>% # log transformed - right skew
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Log transformed dissolved oxygen (mg/L)", y = "Frequency")

do %>% # sqrt transformed - right skew
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Sqrt transformed dissolved oxygen (mg/L)", y = "Frequency")

# Check normality
shapiro_do <- shapiro.test(do$Result) %>% print() # borderline normal, p = 0.04306
shapiro_do_ln <- shapiro.test(do$ln_transformed) %>% print() # normal, p = 0.5089
shapiro_do_log <- shapiro.test(do$log_transformed) %>% print() # normal, p = 0.5089
shapiro_do_sqrt <- shapiro.test(do$sqrt_transformed) %>% print() # normal, p = 0.2771

# Regressions - raw do
do_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = do) # linear model with both predictors and "*" interaction
summary(do_model_synergy)
check_normality(do_model_synergy) # non-normality of residuals detected (p < .001)

do_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = do) # linear model with both predictors and "+" interaction
summary(do_model_additive)
check_normality(do_model_additive) # non-normality of residuals detected (p < .001)

do_model_precip <- lm(Result ~ Total_Precipitation, data = do) # linear model with only precip
summary(do_model_precip)
check_normality(do_model_precip) # residuals appear as normally distributed (p = 0.104)

do_model_temp <- lm(Result ~ Mean_Water_Temp, data = do) # linear model with only water temp
summary(do_model_temp)
check_normality(do_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(do_model_synergy, do_model_additive, do_model_precip, do_model_temp) # additive model has lowest AIC

# Regressions - ln transformed do
ln_do_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = do) # linear model with both predictors and "*" interaction
summary(ln_do_model_synergy)
check_normality(ln_do_model_synergy) # non-normality of residuals detected (p = 0.002)

ln_do_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = do) # linear model with both predictors and "+" interaction
summary(ln_do_model_additive)
check_normality(ln_do_model_additive) # non-normality of residuals detected (p < .001)

ln_do_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = do) # linear model with only precip
summary(ln_do_model_precip)
check_normality(ln_do_model_precip) # residuals appear as normally distributed (p = 0.539)

ln_do_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = do) # linear model with only water temp
summary(ln_do_model_temp)
check_normality(ln_do_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(ln_do_model_synergy, ln_do_model_additive, ln_do_model_precip, ln_do_model_temp) # synergistic model has lowest AIC

# Regressions - log transformed do
log_do_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = do) # linear model with both predictors and "*" interaction
summary(log_do_model_synergy)
check_normality(log_do_model_synergy) # non-normality of residuals detected (p = 0.002)

log_do_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = do) # linear model with both predictors and "+" interaction
summary(log_do_model_additive)
check_normality(log_do_model_additive) # non-normality of residuals detected (p < .001)

log_do_model_precip <- lm(log_transformed ~ Total_Precipitation, data = do) # linear model with only precip
summary(log_do_model_precip)
check_normality(log_do_model_precip) # residuals appear as normally distributed (p = 0.539)

log_do_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = do) # linear model with only water temp
summary(log_do_model_temp)
check_normality(log_do_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(log_do_model_synergy, log_do_model_additive, log_do_model_precip, log_do_model_temp) # synergistic model has lowest AIC

# Regressions - sqrt transformed do
sqrt_do_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = do) # linear model with both predictors and "*" interaction
summary(sqrt_do_model_synergy)
check_normality(sqrt_do_model_synergy) # non-normality of residuals detected (p = 0.001)

sqrt_do_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = do) # linear model with both predictors and "+" interaction
summary(sqrt_do_model_additive)
check_normality(sqrt_do_model_additive) # non-normality of residuals detected (p < .001)

sqrt_do_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = do) # linear model with only precip
summary(sqrt_do_model_precip)
check_normality(sqrt_do_model_precip) # residuals appear as normally distributed (p = 0.401)

sqrt_do_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = do) # linear model with only water temp
summary(sqrt_do_model_temp)
check_normality(sqrt_do_model_temp) # non-normality of residuals detected (p < .001)

compare_performance(sqrt_do_model_synergy, sqrt_do_model_additive, sqrt_do_model_precip, sqrt_do_model_temp) # synergistic model has lowest AIC

# Compare performance of all models with normal residuals
compare_performance(do_model_precip, ln_do_model_precip, log_do_model_precip, sqrt_do_model_precip) # log_do_model_precip has lowest AIC

# Plot do
ggplot(do, aes(x = Total_Precipitation, y = log_transformed)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Total precipitation (mm)", y = "Dissolved oxygen (mg/L)") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(log_do_model_precip)

## pH regression ---------------------------------------------------------------
ph <- read.csv("./data/water_quality/ph.csv")

# Visualize data
ph %>%
  ggplot(aes(x = Total_Precipitation, y = Result)) +
  geom_point() +
  labs(x = "Total Precipitation (over previous 36 hours)",
       y = "pH") +
  theme_minimal()

ph %>%
  ggplot(aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  labs(x = "Mean water temp (over previous 36 hours)",
       y = "pH") +
  theme_minimal()

# Summary statistics
summary(ph$Result)
summary(ph$Total_Precipitation)
summary(ph$Mean_Water_Temp)

# Histograms
ph %>% # raw - slight right skew
  ggplot(aes(x = Result)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "pH", y = "Frequency")

ph %>% # ln transformed - approximately normal
  ggplot(aes(x = ln_transformed)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Ln transformed pH", y = "Frequency")

ph %>% # log transformed - approximately normal
  ggplot(aes(x = log_transformed)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Log transformed pH", y = "Frequency")

ph %>% # sqrt transformed - approximately normal
  ggplot(aes(x = sqrt_transformed)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Square root transformed pH", y = "Frequency")

# Check normality
shapiro_ph <- shapiro.test(ph$Result) %>% print() # normal, p = 0.07346
shapiro_ph_ln <- shapiro.test(ph$ln_transformed) %>% print() # normal, p = 0.1438
shapiro_ph_log <- shapiro.test(ph$log_transformed) %>% print() # normal, p = 0.1438
shapiro_ph_sqrt <- shapiro.test(ph$sqrt_transformed) %>% print() # normal, p = 0.1046

# Regressions - raw pH
ph_model_synergy <- lm(Result ~ Total_Precipitation * Mean_Water_Temp, data = ph) # linear model with both predictors and "*" interaction
summary(ph_model_synergy)
check_normality(ph_model_synergy) # non-normality of residuals detected (p = 0.002)

ph_model_additive <- lm(Result ~ Total_Precipitation + Mean_Water_Temp, data = ph) # linear model with both predictors and "+" interaction
summary(ph_model_additive)
check_normality(ph_model_additive) # non-normality of residuals detected (p = 0.002)

ph_model_precip <- lm(Result ~ Total_Precipitation, data = ph) # linear model with only precip
summary(ph_model_precip)
check_normality(ph_model_precip) # non-normality of residuals detected (p = 0.008)

ph_model_temp <- lm(Result ~ Mean_Water_Temp, data = ph) # linear model with only water temp
summary(ph_model_temp)
check_normality(ph_model_temp) # non-normality of residuals detected (p = 0.007)

compare_performance(ph_model_synergy, ph_model_additive, ph_model_precip, ph_model_temp) # temp model has lowest AIC

# Regressions - ln transformed pH
ln_ph_model_synergy <- lm(ln_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ph) # linear model with both predictors and "*" interaction
summary(ln_ph_model_synergy)
check_normality(ln_ph_model_synergy) # non-normality of residuals detected (p = 0.004)

ln_ph_model_additive <- lm(ln_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ph) # linear model with both predictors and "+" interaction
summary(ln_ph_model_additive)
check_normality(ln_ph_model_additive) # non-normality of residuals detected (p = 0.004)

ln_ph_model_precip <- lm(ln_transformed ~ Total_Precipitation, data = ph) # linear model with only precip
summary(ln_ph_model_precip)
check_normality(ln_ph_model_precip) # non-normality of residuals detected (p = 0.018)

ln_ph_model_temp <- lm(ln_transformed ~ Mean_Water_Temp, data = ph) # linear model with only water temp
summary(ln_ph_model_temp)
check_normality(ln_ph_model_temp) # non-normality of residuals detected (p = 0.014)

compare_performance(ln_ph_model_synergy, ln_ph_model_additive, ln_ph_model_precip, ln_ph_model_temp) # additive and temp models have lowest AIC

# Regressions - log transformed pH
log_ph_model_synergy <- lm(log_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ph) # linear model with both predictors and "*" interaction
summary(log_ph_model_synergy)
check_normality(log_ph_model_synergy) # non-normality of residuals detected (p = 0.004)

log_ph_model_additive <- lm(log_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ph) # linear model with both predictors and "+" interaction
summary(log_ph_model_additive)
check_normality(log_ph_model_additive) # non-normality of residuals detected (p = 0.004)

log_ph_model_precip <- lm(log_transformed ~ Total_Precipitation, data = ph) # linear model with only precip
summary(log_ph_model_precip)
check_normality(log_ph_model_precip) # non-normality of residuals detected (p = 0.018)

log_ph_model_temp <- lm(log_transformed ~ Mean_Water_Temp, data = ph) # linear model with only water temp
summary(log_ph_model_temp)
check_normality(log_ph_model_temp) # non-normality of residuals detected (p = 0.014)

compare_performance(log_ph_model_synergy, log_ph_model_additive, log_ph_model_precip, log_ph_model_temp) # additive and temp models have lowest AIC

# Regressions - sqrt transformed ph
sqrt_ph_model_synergy <- lm(sqrt_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ph) # linear model with both predictors and "*" interaction
summary(sqrt_ph_model_synergy)
check_normality(sqrt_ph_model_synergy) # non-normality of residuals detected (p = 0.003)

sqrt_ph_model_additive <- lm(sqrt_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ph) # linear model with both predictors and "+" interaction
summary(sqrt_ph_model_additive)
check_normality(sqrt_ph_model_additive) # non-normality of residuals detected (p = 0.003)

sqrt_ph_model_precip <- lm(sqrt_transformed ~ Total_Precipitation, data = ph) # linear model with only precip
summary(sqrt_ph_model_precip)
check_normality(sqrt_ph_model_precip) # non-normality of residuals detected (p = 0.012)

sqrt_ph_model_temp <- lm(sqrt_transformed ~ Mean_Water_Temp, data = ph) # linear model with only water temp
summary(sqrt_ph_model_temp)
check_normality(sqrt_ph_model_temp) # non-normality of residuals detected (p = 0.010)

compare_performance(sqrt_ph_model_synergy, sqrt_ph_model_additive, sqrt_ph_model_precip, sqrt_ph_model_temp) # temp model has lowest AIC

# Box cox transformation
lambda_ph <- BoxCox.lambda(ph$Result) # find the optimal lambda
transformed_ph <- BoxCox(ph$Result, lambda_ph) # transform raw values using lambda
ph$box_cox_transformed = transformed_ph # make new column in dataset
View(ph)

shapiro.test(ph$box_cox_transformed) # normal, p = 0.2425

# Regressions - box cox transformed ph
box_cox_ph_model_synergy <- lm(box_cox_transformed ~ Total_Precipitation * Mean_Water_Temp, data = ph) # linear model with both predictors and "*" interaction
summary(box_cox_ph_model_synergy)
check_normality(box_cox_ph_model_synergy) # non-normality of residuals detected (p = 0.009)

box_cox_ph_model_additive <- lm(box_cox_transformed ~ Total_Precipitation + Mean_Water_Temp, data = ph) # linear model with both predictors and "+" interaction
summary(box_cox_ph_model_additive)
check_normality(box_cox_ph_model_additive) # non-normality of residuals detected (p = 0.008)

box_cox_ph_model_precip <- lm(box_cox_transformed ~ Total_Precipitation, data = ph) # linear model with only precip
summary(box_cox_ph_model_precip)
check_normality(box_cox_ph_model_precip) # non-normality of residuals detected (p = 0.037)

box_cox_ph_model_temp <- lm(box_cox_transformed ~ Mean_Water_Temp, data = ph) # linear model with only water temp
summary(box_cox_ph_model_temp)
check_normality(box_cox_ph_model_temp) # non-normality of residuals detected (p = 0.026)

compare_performance(box_cox_ph_model_synergy, box_cox_ph_model_additive, box_cox_ph_model_precip, box_cox_ph_model_temp) # additive model has lowest AIC

# Robust regression
ph_rlm_synergy <- rlm(Result ~ Total_Precipitation * Mean_Water_Temp, data = ph) # rlm model with both predictors and "*" interaction
summary(ph_rlm_synergy)

ph_rlm_additive <- rlm(Result ~ Total_Precipitation + Mean_Water_Temp, data = ph) # rlm model with both predictors and "+" interaction
summary(ph_rlm_additive)

ph_rlm_precip <- rlm(Result ~ Total_Precipitation, data = ph) # rlm model with only precip
summary(ph_rlm_precip)

ph_rlm_temp <- rlm(Result ~ Mean_Water_Temp, data = ph) # rlm model with only temp
summary(ph_rlm_temp)

compare_performance(ph_rlm_synergy, ph_rlm_additive, ph_rlm_precip, ph_rlm_temp) # temp model has lowest AIC

# Plot ph
ggplot(ph, aes(x = Mean_Water_Temp, y = Result)) +
  geom_point() +
  geom_smooth(method=lm, color="darkorchid4") +
  labs(x = "Mean water temperature (C)", y = "pH") +
  theme_minimal()

par(mfrow = c(2, 2))
plot(ph_rlm_temp)

## Air and water temp regression -----------------------------------------------
# Visualize data
combined_daily_avg %>%
  ggplot(aes(x = daily_avg_air_temp, y = daily_avg_water_temp)) +
  geom_point() +
  labs(x = "Daily average air temperature (C)",
       y = "Daily average water temperature (C)") +
  theme_minimal()

# Summary statistics
summary(combined_daily_avg$daily_avg_air_temp)
summary(combined_daily_avg$daily_avg_water_temp)

# Histograms
combined_daily_avg %>% # raw - bimodal (summer and winter peaks)
  ggplot(aes(x = daily_avg_air_temp)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Daily average air temperature (C)", y = "Frequency")

combined_daily_avg %>% # raw - left skew
  ggplot(aes(x = daily_avg_water_temp)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Daily average water temperature (C)", y = "Frequency")

combined_daily_avg %>% # ln transformed - left skew
  ggplot(aes(x = ln_transformed_air)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed daily average air temperature (ln[C])", y = "Frequency")

combined_daily_avg %>% # ln transformed - left skew
  ggplot(aes(x = ln_transformed_water)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Ln transformed daily average water temperature (ln[C])", y = "Frequency")

combined_daily_avg %>% # log transformed - left skew
  ggplot(aes(x = log_transformed_air)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed daily average air temperature (log[C])", y = "Frequency")

combined_daily_avg %>% # log transformed - left skew
  ggplot(aes(x = log_transformed_water)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed daily average water temperature (log[C])", y = "Frequency")

combined_daily_avg %>% # sqrt transformed - left skew
  ggplot(aes(x = sqrt_transformed_air)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Square root transformed daily average air temperature (sqrt[C])", y = "Frequency")

combined_daily_avg %>% # sqrt transformed - left skew
  ggplot(aes(x = sqrt_transformed_water)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Square root transformed daily average water temperature (sqrt[C])", y = "Frequency")

# Check normality with Anderson-Darling test (too many values for a Shapiro test)
ad.test(combined_daily_avg$daily_avg_air_temp) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$daily_avg_water_temp) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$ln_transformed_air) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$ln_transformed_water) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$log_transformed_air) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$log_transformed_water) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$sqrt_transformed_air) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$sqrt_transformed_water) # non-normal, p < 2.2e-16

# Regressions
raw_temp_model <- lm(daily_avg_water_temp ~ daily_avg_air_temp, data = combined_daily_avg) # raw data
summary(raw_temp_model)
check_normality(raw_temp_model) # non-normality of residuals detected (p < .001)

ln_temp_model <- lm(ln_transformed_water ~ ln_transformed_air, data = combined_daily_avg) # ln transformed data
summary(ln_temp_model)
check_normality(ln_temp_model) # non-normality of residuals detected (p < .001)

log_temp_model <- lm(log_transformed_water ~ log_transformed_air, data = combined_daily_avg) # log transformed data
summary(log_temp_model)
check_normality(log_temp_model) # non-normality of residuals detected (p < .001)

sqrt_temp_model <- lm(sqrt_transformed_water ~ sqrt_transformed_air, data = combined_daily_avg) # sqrt transformed data
summary(sqrt_temp_model)
check_normality(sqrt_temp_model) # non-normality of residuals detected (p < .001)

compare_performance(raw_temp_model, ln_temp_model, log_temp_model, sqrt_temp_model) # log temp model has lowest AIC

#Box cox transformation
lambda_air <- BoxCox.lambda(combined_daily_avg$daily_avg_air_temp) # find the optimal lambda for air temp
transformed_air_temp <- BoxCox(combined_daily_avg$daily_avg_air_temp, lambda_air) # transform raw air temp using lambda
combined_daily_avg$box_cox_transformed_air = transformed_air_temp # make new column in dataset

lambda_water <- BoxCox.lambda(combined_daily_avg$daily_avg_water_temp) # find the optimal lambda for water temp
transformed_water_temp <- BoxCox(combined_daily_avg$daily_avg_water_temp, lambda_water) # transform raw water temp using lambda
combined_daily_avg$box_cox_transformed_water = transformed_water_temp # make new column in dataset

View(combined_daily_avg)

ad.test(combined_daily_avg$box_cox_transformed_air) # non-normal, p < 2.2e-16
ad.test(combined_daily_avg$box_cox_transformed_water) # non-normal, p < 2.2e-16

# Regression - box cox transformed temperature 
box_cox_model <- lm(box_cox_transformed_water ~ box_cox_transformed_air, data = combined_daily_avg)
summary(box_cox_model)
check_normality(box_cox_model) # non-normality of residuals detected (p < .001)

# Robust regression
temp_rlm_model <- rlm(daily_avg_water_temp ~ daily_avg_air_temp, data = combined_daily_avg)
summary(temp_rlm_model)

# Set the significance level
alpha <- 0.05

# Calculate the critical t-value for a two-tailed test
critical_t_value <- qt(1 - alpha/2, df = 4441)

critical_t_value

# Plot temperature relationship
combined_daily_avg %>%
  ggplot(aes(x = daily_avg_air_temp, y = daily_avg_water_temp)) +
  geom_point(shape = 16, size = 3) +
  labs(x = expression("Daily average air temperature (" * degree * "C)"),
       y = expression("Daily average water temperature (" * degree * "C)")) +
  theme_bw(base_size = 24) +
  geom_smooth(method = "lm", colour = "royalblue", se = FALSE, linewidth = 2)

par(mfrow = c(2, 2))
plot(temp_rlm_model)
