# WATER TEMPERATURE

## Load packages ---------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

## Import data -----------------------------------------------------------------
# Read Excel files and combine sheets 
SR10_file_path <- "./data/water_temp/SR10_raw_water_temp.xlsx" # define file path
ABF_file_path <- "./data/water_temp/ABF_raw_water_temp.xlsx" # define file path

SR10_sheet_names <- excel_sheets(SR10_file_path) # define sheet names
ABF_sheet_names <- excel_sheets(ABF_file_path) # define sheet names

SR10_list <- lapply(SR10_sheet_names, function(SR10_sheet_names) { # read each sheet and store in a list
  read_excel(SR10_file_path, sheet = SR10_sheet_names)
})
ABF_list <- lapply(ABF_sheet_names, function(ABF_sheet_names) { # read each sheet and store in a list
  read_excel(ABF_file_path, sheet = ABF_sheet_names)
})

SR10_combined_data <- bind_rows(SR10_list) # combine all data frames
ABF_combined_data <- bind_rows(ABF_list) # combine all data frames

## Data prep -------------------------------------------------------------------
# Adjust precision for Side Road 10 so all values have 1 decimal place
SR10_combined_data <- SR10_combined_data %>%
  mutate(Value = round(Value, 1))

# Adjust precision for Aberfoyle so all values have 1 decimal place
ABF_combined_data <- ABF_combined_data %>%
  mutate(Value = round(Value, 1))

# Check for missing values
sum(is.na(SR10_combined_data))
sum(is.na(ABF_combined_data))

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)

years_SR10 <- unique(stringr::str_extract(SR10_sheet_names, "\\d{4}"))

years_ABF <- unique(stringr::str_extract(ABF_sheet_names, "\\d{4}"))

# Identify missing years for Side Road 10
missing_SR10 <- (setdiff(year_range, years_SR10))
print(missing_SR10)

# Identify missing years for Side Road 10
missing_ABF <- (setdiff(year_range, years_ABF))
print(missing_ABF)

# Convert Timestamp and extract year and month for Side Road 10
SR10_combined_data <- SR10_combined_data %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Convert Timestamp and extract year and month for Aberfoyle
ABF_combined_data <- ABF_combined_data %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Aggregate data by year and month, and get metrics 
# Aggregate data for Side Road 10
SR10_results <- SR10_combined_data %>%
  group_by(Year, Month) %>%
  summarise(Minimum = min(Value, na.rm = TRUE),
            Maximum = max(Value, na.rm = TRUE),
            Average = mean(Value, na.rm = TRUE),
            Num_Samples = n(),
            .groups = 'drop')

# Aggregate data for Aberfoyle
ABF_results <- ABF_combined_data %>%
  group_by(Year, Month) %>%
  summarise(Minimum = min(Value, na.rm = TRUE),
            Maximum = max(Value, na.rm = TRUE),
            Average = mean(Value, na.rm = TRUE),
            Num_Samples = n(),
            .groups = 'drop')

## Export results as csv files into the R project
write.csv(SR10_results, "./data/water_temp/SR10_summarized_water_temp.csv")
write.csv(ABF_results, "./data/water_temp/ABF_summarized_water_temp.csv")

## Most extreme values ---------------------------------------------------------
# Identify 5 highest average temp months for Side Road 10
hot5_SR10 <- SR10_results %>%
  arrange(desc(Average)) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(hot5_SR10)

# Identify 5 lowest average temp months for Side Road 10
cold5_SR10 <- SR10_results %>%
  arrange(Average) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(cold5_SR10)

# Identify 5 highest average temp months for Aberfoyle
hot5_ABF <- ABF_results %>%
  arrange(desc(Average)) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(hot5_ABF)

# Identify 5 lowest average temp months for Aberfoyle
cold5_ABF <- ABF_results %>%
  arrange(Average) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(cold5_ABF)

## Comparing gauges ------------------------------------------------------------
# Test normality
shapiro.test(SR10_results$Average) # non-normal, p-value = 3.484e-10
shapiro.test(SR10_results$Maximum) # non-normal, p-value = 1.773e-15
shapiro.test(ABF_results$Average) # non-normal, p-value = 2.484e-11
shapiro.test(ABF_results$Maximum) # non-normal, p-value = 1.844e-09

# Compare average monthly temp betweeen the two gauge stations
gauge_wt_avg <- wilcox.test(SR10_results$Average, ABF_results$Average, var.equal = TRUE)
print(gauge_wt_avg)

# Compare maximum monthly temp betweeen the two gauge stations
gauge_wt_max <- wilcox.test(SR10_results$Maximum, ABF_results$Maximum, var.equal = TRUE)
print(gauge_wt_max)

# Effect size
mean(ABF_results$Maximum) - mean(SR10_results$Maximum)

## Historical data -------------------------------------------------------------
# Mann-Whitney test of 1994 vs. 2023 summer data, see p. 117 in 1996 report
mc11_summer_1994_monthly_avg <- c(6.0, 11.3, 15.2, 17.6, 16.6, 11.1, 7.6)

mc04_summer_1994_monthly_avg <- c(5.8, 13.1, 17.4, 21.7, 18.8, 16.2, 11.3)

SR10_summer_2023 <- SR10_results %>%
  filter(Year == 2023, Month >= 4, Month <= 10)

SR10_summer_2023_monthly_avg <- SR10_summer_2023$Average

ABF_summer_2023 <- ABF_results %>%
  filter(Year == 2023, Month >= 4, Month <= 10)

ABF_summer_2023_monthly_avg <- ABF_summer_2023$Average

SR10_mann_whitney_test <- wilcox.test(mc11_summer_1994_monthly_avg, SR10_summer_2023_monthly_avg)
print(SR10_mann_whitney_test)

ABF_mann_whitney_test <- wilcox.test(mc04_summer_1994_monthly_avg, ABF_summer_2023_monthly_avg)
print(ABF_mann_whitney_test)