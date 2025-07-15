# WATER FLOW

# Load packages ----------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require(remotes, quietly = TRUE)){
  install.packages("remotes")
  library(remotes)
}

if (!require(ContDataQC, quietly = TRUE)){
  install.packages("ContDataQC")
  library(ContDataQC)
} #one way

remotes::install_github("leppott/ContDataQC", build_vignettes = FALSE) #another way if issues with R version
library(ContDataQC)

## Import data -----------------------------------------------------------------
# Read Excel files and combine sheets 
SR10_file_path <- "./data/water_flow/SR10_raw_flow.xlsx" # define file path
ABF_file_path <- "./data/water_flow/ABF_raw_flow.xlsx" # define file path

SR10_sheet_names <- excel_sheets(SR10_file_path) # define sheet names
ABF_sheet_names <- excel_sheets(ABF_file_path) # define sheet names

SR10_list <- lapply(SR10_sheet_names, function(SR10_sheet_names) { # read each sheet and store in a list
  read_excel(SR10_file_path, sheet = SR10_sheet_names)
})
ABF_list <- lapply(ABF_sheet_names, function(ABF_sheet_names) { # reach each sheet and store in a list
  read_excel(ABF_file_path, sheet = ABF_sheet_names)
})

SR10_combined_data <- bind_rows(SR10_list) # combine all data frames
ABF_combined_data <- bind_rows(ABF_list) # combine all data frames

## Data prep -------------------------------------------------------------------
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

# Identify missing years for Aberfoyle
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

## Aggregate data by year and month, and get metrics 
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
write.csv(SR10_results, "./data/water_flow/SR10_summarized_flow.csv")
write.csv(ABF_results, "./data/water_flow/ABF_summarized_flow.csv")

## Most extreme values --------------------------------------------------------- 
# Identify 5 highest average flow months for Side Road 10
high5_SR10 <- SR10_results %>%
  arrange(desc(Average)) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(high5_SR10)

## Identify 5 highest average flow months for Aberfoyle
high5_ABF <- ABF_results %>%
  arrange(desc(Average)) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

print(high5_ABF)

## Comparing gauges ------------------------------------------------------------
mean(ABF_combined_data$Value)
sd(ABF_combined_data$Value)

mean(SR10_combined_data$Value)
sd(SR10_combined_data$Value)

# Test normality
shapiro.test(SR10_results$Average) # non-normal, p-value = 3.484e-10
shapiro.test(ABF_results$Average) # non-normal, p-value = 2.484e-11

# Compare annual average flow between the gauges
SR10_annual_flow <- SR10_combined_data %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

ABF_annual_flow <- ABF_combined_data %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

# Remove years with incomplete flow data
SR10_annual_flow <- SR10_annual_flow %>%
  filter(!Year %in% c(1999)) # remove incomplete years

ABF_annual_flow <- ABF_annual_flow %>%
  filter(!Year %in% c(2002,2020)) # remove incomplete years

gauge_flow <- wilcox.test(SR10_annual_flow$Mean, ABF_annual_flow$Mean, var.equal = TRUE)
print(gauge_flow)

nrow(ABF_annual_flow)
nrow(SR10_annual_flow)

# Effect size
mean(SR10_annual_flow$Mean) - mean(ABF_annual_flow$Mean)

## Flashiness ------------------------------------------------------------------

# Based on: "A New Flashiness Index: Characteristics and Applications to Midwestern Rivers and Streams"

# Side Rd 10
# Convert Timestamp to Date format and calculate daily mean flow
SR10_daily_flow <- SR10_combined_data %>%
  mutate(Date = as.Date(Timestamp)) %>%
  group_by(Year, Date) %>%
  summarize(DailyMeanFlow = mean(Value), .groups = 'drop')

# Calculate overall RBI - value of 0.17 for a watershed of 104 sq km means that it is on the stable end of the continuum
SR10_RBI <- RBIcalc(SR10_daily_flow$DailyMeanFlow)
print(SR10_RBI)

# Aberfoyle
ABF_daily_flow <- ABF_combined_data %>%
  mutate(Date = as.Date(Timestamp)) %>%
  group_by(Year, Date) %>%
  summarize(DailyMeanFlow = mean(Value), .groups = 'drop')

# Calculate overall RBI - value of 0.2 for a watershed of 104 sq km means that it is on the stable end of the continuum
ABF_RBI <- RBIcalc(ABF_daily_flow$DailyMeanFlow)
print(ABF_RBI)

# Calculate annual RBI value, and assess changes over time
# Calculate day-to-day changes in flow, and their absolute values
SR10_daily_flow <- SR10_daily_flow %>%
  mutate(Change = c(NA, diff(DailyMeanFlow)),
         AbsChange = abs(Change))

ABF_daily_flow <- ABF_daily_flow %>%
  mutate(Change = c(NA, diff(DailyMeanFlow)),
         AbsChange = abs(Change))

# Calculate annual sums of absolute changes and total annual flows
SR10_annual_flow <- SR10_daily_flow %>%
  group_by(Year) %>%
  summarize(SumAbsChanges = sum(AbsChange, na.rm = TRUE),
            TotalFlow = sum(DailyMeanFlow),
            FlashinessIndex = SumAbsChanges / TotalFlow)

View(SR10_annual_flow)

ABF_annual_flow <- ABF_daily_flow %>%
  group_by(Year) %>%
  summarize(SumAbsChanges = sum(AbsChange, na.rm = TRUE),
            TotalFlow = sum(DailyMeanFlow),
            FlashinessIndex = SumAbsChanges / TotalFlow)

View(ABF_annual_flow)

# Export annual flashiness dataframes
write.csv(SR10_annual_flow, "./data/water_flow/SR10_annual_flashiness.csv")

write.csv(ABF_annual_flow, "./data/water_flow/ABF_annual_flashiness.csv")

SR10_annual_flashiness <- read.csv("./data/water_flow/SR10_annual_flashiness.csv")
ABF_annual_flashiness <- read.csv("./data/water_flow/ABF_annual_flashiness.csv")

# Remove years with incomplete flashiness data
SR10_annual_flashiness <- SR10_annual_flashiness %>%
  filter(!Year %in% c(1999)) #remove incomplete years

ABF_annual_flashiness <- ABF_annual_flashiness %>%
  filter(!Year %in% c(2002,2020)) #remove incomplete years

# Plotting the BCI value over years
ggplot(SR10_annual_flashiness, aes(x = Year, y = FlashinessIndex)) +
  geom_line(group = 1, color = "black", linetype=2) +  
  geom_point(color = "black") + 
  labs(x = "Year", y = "Flashiness Index") +
  theme_minimal()

ggplot(ABF_annual_flashiness, aes(x = Year, y = FlashinessIndex)) +
  geom_line(group = 1, color = "black", linetype=2) +  
  geom_point(color = "black") + 
  labs(x = "Year", y = "Flashiness Index") +
  theme_minimal()

# Comparing gauges
# Test normality
shapiro.test(SR10_annual_flashiness$FlashinessIndex) # non-normal, p-value = 0.001641
shapiro.test(ABF_annual_flashiness$FlashinessIndex) # normal, p-value = 0.08487

# Compare annual flashiness between the gauges - significant
gauge_flashiness <- wilcox.test(SR10_annual_flashiness$FlashinessIndex, ABF_annual_flashiness$FlashinessIndex, var.equal = TRUE)
print(gauge_flashiness)

# Effect size
mean(ABF_annual_flashiness$FlashinessIndex) - mean(SR10_annual_flashiness$FlashinessIndex)
