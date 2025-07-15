# AIR TEMPERATURE

# Load packages ----------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

# Import data -----------------------------------------------------------------
# Read Excel file and combine data sheets
CSM_file_path <- "./data/air_temp/CSM_raw_air_temp.xlsx" # define file path
CSM_sheet_names <- excel_sheets(CSM_file_path) # define sheet names

CSM_list <- lapply(CSM_sheet_names, function(CSM_sheet_names) { # read each sheet and store in a list
  read_excel(CSM_file_path, sheet = CSM_sheet_names)
})

CSM_combined_data <- bind_rows(CSM_list) # combine all data frames

View(CSM_combined_data)

# Data prep -------------------------------------------------------------------
# Adjust precision so all values have 2 decimal places
CSM_combined_data <- CSM_combined_data %>%
  mutate(Value = round(Value, 2))

# Check for missing values
sum(is.na(CSM_combined_data))

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)

years_CSM <- unique(stringr::str_extract(CSM_sheet_names, "\\d{4}"))

missing_CSM <- (setdiff(year_range, years_CSM))

print(missing_CSM)

# Convert timestamp and extract year and month 
CSM_combined_data <- CSM_combined_data %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Aggregate data by year and month, and get metrics
CSM_results <- CSM_combined_data %>%
  group_by(Year, Month) %>%
  summarise(Minimum = min(Value, na.rm = TRUE),
            Maximum = max(Value, na.rm = TRUE),
            Average = mean(Value, na.rm = TRUE),
            Num_Samples = n(),
            .groups = 'drop')

# Export results as csv files into the R project
write.csv(CSM_results, "./data/air_temp/CSM_summarized_air_temp.csv")

# Most extreme values ----------------------------------------------------------
# Identify 5 highest average temp months
hot5_CSM <- CSM_results %>%
  arrange(desc(Average)) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)

# Identify 5 lowest average temp months
cold5_CSM <- CSM_results %>%
  arrange(Average) %>%
  slice_head(n = 5) %>%
  select(Year, Month, Average)