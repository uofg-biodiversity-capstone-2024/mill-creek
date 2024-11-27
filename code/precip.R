### PRECIPITATION

## Load packages ---------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

## Import data -----------------------------------------------------------------
# Read Excel file and combine sheets
CSM_file_path <- "./data/precipitation/CSM_raw_precip.xlsx" # define file path
CSM_sheet_names <- excel_sheets(CSM_file_path) # define sheet names

CSM_list <- lapply(CSM_sheet_names, function(CSM_sheet_names) { # read each sheet and store in a list
  read_excel(CSM_file_path, sheet = CSM_sheet_names)
})

CSM_combined_data <- bind_rows(CSM_list) # combine all data frames

View(CSM_combined_data)

## Data prep -------------------------------------------------------------------
# Check for missing values
sum(is.na(CSM_combined_data))

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)

years_CSM <- unique(stringr::str_extract(CSM_sheet_names, "\\d{4}"))

missing_CSM <- (setdiff(year_range, years_CSM))

print(missing_CSM)

# Convert Timestamp and extract year and month for Cambridge Shade's Mill
CSM_combined_data <- CSM_combined_data %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Aggregate data by year and month, and get metrics
CSM_results <- CSM_combined_data %>%
  group_by(Year, Month) %>%
  summarise(Minimum = min(Value, na.rm = TRUE),
            Maximum = max(Value, na.rm = TRUE),
            Total = sum(Value, na.rm = TRUE),
            Num_Samples = n(),
            .groups = 'drop')

# Export results as csv files into the R project
write.csv(CSM_results, "./data/precipitation/CSM_summarized_precip.csv")

## Most extreme values ---------------------------------------------------------
# Identify 5 highest total precip years
yearly_total <- CSM_combined_data %>% #determine out yearly total
  group_by(Year) %>%
  summarise(Total = sum(Value, na.rm = TRUE),
            Num_Samples = n(),
            .groups = 'drop')

high5_CSM <- yearly_total %>%
  arrange(desc(Total)) %>%
  slice_head(n = 5) %>%
  select(Year, Total)

print(high5_CSM)

# Identify 5 lowest total precip years
low5_CSM <- yearly_total %>%
  arrange((Total)) %>%
  slice_head(n = 5) %>%
  select(Year, Total)

print(low5_CSM)

