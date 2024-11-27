### MANN-KENDALL TESTS

## Load packages ---------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require(lubridate, quietly = TRUE)) {
  install.packages("lubridate")
  library(lubridate)}

if (!require(trend, quietly = TRUE)) {
  install.packages("trend")
  library(trend)}

if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra")
  library(gridExtra)}

if (!require(patchwork, quietly = TRUE)) {
  install.packages("patchwork")
  library(patchwork)}

## Air temperature -------------------------------------------------------------
# Cambridge Shade's Mills
# Import data
CSM_file_path_air_temp <- "./data/air_temp/CSM_raw_air_temp.xlsx" # define file path
CSM_sheet_names_air_temp <- excel_sheets(CSM_file_path_air_temp) # define sheet names
CSM_list_air_temp <- lapply(CSM_sheet_names_air_temp, function(CSM_sheet_names_air_temp) {
  read_excel(CSM_file_path_air_temp, sheet = CSM_sheet_names_air_temp)
}) # read each sheet and store in a list
CSM_combined_data_air_temp <- bind_rows(CSM_list_air_temp) # combine all data frames

View(CSM_combined_data_air_temp)

sum(is.na(CSM_combined_data_air_temp)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_CSM_air_temp <- unique(stringr::str_extract(CSM_sheet_names_air_temp, "\\d{4}"))
missing_CSM_air_temp <- (setdiff(year_range, years_CSM_air_temp))
print(missing_CSM_air_temp) # "1996" "1997" "1998" "1999" "2000" "2001"

# Convert Timestamp and extract year and month
air_temp <- CSM_combined_data_air_temp %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- air_temp %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months of data
  filter(Months < 8)

print(incomplete_years) # "2021"

clean_air_temp <- air_temp %>%
  filter(!Year %in% c(2021)) # remove incomplete years

# MK on annual mean air temp
annual_mean_air_temp <- clean_air_temp %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(annual_mean_air_temp$Mean)
sens.slope(annual_mean_air_temp$Mean)

nrow(air_temp) # sample size = # observations

# Graph annual mean air temp
annual_mean_air_temp <- rbind(annual_mean_air_temp, # add NA values for incomplete years
                              data.frame(Year = 2021, Mean = NA))

ggplot(annual_mean_air_temp, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 18) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "red3") +
  labs(x = "Year",
       y = "Annual Mean Air Temperature (C)")

# MK on July mean air temp
july_mean_air_temp <- air_temp %>%
  group_by(Year, Month) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop') %>%
  filter(Month == 7)

mk.test(july_mean_air_temp$Mean)

sens.slope(july_mean_air_temp$Mean)

july_mean_air_temp_sample_size <- air_temp %>%
  filter(Month == 7) # to calculate sample size

nrow(july_mean_air_temp_sample_size) # sample size = # observations

# MK on January mean air temp
jan_mean_air_temp <- air_temp %>%
  group_by(Year, Month) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop') %>%
  filter(Month == 1)

mk.test(jan_mean_air_temp$Mean)

sens.slope(jan_mean_air_temp$Mean)

jan_mean_air_temp_sample_size <- air_temp %>%
  filter(Month == 1) # to calculate sample size

nrow(jan_mean_air_temp_sample_size) # sample size = # observations

# Graph January mean air temp
jan_mean_air_temp <- rbind(jan_mean_air_temp, # add NA values for missing years
                           data.frame(Year = 2022, Month = 1, Mean = NA))

july_airtemp <- ggplot(july_mean_air_temp, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 18) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "red3") +
  ggtitle("July") +
  theme(plot.title = element_text(hjust = 0.5, color = "red3"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year",
       y = "July Mean Air Temperature (C)") +
  ylim(-10,25) +
  xlim(2003,2023)

jan_airtemp <- ggplot(jan_mean_air_temp, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 18) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "red3") +
  ggtitle("January") +
  theme(plot.title = element_text(hjust = 0.5, color = "red3")) +
  labs(x = "Year",
       y = "Monthly Mean Air Temperature (C)") +
  ylim(-10,30) +
  xlim(2003,2023)

grid.arrange(jan_airtemp, july_airtemp, ncol = 2) # combine plots


## Precipitation ---------------------------------------------------------------
# Cambridge Shade's Mills
# Import data
CSM_file_path_precip <- "./data/precipitation/CSM_raw_precip.xlsx" # define file path
CSM_sheet_names_precip <- excel_sheets(CSM_file_path_precip) # define sheet names
CSM_list_precip <- lapply(CSM_sheet_names_precip, function(CSM_sheet_names_precip) {
  read_excel(CSM_file_path_precip, sheet = CSM_sheet_names_precip)
}) # read each sheet and store in a list
CSM_combined_data_precip <- bind_rows(CSM_list_precip) # combine all data frames

View(CSM_combined_data_precip)

sum(is.na(CSM_combined_data_precip)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_CSM_precip <- unique(stringr::str_extract(CSM_sheet_names_precip, "\\d{4}"))
missing_CSM_precip <- (setdiff(year_range, years_CSM_precip))
print(missing_CSM_precip) # "1996" "1997" "1998" "1999"

# Convert Timestamp and extract year and month
precip <- CSM_combined_data_precip %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- precip %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months
  filter(Months < 8)

print(incomplete_years) # none

# MK on annual mean air temp
annual_total_precip <- precip %>%
  group_by(Year) %>%
  summarise(Total = sum(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(annual_total_precip$Total)
sens.slope(annual_total_precip$Total)

nrow(precip) # sample size = # observations

# Graph total annual precipitation
ggplot(annual_total_precip, aes(x = Year, y = Total)) +
  geom_point(size = 3, shape = 17) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "turquoise") +
  labs(x = "Year",
       y = "Total Annual Precipitation (mm)")

## Water temperature -----------------------------------------------------------
# Aberfoyle
# Import data
ABF_file_path_water_temp <- "./data/water_temp/ABF_raw_water_temp.xlsx" # define file path
ABF_sheet_names_water_temp <- excel_sheets(ABF_file_path_water_temp) # define sheet names
ABF_list_water_temp <- lapply(ABF_sheet_names_water_temp, function(ABF_sheet_names_water_temp) {
  read_excel(ABF_file_path_water_temp, sheet = ABF_sheet_names_water_temp)
}) # read each sheet and store in a list
ABF_combined_data_water_temp <- bind_rows(ABF_list_water_temp) # combine all data frames

View(ABF_combined_data_water_temp)

sum(is.na(ABF_combined_data_water_temp)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_ABF_water_temp <- unique(stringr::str_extract(ABF_sheet_names_water_temp, "\\d{4}"))
missing_ABF_water_temp <- (setdiff(year_range, years_ABF_water_temp))
print(missing_ABF_water_temp) # "1996" "1997" "1998" "1999" "2000" "2001" "2020" "2021"

# Convert Timestamp and extract year and month
ABF_water_temp <- ABF_combined_data_water_temp %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- ABF_water_temp %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months of data
  filter(Months < 8)

print(incomplete_years) # "2002" "2015" "2016" "2022"

clean_ABF_water_temp <- ABF_water_temp %>%
  filter(!Year %in% c(2002,2015,2016,2022)) # remove incomplete years

# MK on ABF annual mean water temp
ABF_annual_mean_water_temp <- clean_ABF_water_temp %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(ABF_annual_mean_water_temp$Mean)
sens.slope(ABF_annual_mean_water_temp$Mean)

nrow(ABF_water_temp) # sample size = # observations

# Side Road 10
# Import data
SR10_file_path_water_temp <- "./data/water_temp/SR10_raw_water_temp.xlsx" # define file path
SR10_sheet_names_water_temp <- excel_sheets(SR10_file_path_water_temp) # define sheet names
SR10_list_water_temp <- lapply(SR10_sheet_names_water_temp, function(SR10_sheet_names_water_temp) {
  read_excel(SR10_file_path_water_temp, sheet = SR10_sheet_names_water_temp)
}) # read each sheet and store in a list
SR10_combined_data_water_temp <- bind_rows(SR10_list_water_temp) # combine all data frames

View(SR10_combined_data_water_temp)

sum(is.na(SR10_combined_data_water_temp)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_SR10_water_temp <- unique(stringr::str_extract(SR10_sheet_names_water_temp, "\\d{4}"))
missing_SR10_water_temp <- (setdiff(year_range, years_SR10_water_temp))
print(missing_SR10_water_temp) # "1996" "1997" "1998" "2015" "2020" "2021"

# Convert Timestamp and extract year
SR10_water_temp <- SR10_combined_data_water_temp %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- SR10_water_temp %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months of data
  filter(Months < 8)

print(incomplete_years) # "1999" 2014" "2016" "2019" 2022"

clean_SR10_water_temp <- SR10_water_temp %>%
  filter(!Year %in% c(1999,2014,2016,2019,2022)) # remove incomplete years

# MK on SR10 annual mean water temp
SR10_annual_mean_water_temp <- clean_SR10_water_temp %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(SR10_annual_mean_water_temp$Mean)
sens.slope(SR10_annual_mean_water_temp$Mean)

nrow(SR10_water_temp) # sample size = # observations

# Graph annual mean water temp for both gauges
ABF_annual_mean_water_temp <- rbind(ABF_annual_mean_water_temp, # add NA values for missing years
                                    data.frame(Year = 2002, Mean = NA),
                                    data.frame(Year = 2015, Mean = NA),
                                    data.frame(Year = 2016, Mean = NA),
                                    data.frame(Year = 2022, Mean = NA))

ABF_annual_mean_water_temp_graph <- ggplot(ABF_annual_mean_water_temp, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 16) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "royalblue") +
  ggtitle("Aberfoyle") +
  theme(plot.title = element_text(hjust = 0.5, color = "royalblue")) +
  labs(x = "Year",
       y = "Annual Mean Water Temperature (C)") +
  ylim(6.5, 13.5) +
  xlim(2000, 2023.5)

SR10_annual_mean_water_temp <- rbind(SR10_annual_mean_water_temp, # add NA values for missing years
                                     data.frame(Year = 1999, Mean = NA),
                                     data.frame(Year = 2014, Mean = NA),
                                     data.frame(Year = 2016, Mean = NA),
                                     data.frame(Year = 2019, Mean = NA),
                                     data.frame(Year = 2022, Mean = NA))

SR10_annual_mean_water_temp_graph <- ggplot(SR10_annual_mean_water_temp, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 16) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "royalblue") +
  ggtitle("Side Road 10") +
  theme(plot.title = element_text(hjust = 0.5, color = "royalblue"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year") +
  ylim(6.5, 13.5) +
  xlim(2000, 2023.5)

grid.arrange(ABF_annual_mean_water_temp_graph, SR10_annual_mean_water_temp_graph, ncol = 2) # combine plots

# July max/min comparison with historical data
# Organize contemporary data and import historical data for Aberfoyle
ABF_jul_aug <- ABF_water_temp %>% 
  filter(Month %in% c(7, 8)) %>% # separate out July and August data
  group_by(Year) %>%
  summarise( # get max/min values
    max = max(Value),
    min = min(Value)
  )

ABF_jul_aug_historical <- data.frame( # historical data
  Year = c(1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1994),
  max = c(25.8, 24.7, 24.6, 24.7, 25.2, 25.5, 24.2, 23.9, 24.6, 22.3),
  min = c(17.8, 15.9, 16.0, 12.4, 17.7, 17.7, 14.5, 16.1, 15.1, 17.5)
)

ABF_jul_aug <- bind_rows(ABF_jul_aug, ABF_jul_aug_historical)

ABF_jul_aug <- ABF_jul_aug %>%
  arrange(Year)  # sort by Year in ascending order

View(ABF_jul_aug)

mk.test(ABF_jul_aug$min)
sens.slope(ABF_jul_aug$min)

mk.test(ABF_jul_aug$max)
sens.slope(ABF_jul_aug$max)

nrow(ABF_jul_aug) # sample size = # observations

# Organize contemporary data and import historical data for Side Road 10
SR10_jul_aug <- SR10_water_temp %>% 
  filter(Month %in% c(7, 8)) %>% # separate out july and august data
  group_by(Year) %>%
  summarise( # get max/min values
    max = max(Value),
    min = min(Value)
  )

SR10_jul_aug_historical <- data.frame(
  Year = c(1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1994),
  max = c(23.9, 22.2, 21.3, 21.8, 22.3, 24.3, 21.6, 21.7, 21.3, 18.5),
  min = c(15.3, 14.3, 13.8, 13.2, 15.0, 16.3, 13.3, 15.2, 15.4, 15.5)
)

SR10_jul_aug <- bind_rows(SR10_jul_aug, SR10_jul_aug_historical)

SR10_jul_aug <- SR10_jul_aug %>%
  arrange(Year)  # sort by Year in ascending order

View(SR10_jul_aug)

mk.test(SR10_jul_aug$min)
sens.slope(SR10_jul_aug$min)

mk.test(SR10_jul_aug$max)
sens.slope(SR10_jul_aug$max)

nrow(SR10_jul_aug) # sample size = # observations

# Graph historical and contemporary data
ABF_jul_aug_graph <- ggplot(ABF_jul_aug, aes(x = Year)) +
  geom_point(aes(y = min, color = "Minimum"), size = 3, shape = 16) +
  geom_point(aes(y = max, color = "Maximum"), size = 3, shape = 1) +
  geom_segment(aes(xend = Year, y = min, yend = max), linewidth = 0.4, color = "gray60") +
  scale_color_manual(values = c("Minimum" = "royalblue", "Maximum" = "royalblue")) +
  theme_minimal() +
  ggtitle("Aberfoyle") +
  theme(plot.title = element_text(hjust = 0.5, color = "royalblue")) +
  labs(x = "Year", y = "July-August Water Temperature (C)", color = "") +
  ylim(10, 28) +
  xlim(1983, 2023.5)

SR10_jul_aug_graph <- ggplot(SR10_jul_aug, aes(x = Year)) +
  geom_point(aes(y = min, color = "Minimum"), size = 3, shape = 16) +
  geom_point(aes(y = max, color = "Maximum"), size = 3, shape = 1) +
  geom_segment(aes(xend = Year, y = min, yend = max), linewidth = 0.4, color = "gray60") +
  scale_color_manual(values = c("Minimum" = "royalblue", "Maximum" = "royalblue")) +
  theme_minimal() +
  ggtitle("Side Road 10") +
  theme(plot.title = element_text(hjust = 0.5, color = "royalblue"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year", color = "") +
  ylim(10, 28) +
  xlim(1983, 2023.5)

# Combine the plots with a shared legend
combined_plot <- (ABF_jul_aug_graph + SR10_jul_aug_graph) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "right")
combined_plot

## Water flow ------------------------------------------------------------------
# Aberfoyle
# Import data
ABF_file_path_water_flow <- "./data/water_flow/ABF_raw_flow.xlsx" # define file path
ABF_sheet_names_water_flow <- excel_sheets(ABF_file_path_water_flow) # define sheet names
ABF_list_water_flow <- lapply(ABF_sheet_names_water_flow, function(ABF_sheet_names_water_flow) {
  read_excel(ABF_file_path_water_flow, sheet = ABF_sheet_names_water_flow)
}) # read each sheet and store in a list
ABF_combined_data_water_flow <- bind_rows(ABF_list_water_flow) # combine all data frames

View(ABF_combined_data_water_flow)

sum(is.na(ABF_combined_data_water_flow)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_ABF_water_flow <- unique(stringr::str_extract(ABF_sheet_names_water_flow, "\\d{4}"))
missing_ABF_water_flow <- (setdiff(year_range, years_ABF_water_flow))
print(missing_ABF_water_flow) # "1996" "1997" "1998" "1999" "2000" "2001" "2020" "2021"

# Convert Timestamp and extract year and month
ABF_water_flow <- ABF_combined_data_water_flow %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- ABF_water_flow %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months of data
  filter(Months < 8)

print(incomplete_years) # "2002" "2020"

clean_ABF_water_flow <- ABF_water_flow %>%
  filter(!Year %in% c(2002,2020)) # remove incomplete years

# MK on ABF annual mean water flow
ABF_annual_mean_water_flow <- clean_ABF_water_flow %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(ABF_annual_mean_water_flow$Mean)
sens.slope(ABF_annual_mean_water_flow$Mean)

nrow(ABF_water_flow) # sample size = # observations

# Side Road 10
# Import data
SR10_file_path_water_flow <- "./data/water_flow/SR10_raw_flow.xlsx" # define file path
SR10_sheet_names_water_flow <- excel_sheets(SR10_file_path_water_flow) # define sheet names
SR10_list_water_flow <- lapply(SR10_sheet_names_water_flow, function(SR10_sheet_names_water_flow) {
  read_excel(SR10_file_path_water_flow, sheet = SR10_sheet_names_water_flow)
}) # read each sheet and store in a list
SR10_combined_data_water_flow <- bind_rows(SR10_list_water_flow) # combine all data frames

View(SR10_combined_data_water_flow)

sum(is.na(SR10_combined_data_water_flow)) # check for missing values

# Identify which years are missing, from 1996-2023
year_range <- as.character(1996:2023)
years_SR10_water_flow <- unique(stringr::str_extract(SR10_sheet_names_water_flow, "\\d{4}"))
missing_SR10_water_flow <- (setdiff(year_range, years_SR10_water_flow))
print(missing_SR10_water_flow) # "1996" "1997" "1998" "2015" "2020" "2021"

# Convert Timestamp and extract year
SR10_water_flow <- SR10_combined_data_water_flow %>%
  mutate(Timestamp = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp))

# Identify and remove incomplete years
year_month_count <- SR10_water_flow %>% # group by Year and count unique months
  group_by(Year) %>%
  summarise(Months = n_distinct(Month)) %>%
  ungroup()

incomplete_years <- year_month_count %>% # identify years with less than 8 months of data
  filter(Months < 8)

print(incomplete_years) # "1999"

clean_SR10_water_flow <- SR10_water_flow %>%
  filter(!Year %in% c(1999)) # remove incomplete years

# MK on SR10 annual mean water flow
SR10_annual_mean_water_flow <- clean_SR10_water_flow %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            .groups = 'drop')

mk.test(SR10_annual_mean_water_flow$Mean)
sens.slope(SR10_annual_mean_water_flow$Mean)

nrow(SR10_water_flow) # sample size = # observations

# Graph annual mean water flow for both gauges
ABF_annual_mean_water_flow <- rbind(ABF_annual_mean_water_flow, # add NA values for missing years
                                    data.frame(Year = 2020, Mean = NA))

ABF_annual_mean_water_flow_graph <- ggplot(ABF_annual_mean_water_flow, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 1) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "darkgreen") +
  ggtitle("Aberfoyle") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkgreen")) +
  labs(x = "Year",
       y = "Annual Mean Water Flow (m^3/s)") +
  ylim(0.20,1.4) +
  xlim(2000, 2023.5)

SR10_annual_mean_water_flow_graph <- ggplot(SR10_annual_mean_water_flow, aes(x = Year, y = Mean)) +
  geom_point(size = 3, shape = 1) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "darkgreen") +
  ggtitle("Side Road 10") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkgreen"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year") +
  ylim(0.20,1.4) +
  xlim(2000, 2023.5)

grid.arrange(ABF_annual_mean_water_flow_graph, SR10_annual_mean_water_flow_graph, ncol = 2) # combine plots

## Flashiness ------------------------------------------------------------------
# Aberfoyle
# Import data
ABF_annual_flashiness <- read.csv("./data/water_flow/ABF_annual_flashiness.csv")

# Remove years with incomplete flashiness data
clean_ABF_flashiness <- ABF_annual_flashiness %>%
  filter(!Year %in% c(2002,2020)) # remove incomplete years

# MK on ABF annual mean water flashiness
mk.test(clean_ABF_flashiness$FlashinessIndex)
sens.slope(clean_ABF_flashiness$FlashinessIndex)

# Side Road 10
# Import data
SR10_annual_flashiness <- read.csv("./data/water_flow/SR10_annual_flashiness.csv")

# Remove years with incomplete flashiness data
clean_SR10_flashiness <- SR10_annual_flashiness %>%
  filter(!Year %in% c(1999))

# MK on SR10 annual mean water flashiness
mk.test(clean_SR10_flashiness$FlashinessIndex)
sens.slope(clean_SR10_flashiness$FlashinessIndex)

# Graph annual mean water flashiness for both gauges
ABF_flashiness <- rbind(clean_ABF_flashiness, # add NA values for missing years
                        data.frame(Year = 2020, X = NA, SumAbsChanges = NA, TotalFlow = NA, FlashinessIndex = NA))

ABF_flashiness_graph <- ggplot(ABF_flashiness, aes(x = Year, y = FlashinessIndex)) +
  geom_point(size = 3, shape = 15) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "coral") +
  ggtitle("Aberfoyle") +
  theme(plot.title = element_text(hjust = 0.5, color = "coral")) +
  labs(x = "Year",
       y = "Annual Flashiness") +
  ylim(0.125,0.275) +
  xlim(2000, 2023.5)

SR10_flashiness_graph <- ggplot(clean_SR10_flashiness, aes(x = Year, y = FlashinessIndex)) +
  geom_point(size = 3, shape = 15) +
  geom_line() +
  theme_minimal() +
  stat_smooth(method="lm", color = "coral") +
  ggtitle("Side Road 10") +
  theme(plot.title = element_text(hjust = 0.5, color = "coral"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year") +
  ylim(0.125,0.275) +
  xlim(2000, 2023.5)

grid.arrange(ABF_flashiness_graph, SR10_flashiness_graph, ncol = 2) # combine plots


## Surface water quality -------------------------------------------------------
# Side Road 10 Provincial Water Quality Monitoring (PWQMN) data 
wq_file_path <- "./data/water_quality/PWQMN_Mill_Creek_Data.xlsx"

# Chloride
chloride_raw <- read_excel(wq_file_path, sheet = "Chloride") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

chloride_raw <- chloride_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

chloride <- subset(chloride_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(chloride$Mean)
sens.slope(chloride$Mean)

nrow(chloride_raw) #sample size = # observations

# Nitrite
nitrite_raw <- read_excel(wq_file_path, sheet = "Nitrite") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

nitrite_raw <- nitrite_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

nitrite <- subset(nitrite_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(nitrite$Mean)
sens.slope(nitrite$Mean)

nrow(nitrite_raw) #sample size = # observations

# Nitrate
nitrate_raw <- read_excel(wq_file_path, sheet = "Nitrate") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

nitrate_raw <- nitrate_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

nitrate <- subset(nitrate_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(nitrate$Mean)
sens.slope(nitrate$Mean)

nrow(nitrate_raw) #sample size = # observations

# Ammonium
ammonium_raw <- read_excel(wq_file_path, sheet = "Ammonium") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

ammonium_raw <- ammonium_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

ammonium <- subset(ammonium_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(ammonium$Mean)
sens.slope(ammonium$Mean)

nrow(ammonium_raw) #sample size = # observations

# Phosphate
phosphate_raw <- read_excel(wq_file_path, sheet = "Phosphate") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

phosphate_raw <- phosphate_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

phosphate <- subset(phosphate_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(phosphate$Mean)
sens.slope(phosphate$Mean)

nrow(phosphate_raw) #sample size = # observations

# Dissolved oxygen (DO)
do_raw <- read_excel(wq_file_path, sheet = "Dissolved Oxygen") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

do_raw <- do_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

do <- subset(do_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(do$Mean)
sens.slope(do$Mean)

nrow(do_raw) #sample size = # observations

# Conductivity
conductivity_raw <- read_excel(wq_file_path, sheet = "Conductivity") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

conductivity_raw <- conductivity_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

conductivity <- subset(conductivity_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

mk.test(conductivity$Mean)
sens.slope(conductivity$Mean)

nrow(conductivity_raw) #sample size = # observations

# pH
ph_raw <- read_excel(wq_file_path, sheet = "pH") %>%
  select(`Collection Timestamp`, Result, Units) %>%
  rename(Collection_Timestamp = `Collection Timestamp`)

ph_raw <- ph_raw %>%
  mutate(Collection_Timestamp = ymd_hms(Collection_Timestamp),
         Year = year(Collection_Timestamp)) 

sum(ph_raw$Result > 8.5)
mean(ph_raw$Result)

ph <- subset(ph_raw, select = c(Year, Result)) %>%
  group_by(Year) %>%
  summarise(Mean = mean(Result, na.rm = TRUE),
            .groups = 'drop')

sum(ph$Mean > 8.5)

mk.test(ph$Mean)
sens.slope(ph$Mean)

nrow(ph_raw) #sample size = # observations
