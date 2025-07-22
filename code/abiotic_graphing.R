### ABIOTIC GRAPHING

## Load packages ---------------------------------------------------------------
if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(janitor, quietly = TRUE)) {
  install.packages("janitor")
  library(janitor)}

if (!require(lubridate, quietly = TRUE)) {
  install.packages("lubridate")
  library(lubridate)}

## Import data -----------------------------------------------------------------
# PWQMN data 
ammonium <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Ammonium")  # ammonium

chloride <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Chloride") # chloride

conductivity <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Conductivity") # conductivity

dissolved_oxygen <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Dissolved Oxygen") # dissolved oxygen

nitrate <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Nitrate") # nitrate

nitrite <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Nitrite") # nitrite

ph <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "pH") # pH

phosphate <- read_excel("./data/water_quality/PWQMN_Mill_Creek_Data.xlsx", sheet = "Phosphate") # phosphate

# Most extreme years 
air_temp_summarized <- read.csv("data/air_temp/CSM_summarized_air_temp.csv") # air temperature - all years summarized

ABF_water_temp_summarized <- read.csv("data/water_temp/ABF_summarized_water_temp.csv") # Aberfoyle water temperature - all years summarized

SR10_water_temp_summarized <- read.csv("data/water_temp/SR10_summarized_water_temp.csv") # Side Road 10 water temperature - all years summarized

precip_summarized <- read.csv("data/precipitation/CSM_summarized_precip.csv") # precipitation - all years summarized

# Rangers Restoration data
ranger_activities <- read.csv("data/Ranger_restoration_activities.csv", header=TRUE) # restoration activities by type

ranger_sites <- read.csv("data/Ranger_work_sites.csv", header=TRUE) # restoration work sites


# Data prep ---------------------------------------------------------------
### PWQMN data
ammonium$Collection_Date <- as.Date(ammonium$`Collection Timestamp`)

chloride$Collection_Date <- as.Date(chloride$`Collection Timestamp`)

conductivity$Collection_Date <- as.Date(conductivity$`Collection Timestamp`)

dissolved_oxygen$Collection_Date <- as.Date(dissolved_oxygen$`Collection Timestamp`)

nitrate$Collection_Date <- as.Date(nitrate$`Collection Timestamp`)

nitrite$Collection_Date <- as.Date(nitrite$`Collection Timestamp`)

phosphate$Collection_Date <- as.Date(phosphate$`Collection Timestamp`)

ph$Collection_Date <- as.Date(ph$`Collection Timestamp`)


# Most extreme years ------------------------------------------------------
### Air temperature
air_temp_summarized$Month <- month.abb[air_temp_summarized$Month] # give months abbreviated names
air_temp_summarized$Month <- factor(air_temp_summarized$Month, levels = month.abb) # convert month to a factor

air_temp_2002 <- subset(air_temp_summarized, Year == 2002) # select 2002 data
air_temp_2005 <- subset(air_temp_summarized, Year == 2005) # select 2005 data
air_temp_2009 <- subset(air_temp_summarized, Year == 2009) # select 2009 data
air_temp_2011 <- subset(air_temp_summarized, Year == 2011) # select 2011 data
air_temp_2012 <- subset(air_temp_summarized, Year == 2012) # select 2012 data
air_temp_2014 <- subset(air_temp_summarized, Year == 2014) # select 2014 data
air_temp_2015 <- subset(air_temp_summarized, Year == 2015) # select 2015 data
air_temp_2020 <- subset(air_temp_summarized, Year == 2020) # select 2020 data

air_temp_all_years <- bind_rows(air_temp_2002, air_temp_2005, air_temp_2009, air_temp_2011, air_temp_2012, air_temp_2014, air_temp_2015, air_temp_2020)

### Water temperature - Aberfoyle station
ABF_water_temp_summarized$Month <- month.abb[ABF_water_temp_summarized$Month] # give months abbreviated names
ABF_water_temp_summarized$Month <- factor(ABF_water_temp_summarized$Month, levels = month.abb) # convert month to a factor

ABF_high_water_temp_2005 <- subset(ABF_water_temp_summarized, Year == 2005) # select 2005 data - high temp
ABF_high_water_temp_2006 <- subset(ABF_water_temp_summarized, Year == 2006) # select 2006 data - high temp
ABF_high_water_temp_2011 <- subset(ABF_water_temp_summarized, Year == 2011) # select 2011 data - high temp
ABF_high_water_temp_2016 <- subset(ABF_water_temp_summarized, Year == 2016) # select 2016 data - high temp
ABF_high_water_temp_2019 <- subset(ABF_water_temp_summarized, Year == 2019) # select 2019 data - high temp

ABF_high_water_temp_all_years <- bind_rows(ABF_high_water_temp_2005, ABF_high_water_temp_2006, ABF_high_water_temp_2011, ABF_high_water_temp_2016, ABF_high_water_temp_2019)

ABF_low_water_temp_2005 <- subset(ABF_water_temp_summarized, Year == 2005) # select 2005 data - low temp
ABF_low_water_temp_2008 <- subset(ABF_water_temp_summarized, Year == 2008) # select 2008 data - low temp
ABF_low_water_temp_2009 <- subset(ABF_water_temp_summarized, Year == 2009) # select 2009 data - low temp
ABF_low_water_temp_2014 <- subset(ABF_water_temp_summarized, Year == 2014) # select 2014 data - low temp
ABF_low_water_temp_2019 <- subset(ABF_water_temp_summarized, Year == 2019) # select 2019 data - low temp

ABF_low_water_temp_all_years <- bind_rows(ABF_low_water_temp_2005, ABF_low_water_temp_2008, ABF_low_water_temp_2009, ABF_low_water_temp_2014, ABF_low_water_temp_2019)

### Water temperature - Side Road 10 station
SR10_water_temp_summarized$Month <- month.abb[SR10_water_temp_summarized$Month] # give months abbreviated names
SR10_water_temp_summarized$Month <- factor(SR10_water_temp_summarized$Month, levels = month.abb) # convert month to a factor

SR10_high_water_temp_2001 <- subset(SR10_water_temp_summarized, Year == 2001) # select 2001 data - high temp
SR10_high_water_temp_2002 <- subset(SR10_water_temp_summarized, Year == 2002) # select 2002 data - high temp
SR10_high_water_temp_2012 <- subset(SR10_water_temp_summarized, Year == 2012) # select 2012 data - high temp
SR10_high_water_temp_2023 <- subset(SR10_water_temp_summarized, Year == 2023) # select 2023 data - high temp

SR10_high_water_temp_all_years <- bind_rows(SR10_high_water_temp_2001, SR10_high_water_temp_2002, SR10_high_water_temp_2012, SR10_high_water_temp_2023)

SR10_low_water_temp_2003 <- subset(SR10_water_temp_summarized, Year == 2003) # select 2003 data - low temp
SR10_low_water_temp_2018 <- subset(SR10_water_temp_summarized, Year == 2018) # select 2018 data - low temp
SR10_low_water_temp_2019 <- subset(SR10_water_temp_summarized, Year == 2019) # select 2019 data - low temp

SR10_low_water_temp_all_years <- bind_rows(SR10_low_water_temp_2003, SR10_low_water_temp_2018, SR10_low_water_temp_2019)

### Precipitation
precip_summarized$Month <- month.abb[precip_summarized$Month] # give months abbreviated names
precip_summarized$Month <- factor(precip_summarized$Month, levels = month.abb) # convert month to a factor

high_precip_2006 <- subset(precip_summarized, Year == 2006) # select 2006 data - high precip year
high_precip_2008 <- subset(precip_summarized, Year == 2008) # select 2008 data - high precip year
high_precip_2016 <- subset(precip_summarized, Year == 2016) # select 2016 data - high precip year
high_precip_2019 <- subset(precip_summarized, Year == 2019) # select 2019 data - high precip year
high_precip_2023 <- subset(precip_summarized, Year == 2023) # select 2023 data - high precip year

high_precip_years <- bind_rows(high_precip_2006, high_precip_2008, high_precip_2016, high_precip_2019, high_precip_2023)

low_precip_2007 <- subset(precip_summarized, Year == 2007) # select 2007 data - low precip year
low_precip_2011 <- subset(precip_summarized, Year == 2011) # select 2011 data - low precip year
low_precip_2015 <- subset(precip_summarized, Year == 2015) # select 2015 data - low precip year
low_precip_2017 <- subset(precip_summarized, Year == 2017) # select 2017 data - low precip year
low_precip_2022 <- subset(precip_summarized, Year == 2022) # select 2022 data - low precip year

low_precip_years <- bind_rows(low_precip_2007, low_precip_2011, low_precip_2015, low_precip_2017, low_precip_2022)

# Rangers Restoration data ------------------------------------------------
#Restoration methods
years_reported_activities <- ranger_activities %>%
  distinct(Year,Restoration.Work.Performed) %>%
  group_by(Restoration.Work.Performed)%>%
  summarise(count=n())

years_reported_activities$Restoration.Work.Performed <- as.factor(years_reported_activities$Restoration.Work.Performed)
years_reported_activities$Restoration.Work.Performed <- factor(years_reported_activities$Restoration.Work.Performed, levels = years_reported_activities$Restoration.Work.Performed[order(years_reported_activities$count, decreasing = TRUE)])

#Restoration sites
years_reported_sites<-ranger_sites %>%
  distinct(Year,Location) %>%
  group_by(Location)%>%
  summarise(count=n())

years_reported_sites$Location <- as.factor(years_reported_sites$Location)
years_reported_sites$Location <- factor(years_reported_sites$Location, levels = years_reported_sites$Location[order(years_reported_sites$count, decreasing = TRUE)])

# PWQMN graphs -----------------------------------------------------------------
ammonium %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Ammonium") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

chloride %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Chloride") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

conductivity %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Conductivity") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

dissolved_oxygen %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Dissolved oxygen") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

nitrate %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Nitrate") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

nitrite %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Nitrite") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

phosphate %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("Phosphate") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

ph %>% 
  ggplot(aes(Collection_Date, Result)) + 
  geom_point(aes(color = Units, shape = Method)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Date") + 
  ylab("Result") +
  ggtitle("pH") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5))

# Most extreme years graphs ----------------------------------------------------
# Define color palette
colors <- hcl.colors(3, palette = "Set2")

# Air temperature
ggplot(air_temp_all_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", nrow=4, ncol=2) +
  geom_point(aes(y = Average, color = "Average"), size = 3, shape = 18) +
  geom_point(aes(y = Minimum, color = "Minimum"), size = 1, shape = 3) +
  geom_point(aes(y = Maximum, color = "Maximum"), size = 1, shape = 0) +
  labs(x = "Month",
       y = "Monthly mean air temperature (C)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ylim(-30,40) +
  theme_minimal() + 
  theme(panel.grid.minor = element_line(color = "grey",
                                        linewidth = 0.1,
                                        linetype = 2))

# Water temperature - Aberfoyle station
ggplot(ABF_high_water_temp_all_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", nrow=3, ncol=2) +
  geom_point(aes(y = Average, color = "Average"), size = 3, shape = 18) +
  geom_point(aes(y = Minimum, color = "Minimum"), size = 1, shape = 3) +
  geom_point(aes(y = Maximum, color = "Maximum"), size = 1, shape = 0) +
  labs(x = "Month",
       y = "Water temperature (C)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Aberfoyle, high temperature years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                     linewidth = 0.1,
                                     linetype = 2))

ggplot(ABF_low_water_temp_all_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", nrow=3, ncol=2) +
  geom_point(aes(y = Average, color = "Average"), size = 3, shape = 18) +
  geom_point(aes(y = Minimum, color = "Minimum"), size = 1, shape = 3) +
  geom_point(aes(y = Maximum, color = "Maximum"), size = 1, shape = 0) +
  labs(x = "Month",
       y = "Water temperature (C)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Aberfoyle, low temperature years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                                                                linewidth = 0.1,
                                                                                linetype = 2))

# Water temperature - Side Road 10 station
ggplot(SR10_high_water_temp_all_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", nrow=3, ncol=2) +
  geom_point(aes(y = Average, color = "Average"), size = 3, shape = 18) +
  geom_point(aes(y = Minimum, color = "Minimum"), size = 1, shape = 3) +
  geom_point(aes(y = Maximum, color = "Maximum"), size = 1, shape = 0) +
  labs(x = "Month",
       y = "Water temperature (C)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Side Road 10, high temperature years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                                                                linewidth = 0.1,
                                                                                linetype = 2))

ggplot(SR10_low_water_temp_all_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", nrow=3, ncol=2) +
  geom_point(aes(y = Average, color = "Average"), size = 3, shape = 18) +
  geom_point(aes(y = Minimum, color = "Minimum"), size = 1, shape = 3) +
  geom_point(aes(y = Maximum, color = "Maximum"), size = 1, shape = 0) +
  labs(x = "Month",
       y = "Water temperature (C)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Side Road 10, low temperature years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                                                                linewidth = 0.1,
                                                                                linetype = 2))

# Precipitation - high years
ggplot(high_precip_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", ncol = 2) +
  geom_point(aes(y = Total, color = "Total"), size = 3, shape = 18) +
  labs(x = "Month",
       y = "Total precipitation (mm)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ylim(0,250) +
  ggtitle("High precipitation years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                        linewidth = 0.1,
                                        linetype = 2))

# Precipitation - low years
ggplot(low_precip_years, aes(x = Month)) +
  facet_wrap(~Year, scales = "free", ncol=2) +
  geom_point(aes(y = Total, color = "Total"), size = 3, shape = 18) +
  labs(x = "Month",
       y = "Total precipitation (mm)",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ylim(0,250) +
  ggtitle("Low precipitation years") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_line(color = "grey",
                                        linewidth = 0.1,
                                        linetype = 2))

# Rangers restoration graphs ---------------------------------------------------
palette <- hcl.colors(18, palette = "Set2") # define color palette

# Restoration methods
ggplot(years_reported_activities, aes(x = Restoration.Work.Performed, y = count, fill = Restoration.Work.Performed)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values=palette) + 
  labs(x = "Type of Restoration Work Reported",
       y = "Number of Years Reported",
       fill = "Restoration Work Reported") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Restoration sites
ggplot(years_reported_sites, aes(x = Location, y = count, fill = Location)) +
  geom_bar(stat = "identity", color="black") +
  scale_fill_manual(values = palette) +
  labs(x = "Location of Restoration Work",
       y = "Number of Years Reported",
       fill = "Restoration Work Reported") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#Restoration timeline
ranger_activities_summary <- ranger_activities %>%
  count(Year)

ggplot(ranger_activities_summary, aes(x = Year, y = n)) +
  geom_col(fill = "grey") +  
  labs(x = "Year of Restoration Work",
       y = "Number of Different Restoration Work Activities Reported") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
