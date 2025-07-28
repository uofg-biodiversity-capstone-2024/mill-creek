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

ranger_full <- read_excel("data/Ranger_mill_creek_restoration_raw_data.xlsx") # restoration work year, activity, location

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
palette <- hcl.colors(22, palette = "Set2") # define color palette

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
all_years <- data.frame(Year = min(ranger_activities$Year):max(ranger_activities$Year)) #Making a list of all years

ranger_activities_summary <- ranger_activities %>%
  count(Year) %>%
  right_join(all_years, by = "Year") %>%
  mutate(n = ifelse(is.na(n), 0, n)) #Making sure years with 0 observations are included in df as 0, not NA

ggplot(ranger_activities_summary, aes(x = factor(Year), y = n, fill = factor(Year))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(
    x = "Year of Restoration Work",
    y = "Number of Different Restoration Work Activities Reported") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")


# Heat map timeline graph -------------------------------------------------
###Water quality parameters heat map
#Making a df with all water quality params
all_chems <- bind_rows(ammonium, chloride, nitrate, nitrite, phosphate, dissolved_oxygen, conductivity, ph) %>%     #Making a df with all water quality parameters
  clean_names() %>% 
  select(analyte, collection_date_2) %>%
  mutate(analyte = recode(analyte, "AMMONIUM, TOTAL   UNFIL.REAC" = "Ammonium")) %>% 
  mutate(analyte = recode(analyte, "NITRITE,  UNFILTERED REACTIVE" = "Nitrite")) %>% 
  mutate(analyte = recode(analyte, "PHOSPHATE,FILTERED REACTIVE" = "Phosphate")) %>% 
  mutate(analyte = recode(analyte, "DISSOLVED OXYGEN" = "Dissolved oxygen")) %>% 
  mutate(analyte = recode(analyte, "PH FIELD" = "pH")) %>% 
  mutate(analyte = recode(analyte, "PH (-LOG H+ CONCN)" = "pH")) %>%    
  filter(analyte != "NITRATES TOTAL,   UNFIL.REAC") %>% 
  mutate(analyte = recode(analyte, "CONDUCTIVITY, AMBIENT" = "Conductivity")) %>%
  mutate(analyte = recode(analyte, "CONDUCTIVITY, 25C" = "Conductivity")) %>%
  mutate(analyte = recode(analyte, "CHLORIDE,         UNFIL.REAC" = "Chloride")) #Cleaning up df

#Making a df for graphing
all_chems_summary <- all_chems %>%
  mutate(year = year(collection_date_2)) %>%  #Extracting year 
  group_by(year, analyte) %>%                 #Grouping by year and analyte
  summarise(n = n(), .groups = "drop") %>%    #Counting number of rows = observations in each group, then ungroup
  mutate(n_factor = factor(n))                #Making list for palette

#Palette 
palette <- hcl.colors(length(unique(all_chems_summary$n_factor)), palette = "Set2") #define color palette

#Graph
ggplot(all_chems_summary, aes(x = factor(year), y = analyte, fill = n_factor)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = palette, name = "Count") +
  labs(
    x = "Year",
    y = "Analyte",
    title = "Number of Collection Times per Analyte per Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))

###Other abiotics heat map
air_temp_summarized <- air_temp_summarized %>% 
  mutate(Name = "Air temperature")  #adding an id column to every df before binding rows
ABF_water_temp_summarized <- ABF_water_temp_summarized %>% 
  mutate(Name = "Aberfoyle water temperature")  
SR10_water_temp_summarized <- SR10_water_temp_summarized %>% 
  mutate(Name = "Side Road 10 water temperature")
precip_summarized <- precip_summarized %>% 
  mutate(Name = "Precipitation")

#Making a df with all abiotics
all_abiotics <- bind_rows(air_temp_summarized, ABF_water_temp_summarized, SR10_water_temp_summarized, precip_summarized) %>% 
  clean_names() %>% 
  select(year, num_samples, name) 

#Making a df for graphing
all_abiotics_summary <- all_abiotics %>%
  group_by(year, name) %>%                    #Grouping by year and analyte
  summarise(num_samples = sum(num_samples, na.rm = TRUE), .groups = "drop") %>%  #Adding number of samples in each month to get total number in year
  mutate(n_factor = factor(num_samples))      #Making list for palette

all_abiotics_summary <- all_abiotics_summary %>% 
  na.omit() #Air temp has NAs (?!)

#This section used to work, now messes up graph and gives NAs - to investigate later
# #Leveling the names (of params) for the graph later
# custom_order <- c("Air temperature", "Precipitation", "Aberfoyle water temperature", "Side Road 10 water temperature") #custom order
# all_abiotics_summary$name <- factor(all_abiotics_summary$name, 
#                                     levels = rev(levels(all_abiotics_summary$name))) #reverse the levels so first custom name is at the top of the y-axis

#Palette (skipping colours to avoid similairty in hue between low and high values)
palette <- hcl.colors(15, palette = "Set2")[4:15]  # Skip first 3 (pinkish) colors

#Graph
ggplot(all_abiotics_summary, aes(x = factor(year), y = name, fill = num_samples)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = palette, name = "Number of Samples") +
  labs(
    x = "Year",
    y = "Name",
    title = "Number of Collection Times per Parameter per Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

##Heat map of Ranger activities (type, location)
View(ranger_full)

#do names of restoration work match what is in the ranger_activities?
intersect(ranger_full$`restoration work done`, ranger_activities$Restoration.Work.Performed)
#"Garbage removal"
unique(ranger_activities$Restoration.Work.Performed)
unique(ranger_full$`restoration work done`)

#assigning categories from ranger_activities to tasks in restoration_full
ranger_full <- ranger_full %>% 
  clean_names()
ranger_full_with_categories <- ranger_full %>% 
  mutate(category = case_when(
    restoration_work_done == "pruning, gardening, trail cleaning" ~ "Trail/Parking maintenance",
    restoration_work_done == "Trail grooming and widening" ~ "Trail/Parking maintenance",
    restoration_work_done == "Blinders added to pheasents" ~ "Other",
    restoration_work_done == "Garbage removal" ~ "Garbage removal",
    restoration_work_done == "trail construction" ~ "Trail/Parking maintenance",
    restoration_work_done == "Fixed rehabilitation structure" ~ "Repairs to restoration structures",
    restoration_work_done == "Brush bundles" ~ "Suitable fish habitat",
    restoration_work_done == "Sweeper addition" ~ "Deflector/Sweeper construction or installation",
    restoration_work_done == "Deflector and sweeper addition" ~ "Deflector/Sweeper construction or installation",
    restoration_work_done == "added brush bundles and rocks" ~ "Suitable fish habitat",
    restoration_work_done == "Dam Removal" ~ "Obstruction removal",
    restoration_work_done == "Temperature recording" ~ "Abiotic data collection",
    restoration_work_done == "Sediment Sampling" ~ "Abiotic data collection",
    restoration_work_done == "Litter removal" ~ "Vegetation management",
    restoration_work_done == "Deflector installaton" ~ "Deflector/Sweeper construction or installation",
    restoration_work_done == "Seedling planting" ~ "Planting",
    restoration_work_done == "Trimming trees" ~ "Vegetation management",
    restoration_work_done == "Trail maintenance" ~ "Trail/Parking maintenance",
    restoration_work_done == "Gate installation for parking" ~ "Trail/Parking maintenance",
    restoration_work_done == "Weed removal" ~ "Invasive species management",
    restoration_work_done == "fence repair/maintenance" ~ "Trail/Parking maintenance",
    restoration_work_done == "removed log jams, brush bundles/deflectors" ~ "Obstruction removal",
    restoration_work_done == "Cut large swath" ~ "Vegetation management",
    restoration_work_done == "garbage removal" ~ "Garbage removal",
   restoration_work_done == "garbage and log jam removal" ~ "Garbage removal",
   restoration_work_done == "Benthic Invertebrate collection" ~ "Biotic data collection",
   restoration_work_done == "electrofishing" ~ "Biotic data collection",
   restoration_work_done == "trail repair" ~ "Trail/Parking maintenance",
   restoration_work_done == "repairing old rehabilitation structures" ~ "Repairs to restoration structures",
   restoration_work_done == "removing overhang and debris" ~ "Obstruction removal",
   restoration_work_done == "parking area created" ~ "Trail/Parking maintenance",
   restoration_work_done == "beaver dam removal" ~ "Suitable fish habitat",
   restoration_work_done == "brush bundles" ~ "Suitable fish habitat",
   restoration_work_done == "creation of by-pass channel" ~ "Suitable fish habitat",
   restoration_work_done == "removed overhang/vegetation" ~ "Obstruction removal",
   restoration_work_done == "removing trees abd debris" ~ "Obstruction removal",
   restoration_work_done == "seedling planting" ~ "Planting",
   restoration_work_done == "more defined parking areas" ~ "Trail/Parking maintenance",
   restoration_work_done == "shrub removal" ~ "Vegetation management",
   restoration_work_done == "removed fallen trees" ~ "Vegetation management",
   restoration_work_done == "clear parking area" ~ "Trail/Parking maintenance",
   restoration_work_done == "in stream barrier removal" ~ "Suitable fish habitat",
   restoration_work_done == "seed planting" ~ "Planting",
   restoration_work_done == "electrofihing" ~ "Biotic data collection",
   restoration_work_done == "debirs clearing, flow redirection" ~ "Suitable fish habitat",
   restoration_work_done == "clearing weeds in parking lot" ~ "Vegetation management",
   restoration_work_done == "parking area maintenance" ~ "Trail/Parking maintenance",
   restoration_work_done == "bridge removal" ~ "Other",
   restoration_work_done == "removed logs and debris" ~ "Obstruction removal",
   restoration_work_done == "garbage cleanup" ~ "Garbage removal",
   restoration_work_done == "remove debris and overhang" ~ "Obstruction removal",
   restoration_work_done == "debris removal, prepared new section" ~ "Obstruction removal",
   restoration_work_done == "silt curtain installation" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "mark recapture fish mnitoring" ~ "Biotic data collection",
   restoration_work_done == "debirs removal and log jam removal" ~ "Vegetation management",
   restoration_work_done == "blockage remival" ~ "Obstruction removal",
   restoration_work_done == "tree planting" ~ "Planting",
   restoration_work_done == "removing trees, new trail created" ~ "Trail/Parking maintenance",
   restoration_work_done == "Brush bundles/deflectors" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "removal of debris" ~ "Obstruction removal",
   restoration_work_done == "improve ground water drain" ~ "Other",
   restoration_work_done == "create deflectors" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "made more defined channel segment" ~ "Suitable fish habitat",
   restoration_work_done == "log jam and overhead removal" ~ "Obstruction removal",
   restoration_work_done == "debris removal" ~ "Obstruction removal",
   restoration_work_done == "stream rehabilitation" ~ "Suitable fish habitat",
   restoration_work_done == "removing debris add deflectors" ~ "Obstruction removal",
   restoration_work_done == "remove overhang" ~ "Obstruction removal",
   restoration_work_done == "removing debris add deflectors" ~ "Obstruction removal",
   restoration_work_done == "fill in eroded banks" ~ "Suitable fish habitat",
   restoration_work_done == "removal of debris" ~ "Obstruction removal",
   restoration_work_done == "narrow channel" ~ "Suitable fish habitat",
   restoration_work_done == "create brook trout spawning habitat" ~ "Suitable fish habitat",
   restoration_work_done == "garbage and weed removal" ~ "Garbage removal",
   restoration_work_done == "cover structures for fish" ~ "Suitable fish habitat",
   restoration_work_done == "water quality analysis" ~ "Other",
   restoration_work_done == "removal of debris" ~ "Obstruction removal",
   restoration_work_done == "debris removal and narrow channel" ~ "Obstruction removal",
   restoration_work_done == "remove log jams" ~ "Obstruction removal",
   restoration_work_done == "modifying exisiting deflector" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "new deflector" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "moved rocks to shift the thalweg" ~ "Suitable fish habitat",
   restoration_work_done == "removed debris, cleared obstructions" ~ "Obstruction removal",
   restoration_work_done == "goose relocation" ~ "Other",
   restoration_work_done == "carp rescue" ~ "Other",
   restoration_work_done == "trail mulching" ~ "Trail/Parking maintenance",
   restoration_work_done == "removed log jams, made brush bundles" ~ "Obstruction removal",
   restoration_work_done == "improve location of woody debirs" ~ "Obstruction removal",
   restoration_work_done == "removed old T bars" ~ "Obstruction removal",
   restoration_work_done == "Sweeper/deflector addition" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "modified deflectors" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "log jam removal" ~ "Obstruction removal",
   restoration_work_done == "sweeper adjustment" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "moving woody debris" ~ "Obstruction removal",
   restoration_work_done == "removal of debris jams" ~ "Obstruction removal",
   restoration_work_done == "debris and brush removal" ~ "Obstruction removal",
   restoration_work_done == "tree removal" ~ "Obstruction removal",
   restoration_work_done == "invasice buckthorn removal" ~ "Invasive species management",
   restoration_work_done == "tree roval blocking flow" ~ "Obstruction removal",
   restoration_work_done == "temperature monitoring" ~ "Abiotic data collection",
   restoration_work_done == "removed blockages" ~ "Obstruction removal",
   restoration_work_done == "sweepers/deflectors" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "remove overhang, move blocakges" ~ "Obstruction removal",
   restoration_work_done == "clearing surrounding fences" ~ "Other",
   restoration_work_done == "trail cleanup" ~ "Trail/Parking maintenance",
   restoration_work_done == "removed debris jams" ~ "Obstruction removal",
   restoration_work_done == "deflector" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "trail maintance" ~ "Trail/Parking maintenance",
   restoration_work_done == "moved rocks" ~ "Suitable fish habitat",
   restoration_work_done == "clear blockages and overgrown vegetation" ~ "Obstruction removal",
   restoration_work_done == "remove woody debris" ~ "Obstruction removal",
   restoration_work_done == "deflectors" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "fish structure construction" ~ "Suitable fish habitat",
   restoration_work_done == "inaturalist observations" ~ "Biotic data collection",
   restoration_work_done == "deflector" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "log jams" ~ "Obstruction removal",
   restoration_work_done == "Invasive species control" ~ "Invasive species management",
   restoration_work_done == "structure update" ~ "Repairs to restoration structures",
   restoration_work_done == "STREAM sampling" ~ "Abiotic data collection",
   restoration_work_done == "woody debris removal" ~ "Obstruction removal",
   restoration_work_done == "trail maintance" ~ "Trail/Parking maintenance",
   restoration_work_done == "Crossing maintenance" ~ "Trail/Parking maintenance",
   restoration_work_done == "weed removal" ~ "Vegetation management",
   restoration_work_done == "deflector installation" ~ "Deflector/Sweeper construction or installation",
   restoration_work_done == "water level measurements" ~ "Abiotic data collection"
  ))
#Some entries have two separate actvities, SU split them up in the excel file that's on Git (ET has og file in One Drive)
#The rows changed were:
# deflector and trail repair (2004) --> deflector, trail repair
# Crossing maintenance and weed removal (2004) --> crossing, weed removal
# deflector installation, water level measurements (2006) --> deflector, water level measurements

ranger_full_with_categories %>% 
  filter(if_any(everything(), is.na)) %>% 
  distinct() %>% 
  View()
ranger_full_with_categories %>% 
  distinct(location) %>% 
  View()

ranger_full_with_categories <- ranger_full_with_categories %>% 
  mutate(location = if_else(location == "mcKinnon Property", "McKinnon Property", location)) %>% 
  mutate(location = if_else(location == "bond Tract", "Bond Tract", location)) %>% 
  mutate(location = if_else(location == "Bond tract", "Bond Tract", location)) %>% 
  mutate(location = if_else(location == "bond tract", "Bond Tract", location)) %>% 
  mutate(location = if_else(location == "S. Paddocks Property", "Paddock Property", location)) %>% 
  mutate(location = if_else(location == "soper park", "Soper Park", location)) %>% 
  mutate(location = if_else(location == "capital paving pond", "Capital Paving Pond", location)) %>% 
  mutate(location = if_else(location == "sopher park", "Soper Park", location)) %>% 
  mutate(location = if_else(location == "various locations", "Various locations", location)) %>% 
  mutate(location = if_else(location == "various locations", "Various locations", location)) %>% 
  mutate(location = if_else(location == "university of guelph property", "University of Guelph Property", location)) %>% 
  mutate(location = if_else(location == "mill creek", "Various locations", location)) %>% 
  mutate(location = if_else(location == "across mill creek", "Various locations", location)) %>% 
  mutate(location = if_else(location == "throughout mill creek", "Various locations", location)) %>% 
  mutate(location = if_else(location == "Mill Creek", "Various locations", location)) %>% 
  mutate(location = if_else(location == "pond tributary", "Pond tributary", location)) %>% 
  mutate(location = if_else(location == "mccrimmon creek", "McCrimmon creek", location)) %>% 
  mutate(location = if_else(location == "marden creek", "Marden creek", location)) %>% 
  mutate(location = if_else(location == "kerr street bridge", "Kerr Street bridge", location)) %>% 
  mutate(location = if_else(location == "heritage lake", "Heritage Lake", location)) %>% 
  mutate(location = if_else(location == "foti property", "Foti property", location)) 
  
  

View(ranger_full_with_categories)

# ranger_final <- ranger_full_with_categories %>% 
#   select(year, location, category) %>%    #select columns
#   mutate(presence = 1) %>%
#   distinct(year, location, category, presence) %>%
#   complete(year, location, category, fill = list(presence = 0))

observed_ranger_activities <- ranger_full_with_categories %>%
  select(year, location, category) %>%
  distinct() %>%
  mutate(presence = 1)

full_grid <- expand_grid(
  year = unique(ranger_full_with_categories$year),
  location = unique(ranger_full_with_categories$location),
  category = unique(ranger_full_with_categories$category))

ranger_final <- full_grid %>%
  left_join(observed_ranger_activities, by = c("year", "location", "category")) %>%
  mutate(presence = if_else(is.na(presence), 0, presence))

View(ranger_final)

ggplot(ranger_final, aes(x = factor(year), y = location, fill = factor(presence))) +
  geom_tile(color = "black", linewidth = 0.3) +  # black borders between tiles
  scale_fill_manual(
    values = c("0" = "white", "1" = "steelblue"),
    name = "Presence",
    labels = c("Absent", "Present")
  ) +
  labs(
    x = "Year",
    y = "Location",
    title = "Presence/Absence Heatmap by Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()  # remove background grid
  )
ggplot(ranger_final %>% filter(presence == 1), 
       aes(x = factor(year), y = location, fill = category)) +
  geom_tile(color = "black") +
  labs(x = "Year", y = "Location", title = "Heatmap of Categories Present") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(ranger_final, aes(x = factor(year), y = location, fill = ifelse(presence == 1, category, NA))) +
  geom_tile(color = "black") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(length(unique(ranger_final$category)), "Set3"),
    na.value = "white"
  ) +
  labs(x = "Year", y = "Location", title = "Presence Heatmap Colored by Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ranger_final_summary <- ranger_final %>%
  filter(presence == 1) %>%
  count(year, location)  # count how many categories per cell

ggplot(ranger_final_summary, aes(x = factor(year), y = location, fill = n)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "darkred", name = "# Categories") +
  labs(x = "Year", y = "Location", title = "Number of Categories Present per Year × Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Assign a position within each cell for each category
library(dplyr)
library(ggplot2)

# Create numeric x/y for plotting
ranger_split <- ranger_final %>%
  filter(presence == 1) %>%
  mutate(
    year_f = factor(year),
    location_f = factor(location),
    x = as.numeric(year_f),
    y = as.numeric(location_f)
  ) %>%
  group_by(year, location) %>%
  mutate(
    cat_id = row_number(),  # used for offsetting the tiles
    total = n()
  ) %>%
  ungroup()

# Plot: small offset tiles within each year × location block
ggplot(ranger_split, aes()) +
  geom_tile(
    aes(
      x = x + (cat_id - 1) / 10,  # horizontal offset per category
      y = y,
      fill = category
    ),
    width = 0.09, height = 0.9, color = "black"
  ) +
  scale_x_continuous(
    breaks = sort(unique(ranger_split$x)),
    labels = levels(ranger_split$year_f)
  ) +
  scale_y_continuous(
    breaks = sort(unique(ranger_split$y)),
    labels = levels(ranger_split$location_f)
  ) +
  labs(x = "Year", y = "Location", title = "Multi-Category Heatmap with Split Tiles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
