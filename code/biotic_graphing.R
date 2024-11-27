### GRAPHING BIOTIC DATA

## Load packages ---------------------------------------------------------------
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require(cowplot, quietly = TRUE)) {
  install.packages("cowplot")
  library(cowplot)}

if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra")
  library(gridExtra)}

## Import data -----------------------------------------------------------------
# Biotic inventory description data
inventory <- read_excel("./data/biotic_inventory_description.xlsx")
fish <- read_excel("./data/fish_order_family.xlsx")

# STREAM data
stream <- read_excel("./data/STREAM_MILL CREEK GRCA_Taxonomic_Results_2023.xlsx", sheet = "Species")

# Trout data 
trout1 <- read_excel("data/species_scanned_docs.xlsx", sheet = "10.1_pages1-2")
trout2 <- read_excel("data/species_scanned_docs.xlsx", sheet =  "10.1_pages4-9_and_3.11")

# Human population data
census <- data.frame( #sourced from StatsCan census data
  Year = c(1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016, 2021),
  Population = c(4480, 4885, 5397, 5416, 5885, 6689, 7029, 7336, 7944),
  Dwellings = c(1385, 1520, 1745, 1895, 2028, 2396, 2619, 2793, 2946)
)

## Data prep -------------------------------------------------------------------
# Biotic inventory data prep
domains <- inventory %>%
  mutate(`# Entries` = as.numeric(`# Entries`)) %>%
  group_by(Domain) %>%
  summarise(Total = sum(`# Entries`, na.rm = TRUE)) %>%
  mutate(Domain = reorder(Domain, -Total))

eukaryote_kingdoms <- inventory %>%
  mutate(`# Entries` = as.numeric(`# Entries`)) %>%
  filter(Domain == "Eukaryota") %>%
  group_by(Kingdom) %>%
  summarise(Total = sum(`# Entries`, na.rm = TRUE)) %>%
  mutate(Kingdom = reorder(Kingdom, -Total))

animal_phyla <- inventory %>%
  mutate(`# Entries` = as.numeric(`# Entries`)) %>%
  filter(Domain == "Eukaryota", Kingdom == "Animalia") %>%
  group_by(Phylum) %>%
  summarise(Total = sum(`# Entries`, na.rm = TRUE)) %>%
  mutate(Phylum = reorder(Phylum, -Total))

vert_classes <- inventory %>%
  mutate(`# Entries` = as.numeric(`# Entries`)) %>%
  filter(Domain == "Eukaryota", Kingdom == "Animalia", Phylum == "Chordata") %>%
  group_by(Class) %>%
  summarise(Total = sum(`# Entries`, na.rm = TRUE)) %>%
  mutate(Class = reorder(Class, -Total))

invert_classes <- inventory %>%
  mutate(`# Entries` = as.numeric(`# Entries`)) %>%
  filter(Domain == "Eukaryota", Kingdom == "Animalia", Phylum != "Chordata", !is.na(Class)) %>%
  group_by(Class) %>%
  summarise(Total = sum(`# Entries`, na.rm = TRUE)) %>%
  mutate(Class = reorder(Class, -Total))

 vert_classes <- vert_classes %>% mutate(Class = as.factor(Class), Type = "Vertebrate")
 invert_classes <- invert_classes %>% mutate(Class = as.factor(Class), Type = "Invertebrate")
 combined_classes <- rbind(vert_classes, invert_classes)
 combined_classes$Class <- factor(combined_classes$Class, levels = c(levels(vert_classes$Class), "----", levels(invert_classes$Class)))

fish_orders <- fish %>%
  group_by(Order) %>%
  summarise(Total = n()) %>%
  mutate(Order = reorder(Order, -Total))

fish_families <- fish %>%
  group_by(Family) %>%
  summarise(Total = n()) %>%
  mutate(Family = reorder(Family, -Total))

# STREAM data prep
filtered_stream_data <- stream %>% #select only sites on Mill Creek
  filter(if_any(starts_with("MIL") | starts_with("MLC"), ~ !is.na(.))) %>%
  select(-starts_with("BLR"), -starts_with("CHL"), -starts_with("HWC"), -starts_with("RAN")) %>%
  mutate(across(starts_with("MIL") | starts_with("MLC"), ~ as.integer(. == "P")))
View(filtered_stream_data)

phylum_count <- filtered_stream_data %>% #count species in each phylum
  group_by(Phylum) %>%
  summarise(Species_Count = n_distinct(Species))

class_count <- filtered_stream_data %>% #count species in each class
  group_by(Class) %>%
  summarise(Species_Count = n_distinct(Species))

family_insecta_count <- filtered_stream_data %>% #subset by class Insecta and count species in each family
  filter(Class == "Insecta") %>%
  group_by(Family) %>%
  summarise(Species_Count = n_distinct(Species))

# Trout population data prep
trout1 <- trout1 %>% 
  rename("trout_per_ha" = "#s/ha")

trout2 <- trout2 %>% 
  filter(`population numbers per`!="station")

brown_trout <- trout2 %>% filter(Species == "brown") #filter data for brown trout
brook_trout <- trout2 %>% filter(Species == "brook") #filter data for brook trout

# Define color palette
palette <- hcl.colors(63, palette = "Set2")
palette2 <- hcl.colors(3, palette = "Set2")

## Biotic inventory graphs -----------------------------------------------------
# Graph Domains
ggplot(domains, aes(x = Domain, y = Total, fill = Domain)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Domains", y = "Entries (#)", fill = "Domain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Graph Eukaryotic Kingdoms
ggplot(eukaryote_kingdoms, aes(x = Kingdom, y = Total, fill = Kingdom)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Eukaryote Kingdoms", y = "Entries (#)", fill = "Kingdom") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Graph Animal Phyla
ggplot(animal_phyla, aes(x = Phylum, y = Total, fill = Phylum)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Animal Phyla", y = "Entries (#)", fill = "Phylum") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Graph Classes 
ggplot(combined_classes, aes(x = Class, y = Total, fill = Class)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Classes", y = "Entries (#)", fill = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 450)) +
  geom_vline(xintercept = which(levels(combined_classes$Class) == "Insecta") - 1.5, linetype = "dashed") +
  annotate("text", x = (which(levels(combined_classes$Class) == "Insecta") - 1.5) / 2, y = 450, label = "Vertebrate", vjust = 1.5, fontface = "italic") +
  annotate("text", x = (which(levels(combined_classes$Class) == "Insecta") + length(levels(invert_classes$Class)) / 2 - 0.75), y = 450, label = "Invertebrate", vjust = 1.5, fontface = "italic")

# Graph fish
ggplot(fish_orders, aes(x = Order, y = Total, fill = Order)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Fish Orders", y = "Entries (#)", fill = "Order") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,9), breaks = 0:9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

ggplot(fish_families, aes(x = Family, y = Total, fill = Family)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Fish Families", y = "Entries (#)", fill = "Family") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,8), breaks = 0:8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## STREAM data graphs ----------------------------------------------------------
# Graph Phyla
ggplot(phylum_count, aes(x = reorder(Phylum, -Species_Count), y = Species_Count, fill = Phylum)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Phylum", y = "Species (#)", fill = "Phylum") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Graph Classes
ggplot(class_count, aes(x = reorder(Class, -Species_Count), y = Species_Count, fill = Class)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Class", y = "Species (#)", fill = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

# Graph Families within Class Insecta
ggplot(family_insecta_count, aes(x = reorder(Family, -Species_Count), y = Species_Count, fill = Family)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = palette) +
  labs(x = "Family", y = "Species (#)", fill = "Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Trout population graphs -----------------------------------------------------
trout1 %>%
  ggplot(aes(x = Year, y = trout_per_ha, color = Station, fill = Station)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values=palette2) +
  scale_x_continuous(breaks = unique(trout1$Year)) + #label every year
  labs(x = "Year", y = "Trout/hectare (#)") +
  theme_cowplot()

brown_trout_yoy <- 
  ggplot(aes(x = Year, y = YOY, fill = Station), data = brown_trout) + 
  geom_bar(stat = "identity", color = "white") + 
  scale_fill_manual(values=palette2) +
  scale_x_continuous(breaks = unique(brown_trout$Year)) + #label every year
  labs(x = "Year", y = "Young of the year (#)") +
  ggtitle("Brown trout") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"))

brook_trout_yoy <- 
  ggplot(aes(x = Year, y = YOY, fill = Station), data = brook_trout) + 
  geom_bar(stat = "identity", color = "white") + 
  scale_fill_manual(values=palette2) +
  scale_x_continuous(breaks = unique(brook_trout$Year)) + #label every year
  labs(x = "Year", y = "Young of the year (#)") +
  ggtitle("Brook trout") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"))

grid.arrange(brown_trout_yoy, brook_trout_yoy, ncol=2) #combine brown and brook yoy plots

## Human population graph ------------------------------------------------------
ggplot(census, aes(x = Year)) +
  geom_bar(aes(y = Population, fill = "Population"), stat = "identity", position = "identity") +
  geom_bar(aes(y = Dwellings, fill = "Dwellings"), stat = "identity", position = "identity") +
  scale_fill_manual(values = c("Population" = "grey80", "Dwellings" = "grey20"), name = "Legend") +
  scale_x_continuous(breaks = census$Year, labels = census$Year) +
  scale_y_continuous(name = "Population",
                     sec.axis = sec_axis(~ ., name = "Dwellings")) +  #direct mapping for the second y-axis
  theme_minimal()

    