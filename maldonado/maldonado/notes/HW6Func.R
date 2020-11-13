#Brittany Maldonado, November 7, 2020, Brainstorming Function Ideas for HW6
library(lubridate)
library(tidyverse)
library(iNEXT)
library(leaflet)

#Trying to make a graph with total biomass for each species for each year
agp <- read.csv(file = "/cloud/project/maldonado/data/AGP/AGP_forR_cleaned.csv")
agp_biomass <- agp 
agp_biomass <- agp_biomass %>% filter(Species != "Other")
agp_biomass <- agp_biomass %>% filter(Species != "Axolotl Salamander")
agp_biomass <- agp_biomass %>% filter(Species != "Canadian Toad")
agp_biomass <- agp_biomass %>% filter(Species != "Specify")
agp_biomass$Date1 <- mdy(agp_biomass$Date)
agp_biomass_yr <- agp_biomass %>% mutate(Year = year(Date1))

sum <- agp_biomass_yr %>% 
  group_by(Species, Year) %>% 
  summarise(Biomass = sum(Mass, na.rm = TRUE)) 

sum_tig <- sum %>% filter(Species == "Tiger Salamander")
sum_rest <- sum %>% filter(Species != "Tiger Salamander")

ggplot(data = sum_rest, mapping = aes(x = Year, y = Biomass, color = Species)) +
  +     geom_line() 



