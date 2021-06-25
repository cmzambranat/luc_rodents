library(tidyverse)
library(here)
library(ggplot2)
library(countrycode)
library(stars)


data_names =  c("X", "lat", "lon", "SSP1_2050", "SSP4_2050", "SSP5_2050", "cost_SSP4", "cost_SSP5", "ADMIN", 
                "ISO_A3","geom1", "geom2","HIC", "LIC", "UMC", "LMC", "economy")

data_luc = as_tibble(read.csv(here('data/DATA_SHARED_SOCIOECONOMIC_PATHS.csv'), col.names = data_names)) %>%
  select(-X, -geom1, -geom2) %>%
  mutate(group = countrycode(data_luc$ADMIN, origin = "country.name", destination = "region")
  )



luc_sf = st_as_sf(data_luc, coords = c("lon", "lat"), crs = 4326, agr = "constant")
plot(luc_sf["SSP1_2050"])
# ar5: IPCC's regional mapping used both in the Fifth Assessment Report (AR5) and for the Reference Concentration Pathways (RCP)
# continent: Continent as defined in the World Bank Development Indicators
# region: 7 Regions as defined in the World Bank Development Indicators

data_avoided = data_luc %>%
  select(cost_SSP4, cost_SSP5, SSP1_2050, group) %>%
  pivot_longer(!group, names_to = "scenario", values_to = "avoided_risk") %>%
  drop_na()

ggplot(data_avoided, aes(x = group, y = avoided_risk, fill = scenario)) + 
  geom_boxplot()


data_risk = data_luc %>%
  filter(continent == 'Americas') %>%
  select(SSP4_2050, SSP5_2050, SSP1_2050, ADMIN) %>%
  pivot_longer(!ADMIN, names_to = "scenario", values_to = "risk") %>%
  drop_na()


  

ggplot(data_risk, aes(x = ADMIN, y = risk, fill = scenario)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
