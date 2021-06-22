library(diffeR)
categoryComponentsPlot(ssp1_2050, ssp1_2025, breaks = cI)
categorySourcesPlot(ssp1_2025, ssp1_2050)
MAD(ssp1_2025, ssp1_2050)
MADscatterplot(ssp1_2025, ssp1_2050)
MADscatterplot(ssp1_2025, ssp4_2050)
overallComponentsPlot(ssp1_2025, ssp4_2050)
overallSourcesPlot(ssp1_2025, ssp4_2050, breaks = cI)

differenceMR(ssp1_2025, ssp1_2050, eval = 'original')




library(tidyverse)
ssp1_2025 = raster(here('data/SSP1_2025.tif'))
var_ssp1_2025 = as_tibble(values(ssp1_2025)/max(values(ssp1_2025), na.rm = T))

ssp1_2050 = raster(here('data/SSP1_2050.tif'))
var_ssp1_2050 = as_tibble(values(ssp1_2050)/max(values(ssp1_2050), na.rm = T))

ssp4_2050 = raster(here('data/SSP4_2050.tif'))
var_ssp4_2050 = as_tibble(values(ssp4_2050))

ssp5_2050 = raster(here('data/SSP5_2050.tif'))
var_ssp5_2050 = as_tibble(values(ssp5_2050))


ssp1_2050_ssp1_2025

rest = ssp4_2050 - ssp1_2025
var4 = as_tibble(values(rest))

ggplot() + 
  geom_density(data = var_ssp1_2025, aes(x = value), lwd = 3) +
  geom_density(data = var_ssp1_2050, aes(x = value), col = 'green') +
  geom_density(data = var_ssp4_2050, aes(x = value), col = 'red') +
  geom_density(data = var_ssp5_2050, aes(x = value), col = 'cyan')

test = ssp1_2050 - ssp1_2025
test_var = as_tibble(values(test))
ggplot(test_var, aes(x = value)) + 
  geom_histogram()

ggplot() + 
  layer_spatial(test) +
  scale_colour_brewer(type = 'div', palette = 'PRGn', na.value = NA)
#ggplot2::geom_raster(data = rod_ras, aes_string(x = "x", y = "y", fill = "richness")) +
#coord_equal() +
#geom_sf(data = cnh, color = "white", fill = NA, size = 0.3) +
scale_fill_viridis_c(na.value = NA, breaks = cI) 
ggplot() +  
  geom_boxplot(data = var, aes(y = value)) +
  geom_boxplot(data = var2, aes(y = value), col = 'red')
