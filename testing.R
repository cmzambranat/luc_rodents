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




richness
ggsave(here('figures/rodent_richness.png'), richness, 
       device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)

ssp1_2050 = raster(here('data/SSP1_2050.tif'), crs = crs_proj)
ssp4_2050 = raster(here('data/SSP4_2050.tif'), crs = crs_proj)
ssp5_2050 = raster(here('data/SSP5_2050.tif'), crs = crs_proj)

plot(ssp1_2050 - ssp4_2050)


cI = round(boxbreaks(var, 2), 2)

var <- values(ssp1_2025)
cI_ssp1_2025 = round(quantile(var, percent, na.rm = T, names = F), 3)
cI = round(classIntervals(var, style = 'quantile', n = 7, intervalClosure = "left")$brks, 2)


# Palette: tmaptools::palette_explorer()
pal = (tmaptools::get_brewer_pal("PuBuGn", n = 10, contrast = 1, plot = F))
pal = viridisLite::magma(10, begin = 0.3, end = 1, direction = -1)

ssp1_2025_map = luc_rod_plot(ssp1_2025, pal = pal, cI = cI, lab = lab)
ssp1_2025_map

var_ssp1_2050 <- values(ssp1_2050)
cI_ssp1_2050 = round(quantile(var_ssp1_2050, percent, na.rm = T), 3)
ssp1_2050_map = luc_rod_plot(ssp1_2050, pal = pal, cI = cI_ssp1_2050)

var_ssp4_2050 <- values(ssp4_2050)
cI_ssp4_2050 = round(quantile(var_ssp4_2050, percent, na.rm = T), 3)
ssp4_2050_map = luc_rod_plot(ssp4_2050, pal = pal, cI = cI_ssp4_2050)

var_ssp5_2050 <- values(ssp5_2050)
cI_ssp5_2050 = round(quantile(var_ssp5_2050, percent, na.rm = T), 3)
ssp5_2050_map = luc_rod_plot(ssp5_2050, pal = pal, cI = cI_ssp5_2050)


patchwork <- ssp1_2025_map + ssp1_2050_map + ssp4_2050_map + ssp5_2050_map

a =patchwork + plot_annotation(tag_levels = 'A')  



+ plot_layout(guides = 'collect')



ggsave(here('figures/fig_1_v2.png'), a, 
       device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)
