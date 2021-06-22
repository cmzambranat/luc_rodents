library(here)
library(ggplot2)
library(terra)
library(tmap)
library(viridis)
library(ggthemes)
library(sf)
library(ggspatial)
library(raster)
library(classInt)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

# Define projection
crs_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# Read rasters
rod_richness = raster(here('data/richness.tif'), crs = crs_proj)
ssp1 = raster(here('data/SSP1_2050_SSP1_2025 .tif'))

# Get world countries boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Species richness map

richness =      
ggplot() +
  layer_spatial(rod_richness) +
  #coord_equal() +
  geom_sf(data = world, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'turbo', na.value = NA) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 8)) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.35),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill = 'white'),
        #panel.grid.major = element_line(color = "#def3f6"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill="#def3f6"),
        legend.text=element_text(size = 17)) +
  coord_sf(xlim = st_bbox(rod_richness)[c(1, 3)],
           ylim = st_bbox(rod_richness)[c(2, 4)],
           expand = FALSE)

richness
ggsave(here('figures/rodent_richness.png'), richness, 
      device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)

# SSP1 2025 ---------------------------------------------------------------
# Read raster
ssp1_2025 = raster(here('data/SSP1_2025.tif'), crs = crs_proj)

# Percent mapping
# Source: https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#extreme-value-maps
percent <-  c(0,.01,.1,.5,.9,.99,1)
var <- values(ssp1_2025)
cI = round(quantile(var, percent, na.rm = T), 3)

# Palette 
pal = (tmaptools::get_brewer_pal("YlGnBu", n = 9, contrast = 0.7, plot = F))

luc_rod_plot(ssp1_2025, cI, pal)


ssp1_2025_map =
  ggplot() +
  layer_spatial(ssp1_2025) +
  geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
  scale_fill_viridis(option = 'cividis', na.value = NA) +
  #scale_fill_gradientn(na.value = NA, colours = pal, 
   #                    values = rescale(cI),
    #                   limits=c(0, 6.972)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 8)) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.35),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 12)) +
  coord_sf(xlim = st_bbox(ssp1_2050_ssp1_2025)[c(1, 3)],
           ylim = st_bbox(ssp1_2050_ssp1_2025)[c(2, 4)],
           expand = FALSE)

ssp1_2025_map
ggsave(here('figures/ssp1_2050_ssp1_2025_perc.png'), ssp1_2050_ssp1_2025_map, 
       device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)


## SPP1 2050 - ssp1 2025
ssp1_2050_ssp1_2025 = raster(here('data/SSP1_2050_SSP1_2025.tif'), crs = crs_proj)

# Percent mapping
# Source: https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#extreme-value-maps
percent <-  c(0,.01,.1,.5,.9,.99,1)
var <- values(ssp1_2050_ssp1_2025)
var = var[!is.na(var)]

cI = round(quantile(var, percent, na.rm = T), 3)
cI = round(boxbreaks(var, 2), 2)

cI = round(classIntervals(var, style = 'quantile', n = 5, intervalClosure = "left")$brks, 2)


cI = c(-2.32, -0.123, -0.02, 0.139, 3.40)




# Palette 
pal = rev(tmaptools::get_brewer_pal("RdBu", n = 5, contrast = 0.7, plot = F))

ssp1_2050_ssp1_2025_map =
  ggplot() +
  layer_spatial(ssp1_2050_ssp1_2025) +
  #ggplot2::geom_raster(data = rod_ras, aes_string(x = "x", y = "y", fill = "richness")) +
  #coord_equal() +
  geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
  scale_fill_gradientn(na.value = NA, colours = pal, 
                       values = rescale(cI),
                       limits=c(-2.33, 3.40)) +
  #scale_fill_manual(values = cI) +
  #scale_colour_brewer(type = 'div', palette = 'PRGn', na.value = NA, breaks = cI) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 8)) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.35),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 12)) +
  coord_sf(xlim = st_bbox(ssp1_2050_ssp1_2025)[c(1, 3)],
           ylim = st_bbox(ssp1_2050_ssp1_2025)[c(2, 4)],
           expand = FALSE)

ssp1_2050_ssp1_2025_map
ggsave(here('figures/ssp1_2050_ssp1_2025_perc.png'), ssp1_2050_ssp1_2025_map, 
       device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)




# tmap
test=
tm_shape(ssp1_2050_ssp1_2025) +
  tm_raster("SSP1_2050_SSP1_2025", palette = '-RdBu', style = 'order', midpoint = 0, as.count = F, drop.levels = T) +
  tm_layout(scale=.8, legend.position = c("left","bottom"))

test
tmap_save(test, here('figures/ssp1_2050_ssp1_2025.pdf'), dpi = 500)









