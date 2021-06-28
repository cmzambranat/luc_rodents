library(here)
library(ggplot2)
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
library(patchwork)

# Define projection
crs_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# Read rasters
rod_richness = raster(here('data/richness.tif'), crs = crs_proj)
ssp1 = raster(here('data/SSP1_2050_SSP1_2025 .tif'))

# Get world countries boundaries
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != 'Antarctica')


# Figure 2 ----------------------------------------------------------------
# Map rodent species richness
richness =      
ggplot() +
  layer_spatial(rod_richness) +
  #coord_equal() +
  geom_sf(data = world, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'turbo', na.value = NA) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.35),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill="#def3f6"),
        legend.text=element_text(size = 12)) +
  coord_sf(xlim = st_bbox(rod_richness)[c(1, 3)],
           ylim = st_bbox(rod_richness)[c(2, 4)],
           expand = FALSE)

# Hazard map SSP1 2025
# Read raster
ssp1_2025 = raster(here('data/SSP1_2025.tif'), crs = crs_proj)

# Estimate % breaks (Percent mapping)
# Source: https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#extreme-value-maps
percent <-  c(0,.01,.1,.5,.9,.99,1)
var_ssp1_2025 <- values(ssp1_2025)
cI_ssp1_2025 = round(quantile(var_ssp1_2025, percent, na.rm = T), 3)

lab = c('< 1%', '1% - 10%', '10% - 50%', '50% - 90%', '90% - 99%', '> 99%', 'test')

ssp1_2025_map =
  ggplot() +
  layer_spatial(ssp1_2025) +
  geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
  scale_fill_viridis(option = 'inferno', 
                     na.value = NA, 
                     direction = -1, 
                     breaks = cI_ssp1_2025, 
                     guide = 'legend', 
                     labels = lab) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.35),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 12)) +
  coord_sf(xlim = st_bbox(ssp1_2025)[c(1, 3)],
           ylim = st_bbox(ssp1_2025)[c(2, 4)],
           expand = FALSE)

# Save patchwork object
fig_1 = (richness / ssp1_2025_map) + plot_annotation(tag_levels = 'A') 

# Save on disk. Assumes a width of two columns or 12 cm when printed
ggsave(here('figures/fig_1.pdf'), fig_1, 
       device = 'pdf', width = 12, height = 10, dpi = 300, units = "cm", scale = 2)

# Figure 3 ----------------------------------------------------------------
# Contrasting baseline (2025 SSP1) vs different future scenarios (2050 SSP1, SSP5, SSP4)
## SPP1 2050 - SSP1 2025
ssp1_2050_ssp1_2025 = raster(here('data/SSP1_2050_SSP1_2025.tif'), crs = crs_proj)
ssp4_2050_ssp1_2025 = raster(here('data/SSP4_2050_SSP1_2025.tif'), crs = crs_proj)
ssp5_2050_ssp1_2025 = raster(here('data/SSP5_2050_SSP1_2025.tif'), crs = crs_proj)

# Percent mapping
# Source: https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#extreme-value-maps
percent <-  c(0,.01,.1,.5,.9,.99,1)
var_ssp4_2050_ssp1_2025 <- values(ssp4_2050_ssp1_2025)
var_ssp4_2050_ssp1_2025 = var_ssp4_2050_ssp1_2025[!is.na(var_ssp4_2050_ssp1_2025)]

# Breaks
cI_ssp4_2050_ssp1_2025 = round(quantile(var_ssp4_2050_ssp1_2025, percent, na.rm = T), 3)

# Labels
lab2 = c('< 1%', '1% - 10%', '10% - 50%', '50% - 90%', '90% - 99%', '> 99%')

# Palette 
#pal = rev(tmaptools::get_brewer_pal("RdBu", n = 5, contrast = 0.7, plot = F))
#pal = diverging_hcl(n = 7, h = c(255, 12), c = c(50, 80), l = c(20, 97), power = c(1, 1.3))

library(scico)
pal = scico(7, palette = 'vik')

ssp1_2050_ssp1_2025_map = scenarios_permap(ssp1_2050_ssp1_2025, pal = pal)

ssp4_2050_ssp1_2025_map = scenarios_permap(ssp4_2050_ssp1_2025, pal = pal)

ssp5_2050_ssp1_2025_map = scenarios_permap(ssp5_2050_ssp1_2025, pal = pal)

scenarios_boxmap2(ssp5_2050_ssp1_2025, pal = pal)

fig_2 = (ssp1_2050_ssp1_2025_map / ssp4_2050_ssp1_2025_map / ssp5_2050_ssp1_2025_map) + plot_annotation(tag_levels = 'A')

ggsave(here('figures/fig_2.pdf'), fig_2, 
       device = 'pdf', width = 12, height = 15, dpi = 300, units = "cm", scale = 2)


# Figure 4 ----------------------------------------------------------------
# Avoided cost

ssp4_2050_ssp1_2050 = raster(here('data/SSP4_2050_SSP1_2050.tif'), crs = crs_proj)
ssp5_2050_ssp1_2050 = raster(here('data/SSP5_2050_SSP1_2050.tif'), crs = crs_proj)

# scenarios_boxmap2(ssp4_2050_ssp1_2050, pal = pal)
# scenarios_boxmap2(ssp5_2050_ssp1_2050, pal = pal)

a = scenarios_permap(ssp5_2050_ssp1_2050, pal = pal)
b = scenarios_permap(ssp4_2050_ssp1_2050, pal = pal)

# MAD scatter plots, following differ

ssp1_2050_weighted = weighted_area_moll(ssp1_2050, world)
ssp4_2050_weighted = weighted_area_moll(ssp4_2050, world)
ssp5_2050_weighted = weighted_area_moll(ssp5_2050, world)

# Scatter plot
c = ssp1_2050_weighted %>%
  left_join(ssp4_2050_weighted) %>%
  left_join(ssp5_2050_weighted) %>%
  mutate(point_size = case_when(
    income_grp == "1. High income: OECD" ~ 2.5,
    income_grp == "2. High income: nonOECD" ~ 1,
    income_grp == "5. Low income" ~ 1,
    income_grp == "4. Lower middle income" ~ 1,
    income_grp == "3. Upper middle income" ~ 1)
  ) %>%
  ggplot(aes(x = SSP1_2050_weighted, 
             y = SSP4_2050_weighted, 
             size = point_size, 
             colour = income_grp) )+ 
  geom_point() + 
  #scale_size_continuous(range = c(3, 7)) +
  scale_color_brewer(type = "qual", palette = 'Set1') +
  scale_size_identity() +
  #scale_size(range = c(1, 3)) +
  guides(fill = FALSE) +
  #coord_fixed(ratio = 1) + 
  #geom_smooth(aes(x = SSP1_2050_weighted, y = SSP5_2050_weighted), method = "loess") +
  geom_abline(colour = "black") + 
  #facet_wrap(~income_grp) +
  xlab("SSP1 2050") + 
  ylab("SSP4 2050") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        #axis.ticks = element_blank(),
        panel.spacing = unit(1, "lines"),
        #plot.margin = unit(c(0, 0, 0, 0), "null"),
        #panel.margin = unit(c(0, 0, 0, 0), "null"),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        #legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.2, 'cm'),
        legend.margin = margin(t = 0, unit='cm')
        )


# MAD scatter plots, following differ
# Scatter plot
d = ssp1_2050_weighted %>%
  left_join(ssp4_2050_weighted) %>%
  left_join(ssp5_2050_weighted) %>%
  mutate(point_size = case_when(
    income_grp == "1. High income: OECD" ~ 2.5,
    income_grp == "2. High income: nonOECD" ~ 1,
    income_grp == "5. Low income" ~ 1,
    income_grp == "4. Lower middle income" ~ 1,
    income_grp == "3. Upper middle income" ~ 1)
  ) %>%
  ggplot(aes(x = SSP1_2050_weighted, 
             y = SSP5_2050_weighted, 
             size = point_size, 
             colour = income_grp) )+ 
    geom_count() + 
    #scale_size_continuous(range = c(3, 7)) +
    scale_color_brewer(type = "qual", palette = 'Set1') +
    scale_size_identity() +
    #scale_size(range = c(1, 3)) +
    guides(fill = FALSE) +
    #coord_fixed(ratio = 1) + 
    #geom_smooth(aes(x = SSP1_2050_weighted, y = SSP5_2050_weighted), method = "loess") +
    geom_abline(colour = "black") + 
    #facet_wrap(~income_grp) +
    xlab("SSP1 2050") + 
    ylab("SSP5 2050") +
    theme_bw() +
    theme(legend.position = c(0.8, 0.2),
        #axis.ticks = element_blank(),
        panel.spacing = unit(1, "lines"),
        #plot.margin = unit(c(0, 0, 0, 0), "null"),
        #panel.margin = unit(c(0, 0, 0, 0), "null"),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        #legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.2, 'cm'),
        legend.margin = margin(t = 0, unit='cm')
        )
  
fig_4_top = a + c
fig_4_bottom = b + d

fig_4 = fig_4_top / fig_4_bottom + plot_annotation(tag_levels = "A")

ggsave(here('figures/fig_4.pdf'), fig_4, 
       device = 'pdf', width = 12, height = 6, dpi = 300, units = "cm", scale = 2)





# Boxplots and scatter plots to analyze avoided hazard
# Read individual scenario models
ssp1_2025 = raster(here('data/SSP1_2025.tif'), crs = crs_proj)
ssp1_2050 = raster(here('data/SSP1_2050.tif'), crs = crs_proj)
ssp4_2050 = raster(here('data/SSP4_2050.tif'), crs = crs_proj)
ssp5_2050 = raster(here('data/SSP5_2050.tif'), crs = crs_proj)

# Calculate weighted hazard per country 
ssp4_ssp1_2050_weighted = weighted_area_moll(ssp4_2050_ssp1_2050, world)
ssp5_ssp1_2050_weighted = weighted_area_moll(ssp5_2050_ssp1_2050, world)

# Boxplots 

left_join(ssp4_ssp1_2050_weighted, ssp5_ssp1_2050_weighted) %>%
  pivot_longer(!c(name_sort, region_wb, continent, economy, income_grp), names_to = "scenario", values_to = "avoided_hazard") %>%
  filter(region_wb != "Antarctica") %>%
  ggplot() + 
    geom_boxplot(aes(x = income_grp, y = avoided_hazard, fill = scenario)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# From SSP1 2050 vs the other scenarios (e.g. SSP12050 - SSP42050)
# Subtraction SSP1 2050 - SSP4 2050
ssp1_2050_ssp4_2050 = ssp1_2050 - ssp4_2050
names(ssp1_2050_ssp4_2050) <- "ssp1_2050_ssp4_2050"

# Subtraction SSP1 2050 - SSP5 2050
ssp1_2050_ssp5_2050 = ssp1_2050 - ssp5_2050
names(ssp1_2050_ssp5_2050) <- "ssp1_2050_ssp5_2050"

# Calculate weighted hazard
ssp1_ssp4_2050_weighted = weighted_area_moll(ssp1_2050_ssp4_2050, world)
ssp1_ssp5_2050_weighted = weighted_area_moll(ssp1_2050_ssp5_2050, world)

# Boxplots

left_join(ssp1_ssp4_2050_weighted, ssp1_ssp5_2050_weighted) %>%
  pivot_longer(!c(name_sort, region_wb, continent, economy, income_grp), names_to = "scenario", values_to = "avoided_hazard") %>%
  filter(region_wb != "Antarctica") %>%
  ggplot(aes(x = income_grp, y = avoided_hazard, fill = scenario)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))




    
    ggplot(mtcars2) +
      geom_jitter(aes(value,mpg, colour=variable),) + geom_smooth(aes(value,mpg, colour=variable), method=lm, se=FALSE) +
      facet_wrap(~variable, scales="free_x") +
      labs(x = "Percentage cover (%)", y = "Number of individuals (N)")
    

view(avoided %>% filter(scenario == 'weigthed_risk_ssp5'))


%>%
  summary(avoided_risk)
