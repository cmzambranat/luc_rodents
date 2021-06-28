luc_rod_plot = function(luc_raster, legend_yn = c(0.1, 0.35), pal = NULL, cI = NULL, lab = NULL){
  ggplot2::ggplot() +
              layer_spatial(luc_raster) +
                geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
                #scale_fill_viridis(option = 'magma', na.value = NA, direction = -1) +
                scale_fill_gradientn(na.value = NA, colours = pal,
                                     values = rescale(cI),
                                     limits = c(minValue(luc_raster), maxValue(luc_raster)),
                                     labels = lab,
                                     guide = 'legend' ) +
                #guides(fill = guide_colourbar(barwidth = 1, barheight = 10)) +
                theme_bw() + 
                theme(legend.position = legend_yn,
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
                      legend.text = element_text(size = 10)) +
                coord_sf(xlim = st_bbox(luc_raster)[c(1, 3)],
                         ylim = st_bbox(luc_raster)[c(2, 4)],
                         expand = FALSE)
}





scenarios_permap = function(luc_raster, legend_yn = c(0.1, 0.35), pal = NULL, lab = NULL){
  percent <- c(0, .01, .1, .5, .9, .99, 1)
  var <- values(luc_raster)
  var <- var[!is.na(var)]
  bperc <- round(quantile(var, percent, na.rm = T), 3)
  ggplot2::ggplot() +
  layer_spatial(luc_raster) +
  geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
  scale_fill_gradientn(na.value = NA, 
                       colours = pal, 
                       values = rescale(bperc),
                       #labels = lab,
                       limits = c(minValue(luc_raster), maxValue(luc_raster)),
                       guide = 'legend') +
  theme_bw() + 
  theme(legend.position = legend_yn,
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill = "#def3f6"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        legend.margin = margin(t = 0, unit='cm')
        ) +
  coord_sf(xlim = st_bbox(ssp1_2050_ssp1_2025)[c(1, 3)],
           ylim = st_bbox(ssp1_2050_ssp1_2025)[c(2, 4)],
           expand = FALSE)
}




scenarios_boxmap = function(luc_raster, legend_yn = c(0.1, 0.35), pal = NULL, lab = NULL){
  percent <- c(0, .01, .1, .5, .9, .99, 1)
  var <- values(luc_raster)
  var <- var[!is.na(var)]
  bperc <- round(quantile(var, percent, na.rm = T), 3)
  ggplot2::ggplot() +
    layer_spatial(luc_raster) +
    geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
    scale_fill_gradientn(na.value = NA, 
                         colours = pal, 
                         values = rescale(bperc),
                         #labels = lab,
                         limits = c(minValue(luc_raster), maxValue(luc_raster)),
                         guide = 'legend') +
    theme_bw() + 
    theme(legend.position = legend_yn,
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
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
}



weighted_area_moll = function(raster_layer, zones, method = 'cubic'){
  varname <- paste0(names(raster_layer), '_weighted')
  crs_moll = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs'
  zones_proj = st_transform(zones, crs_moll)
  raster_layer_proj = terra::project(terra::rast(raster_layer), y = crs_moll, method = method)
  tibble(exactextractr::exact_extract(raster(raster_layer_proj), zones_proj, function(values, coverage_fraction)
    sum(values * coverage_fraction, na.rm = TRUE) / sum(coverage_fraction),
    append_cols = TRUE) %>%
      mutate(!!varname := na_if(result, 0)) %>%
      select(name_sort, region_wb, continent, economy, income_grp, varname)
  )
}












luc_raster = ssp4_2050_ssp1_2025
luc_raster = ssp1_2050_ssp1_2025

scenarios_boxmap2 = function(luc_raster, legend_yn = c(0.1, 0.35), pal = NULL){
  var <- values(luc_raster)
  var <- var[!is.na(var)]
  box <- round(boxbreaks(var), 3)
  ggplot2::ggplot() +
    layer_spatial(luc_raster) +
    geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
    scale_fill_gradientn(na.value = NA, 
                         colours = pal, 
                         values = scales::rescale(box),
                         #labels = lab,
                         limits = c(minValue(luc_raster), maxValue(luc_raster)),
                         guide = 'legend') +
    theme_bw() + 
    theme(legend.position = legend_yn,
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
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
}







boxbreaks <- function(v, mult = 1.5) {
  # break points for box map
  # arguments:
  #   v: vector with observations
  #   mult: multiplier for IQR (default 1.5)
  # returns:
  #   bb: vector with 7 break points
  # compute quartile and fences
  qv <- unname(quantile(v))
  iqr <- qv[4] - qv[2]
  upfence <- qv[4] + mult * iqr
  lofence <- qv[2] - mult * iqr
  # initialize break points vector
  bb <- vector(mode="numeric",length=7)
  # logic for lower and upper fences
  if (lofence < qv[1]) {  # no lower outliers
    bb[1] <- lofence
    bb[2] <- floor(qv[1])
  } else {
    bb[2] <- lofence
    bb[1] <- qv[1]
  }
  if (upfence > qv[5]) { # no upper outliers
    bb[7] <- upfence
    bb[6] <- ceiling(qv[5])
  } else {
    bb[6] <- upfence
    bb[7] <- qv[5]
  }
  bb[3:5] <- qv[2:4]
  return(bb)
}



cI = round(boxbreaks(var, 2), 2)

cI = round(classIntervals(var, style = 'quantile', n = 5, intervalClosure = "left")$brks, 2)


cI = c(-2.32, -0.123, -0.02, 0.139, 3.40)



