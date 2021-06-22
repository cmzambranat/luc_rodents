luc_rod_plot = function(luc_raster, cI, pal){
  ggplot2::ggplot() +
              layer_spatial(luc_raster) +
                geom_sf(data = world, color = "black", fill = NA, size = 0.2) +
                scale_fill_gradientn(na.value = NA, colours = pal, 
                                     values = rescale(cI),
                                     limits=c(minValue(luc_raster), maxValue(luc_raster))) +
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
                coord_sf(xlim = st_bbox(luc_raster)[c(1, 3)],
                         ylim = st_bbox(luc_raster)[c(2, 4)],
                         expand = FALSE)
}






boxbreaks <- function(v,mult=1.5) {
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



