grid_to_raster <- function (grid, var) {
  # original: plotdap:::get_raster
  # grid <- sst_grid
  #library(magrittr)
  
  times <- grid$summary$dim$time$vals
  lats <- grid$summary$dim$latitude$vals
  lons <- grid$summary$dim$longitude$vals
  ylim <- range(lats, na.rm = TRUE)
  xlim <- range(lons, na.rm = TRUE)
  ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])
  r <- if (length(times) > 1) {
    d <- dplyr::arrange(grid$data, time, desc(lat), lon)
    b <- raster::brick(nl = length(times), nrows = length(lats), 
                       ncols = length(lons))
    raster::values(b) <- lazyeval::f_eval(var, d)
    raster::setExtent(b, ext)
  }
  else {
    d <- dplyr::arrange(grid$data, desc(lat), lon)
    r <- raster::raster(nrows = length(lats), ncols = length(lons), 
                   #ext = ext, vals = lazyeval::f_eval(var, d)) # plotdap:::get_raster
                   ext = ext, vals = d[,var])
  }
  #browser()
  #names(r) <- make.names(unique(grid$data$time) %||% "")
  r
}