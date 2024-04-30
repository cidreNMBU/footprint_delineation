

get_lidar_footprint <- function(dsm, 
                                position_x,
                                position_y, 
                                h, 
                                scan_angle) {
  
  ## 0. Manage posible errors
  if (terra::nlyr(dsm) > 1) stop("Please, introduce a single layer object with a DSM")
  names(dsm) <- "Z"
  
  ## 1. Get UAV position -----------------------------
  
  uav_sf <- sf::st_as_sf(
    x      = data.frame(x = position_x, y = position_y),
    coords = c("x", "y"),
    crs    = crs(dsm)
  )
  
  
  ## 2. UAV range ------------------------------------
  
  ## Get relative height
  ## Difference between UAV and DSM
  relative_height_sr <- h - dsm
  
  ## Euclidean distance between drone point and any other point
  euclidean_distance_sr <- terra::distance(
    relative_height_sr,
    uav_sf
  )
  
  ## dmax
  dmax <- relative_height_sr * tan(scan_angle / 2 * pi / 180)
  
  ## Mask values inside the drone area
  mask_sr <- terra::ifel(
    euclidean_distance_sr <= dmax,
    1, 0
  )
  
  ## 3. Shadowing trees ------------------------------
  
  ## Get observer height
  observer_height <- terra::extract(
    x = relative_height_sr,
    y = cbind(position_x, position_y)
  ) |> dplyr::pull(Z)
  
  ## Calculate viewshed
  viewshed_sr <- terra::viewshed(
    x        = dsm,
    loc      = c(position_x, position_y),
    observer = observer_height,
  )
  ## Convert to 1/0
  viewshed_sr <- terra::ifel(
    isTRUE(viewshed_sr), 1, 0
  )
  
  ## 4. UAV footprint -----------------------------------
  
  footprint_sr <- mask_dmax_sr * viewshed_sr
  
  ## 5. Vectorize ---------------------------------------
  
  ## Convert to SF polygon
  footprint_sf <- terra::as.polygons(footprint_sr) %>%
    sf::st_as_sf() %>% 
    dplyr::filter(Z == 1) %>% 
    sf::st_cast("POLYGON")
  
  ## Get a list with results
  return(
    list(
      spatraster = footprint_sr,
      sf         = footprint_sf
    )
  )
}


visualize_plot <- function(dsm, footprint_sf, pal, position_x, position_y) {
  ggplot() +
    ## Digital surface model
    geom_spatraster(
      data = dsm
    ) +
    scale_fill_gradientn(
      colors   = pal,
      na.value = NA,
      name     = "DSM (m)"
    ) +
    ## Area
    geom_sf(
      data = footprint_sf,
      fill  = NA,
      color = "black",
      linewidth = 1.5
    ) +
    ## Drone
    geom_icon(
      data = data.frame(x = position_x, y = position_y),
      aes(x, y, image = "airplane-outline")
    ) +
    ## Labels
    labs(
      title = str_glue(
        "Footprint flying at {h} meters with scan angle of {scan_angle}°"
      )
    ) +
    ## Theme
    theme_void(
      base_family = "Source Sans Pro"
    ) +
    theme(
      plot.title = element_text(
        size  = 15,
        hjust = .5,
        face  = "bold"
      )
    )
}

get_center_h <- function(x, y, h, dsm) {
  
  height_val <- terra::extract(
    x = dsm,
    y = cbind(x, y)
  )
  
  return(
   pull(h - height_val, Z)
  )
  
}















