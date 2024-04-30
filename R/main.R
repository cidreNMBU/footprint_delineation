
# 1. Load packages --------------------------------------------------------

library(pacman)

p_load(
  tidyverse, lidR, terra, sf, tidyterra, ggimage
)

# 2. Load data ------------------------------------------------------------

## Get LAS from lidR package
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las     <- readLAS(LASfile, select = "xyzc")

## Get the Digital Surface Model
dsm_sr <- rasterize_canopy(
  las       = las,
  res       = 3,
  algorithm = p2r()
)

## Lidar palette
pal <- colorRamps::matlab.like(50)

## Visualize it
plot(dsm_sr, col = pal)

# 3. Calculate polygon ----------------------------------------------------

## 3.1. Set up ------------------------------------------

## Parameters
scan_angle <- 20
h          <- 900
position_x <- 273500
position_y <- 5274500

## Get drone position
drone_sf <- st_as_sf(
  x      = data.frame(x = position_x, y = position_y),
  coords = c("x", "y"),
  crs    = crs(dsm_sr)
)

## Visualize
plot(dsm_sr, col = pal)
plot(drone_sf, add = TRUE)

## 3.2. Get euclidean distance --------------------------

## Get relative height
## Difference between drone and DSM
relative_height_sr <- h - dsm_sr

## Euclidean distance between drone point and any other point
euclidean_distance_sr <- terra::distance(
  relative_height_sr,
  drone_sf
)

## Visualize
plot(euclidean_distance_sr)
plot(drone_sf, add = TRUE)


## 3.3. Mask drone area ----------------------------------

## dmax
dmax <- relative_height_sr * tan(scan_angle / 2)

## Visualize
plot(dmax)

## Mask values inside the drone area
mask_sr <- terra::ifel(
  euclidean_distance_sr <= dmax,
  1, 0
)

## Visualize
plot(mask_sr)

## 3.4. Convert to polygon -------------------------------

## Convert to SF polygon
mask_sf <- terra::as.polygons(mask_sr) %>%
  st_as_sf() %>% 
  filter(Z == 1) %>% 
  st_cast("POLYGON")


mask_sf %>% 
  ggplot() +
  geom_sf(fill = "indianred2")


## 3.5. Visualize with raster ----------------------------

## Visualize
ggplot() +
  ## Digital surface model
  geom_spatraster(
    data = dsm_sr
  ) +
  scale_fill_gradientn(
    colors   = pal,
    na.value = NA,
    name     = "DSM (m)"
  ) +
  ## Area
  geom_sf(
    data = mask_sf,
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


# 4. Test function --------------------------------------------------------

## parameters
scan_angle <- 20
h          <- 900 
position_x <- 273500
position_y <- 5274500

## Get polygon
mask_sf <- get_lidar_plot(
  dsm        = dsm_sr,
  position_x = position_x,
  position_y = position_y,
  h          = h,
  scan_angle = scan_angle
)

## Visualize
visualize_plot(
  dsm        = dsm_sr,
  mask_sf    = mask_sf,
  pal        = pal,
  position_x = position_x,
  position_y = position_y
)
























