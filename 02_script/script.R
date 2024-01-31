#' ----
#' theme: analise de dados geoespaciais no r
#' autor: mauricio vancine
#' data: 2024-03-12
#' ----

# preparar r --------------------------------------------------------------

# pacotes
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(geodata)
library(tmap)

# options
options(scipen = 1e3)

# importar dados ----------------------------------------------------------

# tabela ----

## importar ----
pontos <- readr::read_csv("03_data/pontos_campinas.csv")
pontos

plot(pontos)
plot(pontos[, 2:3], pch = 20, cex = 2)

# vetor ----

## pontos ----

### terra ----
# transformar
pontos_terra <- terra::vect(as.matrix(pontos[, 2:3]), crs = "EPSG:4326")
pontos_terra

# importar
pontos_terra <- terra::vect("03_data/pontos_campinas.gpkg")
pontos_terra <- terra::vect("03_data/pontos_campinas.shp")
pontos_terra

plot(pontos_terra)

### sf ----
# transformar
pontos_sf <- sf::st_as_sf(pontos, coords = c("longitude", "latitude"), crs = 4326)
pontos_sf

# importar
pontos_sf <- sf::st_read("03_data/pontos_campinas.gpkg")
pontos_sf <- sf::st_read("03_data/pontos_campinas.shp")
pontos_sf

plot(pontos_sf)
plot(pontos_sf$geometry)

### tabela de atributos ----
terra::geom(pontos_terra)
sf::st_drop_geometry(pontos_sf)

## poligonos ----
campinas_sf <- geobr::read_municipality(code_muni = 3509502, year = 2022)
campinas_sf

campinas_terra <- terra::vect(campinas_sf)
campinas_terra

plot(campinas_sf$geom, col = "gray", axes = TRUE)
plot(pontos_sf$geometry, pch = 20, add = TRUE)

plot(campinas_terra, col = "gray")
plot(pontos_terra, add = TRUE)

# tabela de atributos
sf::st_drop_geometry(campinas_sf)

## raster ----

### terra ----
mapbiomas_terra <- terra::rast("03_data/mapbiomas_10m_campinas.tif")
mapbiomas_terra

plot(mapbiomas_terra)
plot(campinas_terra, add = TRUE)
plot(pontos_terra, add = TRUE)

### stars ----
mapbiomas_stars <- stars::read_stars("03_data/mapbiomas_10m_campinas.tif")
mapbiomas_stars

plot(mapbiomas_stars)

## worldclim ----
### precipitation ----
precipitation <- geodata::worldclim_tile(var = "prec", 
                                         lon = pontos$longitude[1], 
                                         lat = pontos$latitude[1], 
                                         path = "03_data")
precipitation
plot(precipitation)

### temperature ----
temperature <- geodata::worldclim_tile(var = "tavg", 
                                       lon = pontos$longitude[1], 
                                       lat = pontos$latitude[1], 
                                       path = "03_data")
temperature
plot(temperature)

### elevation ----
elevation <- geodata::elevation_3s(lon = pontos$longitude[1], 
                                   lat = pontos$latitude[1], 
                                   path = "03_data")
elevation
plot(elevation)

# reprojetar --------------------------------------------------------------

## vetor ----

### terra ----
pontos_terra_utm <- terra::project(x = pontos_terra, y = "EPSG:32723")
pontos_terra_utm

plot(pontos_terra)
plot(pontos_terra_utm)

### sf ----
pontos_sf_utm <- sf::st_transform(pontos_sf, crs = 32723)
pontos_sf_utm

plot(pontos_sf$geometry, pch = 20, axes = TRUE)
plot(pontos_sf_utm$geometry, pch = 20, axes = TRUE)

## raster ----
mapbiomas_terra
res(mapbiomas_terra)[1]*3600*30

mapbiomas_terra_utm <- terra::project(x = mapbiomas_terra, y = "EPSG:32723", method = "near", res = 10)
mapbiomas_terra_utm

plot(mapbiomas_terra)
plot(mapbiomas_terra_utm)

# operacoes -------------------------------------------------

## extrair valores para pontos ----
plot(elevation)
plot(pontos_terra, add = TRUE)

pontos_sf_elev <- pontos_sf %>% 
    dplyr::mutate(elev = terra::extract(elevation, pontos_terra, ID = FALSE)[, 1])
pontos_sf_elev

pontos_terra_prep <- terra::extract(precipitation, pontos_terra, ID = FALSE)
colnames(pontos_terra_prep) <- sub("tile_41_wc2.1_30s_", "", colnames(pontos_terra_prep))
pontos_terra_prep

pontos_terra_temp <- terra::extract(temperature, pontos_terra, ID = FALSE)
colnames(pontos_terra_temp) <- sub("tile_41_wc2.1_30s_", "", colnames(pontos_terra_temp))
pontos_terra_temp

pontos_sf_elev_prec_temp <- pontos_sf_elev %>% 
    dplyr::bind_cols(pontos_terra_prep, pontos_terra_temp)
pontos_sf_elev_prec_temp

## media dos rasters
precipitation_annual <- terra::app(precipitation, sum, cores = 3)
precipitation_annual
plot(precipitation_annual)

temperature_mean <- terra::app(temperature, mean, cores = 3)
temperature_mean
plot(temperature_mean)

pontos_sf_elev_prec_temp <- pontos_sf_elev %>% 
    dplyr::mutate(prec = terra::extract(precipitation_annual, pontos_terra, ID = FALSE)[, 1],
                  temp = terra::extract(temperature_mean, pontos_terra, ID = FALSE)[, 1],)
pontos_sf_elev_prec_temp

## extensao e mascara ----
mapbiomas_terra_campinas <- terra::crop(mapbiomas_terra, campinas_terra, mask = TRUE)
elevation_campinas <- terra::crop(elevation, campinas_terra, mask = TRUE)
precipitation_annual_campinas <- terra::crop(precipitation_annual, campinas_terra, mask = TRUE)
temperature_mean_campinas <- terra::crop(temperature_mean, campinas_terra, mask = TRUE)

plot(mapbiomas_terra_campinas)
plot(elevation_campinas)
plot(precipitation_annual_campinas)
plot(temperature_mean_campinas)

## reamostragem ----
mapbiomas_terra_campinas_rea <- terra::resample(mapbiomas_terra_campinas, precipitation_annual_campinas, method = "near")
mapbiomas_terra_campinas_rea

plot(mapbiomas_terra_campinas)
plot(mapbiomas_terra_campinas_rea)

elevation_campinas_rea <- terra::resample(elevation_campinas, precipitation_annual_campinas)
elevation_campinas_rea

plot(elevation_campinas)
plot(elevation_campinas_rea)

precipitation_annual_campinas_rea_nea <- terra::resample(precipitation_annual_campinas, elevation_campinas, method = "near")
precipitation_annual_campinas_rea_bil <- terra::resample(precipitation_annual_campinas, elevation_campinas, method = "bilinear")
precipitation_annual_campinas_rea_cub <- terra::resample(precipitation_annual_campinas, elevation_campinas, method = "cubic")

precipitation_annual_campinas_rea_nea
precipitation_annual_campinas_rea_bil
precipitation_annual_campinas_rea_cub

plot(precipitation_annual_campinas)
plot(precipitation_annual_campinas_rea_nea)
plot(precipitation_annual_campinas_rea_bil)
plot(precipitation_annual_campinas_rea_cub)

## buffer ----
pontos_terra_buffer <- terra::buffer(pontos_terra, width = 1000)
pontos_terra_buffer

campinas_terra_buffer <- terra::buffer(campinas_terra, width = -1000)
campinas_terra_buffer

plot(campinas_terra)
plot(campinas_terra_buffer, border = "red", add = TRUE)
plot(pontos_terra, add = TRUE)
plot(pontos_terra_buffer, add = TRUE)

## extrair dados para o buffer ----
ambiental <- c(elevation_campinas_rea, precipitation_annual_campinas, temperature_mean_campinas)
ambiental
plot(ambiental)

pontos_terra_buffer_amb <- terra::zonal(ambiental, pontos_terra_buffer, fun = "mean")
pontos_terra_buffer_amb

pontos_sf_elev_prec_temp

# maps --------------------------------------------------------------------

## estatico ----
tmap_mode(mode = "plot")

### pontos ----
map_pontos <- tm_shape(campinas_sf) +
    tm_polygons() +
    tm_shape(pontos_sf_elev_prec_temp) +
    tm_bubbles(fill = "elev",
               fill.scale = tm_scale_continuous(values = "-RdYlGn"),
               fill.legend = tm_legend(title = "Elevação (m)", 
                                      position = tm_pos_in("left", "top"),
                                      reverse = TRUE)) +
    tm_graticules(lines = FALSE) +
    tm_compass(position = c("right", "bottom"), size = 3) +
    tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position = c("right", "bottom"))
map_pontos

### raster ----
map_uso <- tm_shape(mapbiomas_terra_campinas) +
    tm_raster(col.scale = tm_scale_categorical(values = c("#1f8d49", "#7a5900", "#519799",
                                                          "#edde8e", "#C27BA0", "#ffefc3",
                                                          "#d4271e", "#db4d4f", "#ffaa5f",
                                                          "#9c0027", "#2532e4", "#d082de")),
              col.legend = tm_legend(title = "Classes", 
                                     position = tm_pos_in("left", "top"))) +
    tm_shape(campinas_sf) +
    tm_borders() +
    tm_graticules(lines = FALSE) +
    tm_compass(position = c("right", "bottom"), size = 3) +
    tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position = c("right", "bottom"))
map_uso

### varios raster ----
map_elev <- tm_shape(elevation_campinas_rea) +
    tm_raster(col.scale = tm_scale_continuous(values = "-RdYlGn"),
              col.legend = tm_legend(title = "Elevação (m)", 
                                     position = tm_pos_in("left", "top"),
                                     reverse = TRUE)) +
    tm_shape(campinas_sf) +
    tm_borders() +
    tm_graticules(lines = FALSE) +
    tm_compass(position = c("right", "bottom"), size = 3) +
    tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position = c("right", "bottom"))
map_elev

map_prec <- tm_shape(precipitation_annual_campinas) +
    tm_raster(col.scale = tm_scale_continuous(),
              col.legend = tm_legend(title = "Precipitação (mm)", 
                                     position = tm_pos_in("left", "top"),
                                     reverse = TRUE)) +
    tm_shape(campinas_sf) +
    tm_borders() +
    tm_graticules(lines = FALSE) +
    tm_compass(position = c("right", "bottom"), size = 3) +
    tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position = c("right", "bottom"))
map_prec

map_temp <- tm_shape(temperature_mean_campinas) +
    tm_raster(col.scale = tm_scale_continuous(values = "-Spectral"),
              col.legend = tm_legend(title = "Temperatural (ºC)", 
                                     position = tm_pos_in("left", "top"),
                                     reverse = TRUE)) +
    tm_shape(campinas_sf) +
    tm_borders() +
    tm_graticules(lines = FALSE) +
    tm_compass(position = c("right", "bottom"), size = 3) +
    tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position = c("right", "bottom"))
map_temp

map <- tmap_arrange(map_uso, map_elev, map_prec, map_temp)
map

# export
tmap_save(map, "03_data/map.png", width = 45, height = 30, units = "cm", dpi = 300, asp = FALSE)

## interativo ----
tmap_mode(mode = "view")

map_pontos <- tm_shape(campinas_sf) +
    tm_polygons() +
    tm_shape(pontos_sf_elev_prec_temp) +
    tm_bubbles(fill = "elev",
               fill.scale = tm_scale_continuous(values = "-RdYlGn"),
               fill.legend = tm_legend(title = "Elevação (m)", 
                                       position = tm_pos_in("left", "top"),
                                       reverse = TRUE))
map_pontos

map_uso <- tm_shape(mapbiomas_terra_campinas) +
    tm_raster(col.scale = tm_scale_categorical(values = c("#1f8d49", "#7a5900", "#519799",
                                                          "#edde8e", "#C27BA0", "#ffefc3",
                                                          "#d4271e", "#db4d4f", "#ffaa5f",
                                                          "#9c0027", "#2532e4", "#d082de")),
              col.legend = tm_legend(title = "Classes", 
                                     position = tm_pos_in("left", "top"))) +
    tm_shape(campinas_sf) +
    tm_borders()
map_uso

# exportar
tmap::tmap_save(tm = map_pontos, filename = "03_data/map_pontos.html")
tmap::tmap_save(tm = map_uso, filename = "03_data/map_uso.html")

# end ---------------------------------------------------------------------
