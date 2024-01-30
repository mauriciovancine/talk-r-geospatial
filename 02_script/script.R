#' ----
#' theme: análise de dados geoespaciais no r
#' autor: mauricio vancine
#' data: 2024-03-12
#' ----

# preparar r --------------------------------------------------------------

# pacotes
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(tmap)

# importar dados ----------------------------------------------------------

# tabela ----

## importar ----
pontos <- readr::read_csv("03_data/pontos.csv")
pontos

plot(pontos)
plot(pontos[, 2:3])

# vetor ----

## pontos ----

### terra ----
# transformar
pontos_terra <- terra::vect(as.matrix(pontos[, 2:3]), crs = "+proj=longlat +datum=WGS84")
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

# reprojetar --------------------------------------------------------------

## vetor ----



# end ---------------------------------------------------------------------
