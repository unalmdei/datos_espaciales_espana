library(sf)
# Cargar datos
## Poligonos
espana <- read_sf("data/limites_espana.shp")
head(espana)

aeropuertos <- read_sf("data/BCN500_0606P_AEROP_AEROD.shp")
aeropuertos

library(raster)
# Cargar datos
## Ráster individual
lst_01 <- raster("data/raster/lst_01.tif")
lst_01


# Cargar datos
## Grupos de rásters
l <- list.files("data/raster/", full.names = T)
lst <- stack(l)


library(terra)
lst_01_terra<-terra::rast("data/raster/lst_01.tif")
lst_01_terra


# La función 'rast' buscará y cargará todos los archivos que coincidan con el patrón
# Se crea un único objeto 'SpatRaster' con múltiples capas
capas_raster <- terra::rast(list.files("data/raster", pattern = "\\.tif", full.names = TRUE))
nlyr(capas_raster)


# Cambiar proyección
## Proyección actual 
st_crs(espana)$epsg
st_crs(espana)$proj4string

## Cambiar a Pseudo Mercator
espana_psm <- st_transform(espana, crs = 3857)
st_crs(espana_psm)$epsg
st_crs(espana_psm)$proj4string

# Conocer CRS actual
crs(lst_01)
## CRS arguments: +proj=longlat +datum=WGS84 +no_defs
# Proyectar a UTM
lst_01_psm <- projectRaster(lst_01, crs = crs("+init=epsg:3857"))
crs(lst_01_psm)

#Extensión y resolución:
extent(lst_01)
res(lst_01)

extent(lst_01_psm)
res(lst_01_psm)


#Visualización
library(sf)
library(tidyverse)
#Cortar vector por atributos y visualizar
# Seleccionar sólo Comunidad de Madrid
andalucia <- espana %>%
  filter(CCAA == "Andalucía")
plot(andalucia)

#vectores y ggplot
## Gráfico con ggplot
library(ggplot2)
ggplot(andalucia) +
  geom_sf(aes(fill = ETIQUETA))

#cortar raster por máscaras
# Creo máscara
madrid_psm <- espana %>%
  filter(CCAA == "Comunidad de Madrid") %>%     
  st_transform(crs = 3857)
madrid_lst_01 <- mask(lst_01_psm, madrid_psm)
plot(madrid_lst_01)

madrid_lst_01 <- crop(madrid_lst_01, madrid_psm)
plot(madrid_lst_01)

#Rásters y ggplot2
# Convertir a tabla
rt <- data.frame(rasterToPoints(madrid_lst_01))
colores <- rev(terrain.colors(15))
ggplot() +
  geom_raster(data = rt,
              aes(x = x, y = y, fill = LST_Day_1km)) +
  scale_fill_gradientn(colours = colores) +
  coord_equal()
