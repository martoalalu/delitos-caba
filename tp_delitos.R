library(data.table)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)
library(data.table)
library(ggmap)


delitos <- fread("https://raw.githubusercontent.com/martoalalu/delitos-caba/master/data/delitos.csv")
delitos_geo <- delitos %>% 
  filter(!is.na(longitud), !is.na(latitud)) %>%
  filter(longitud != 0) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

colnames(delitos_geo)
unique(delitos_geo$tipo_delito)

delitos_geo_robo <- delitos_geo %>% 
  filter(tipo_delito =="Robo (Con violencia)")

bbox <- c(-58.546686, #Asignar coordenadas de los límites del bounding box de la Ciudad
          -34.711145,
          -58.329097,
          -34.529156)

CABA <- get_stamenmap(bbox = bbox, #Descargar mapa
                      maptype = "toner-lite",
                      zoom=13)


ggmap(CABA) +
  geom_bin2d(data = filter(delitos,tipo_delito=="Lesiones Seg Vial"),
             aes(x = longitud, y = latitud), bins = 100, alpha = 0.8) +
  labs(title="Distribución de delitos",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://https://mapa.seguridadciudad.gob.ar/",
       fill="Cantidad")+
  scale_fill_distiller(palette = "Spectral")+
  theme_void()

ggmap(CABA) +
  stat_density_2d(data = delitos, 
                  aes(x = longitud, y = latitud, 
                      fill = stat(level)), alpha = 0.6, geom = "polygon") +
  labs(title="Distribución de delitos",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       fill="Nivel")+
  scale_fill_distiller(palette = "Spectral")+
  theme_void()+
  facet_wrap(~tipo_delito)

calles <- read_sf("C:/Users/20332842324/Desktop/callejero.shp")


#Cantidad por barrios
#


comisarias<-read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv")

comisarias <- comisarias %>% 
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)



barrios <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

ggplot() + 
       geom_sf(data=barrios,size=.0)+
       geom_sf(data=comisarias, size=1, alpha=1, color="red")+
       geom_sf(data=delitos, size=1, alpha=1, color="blue")
       labs(title = "Densidad de población en centroides",
            subtitle = "Ciudad Autónoma de Buenos Aires")

       
ggplot(data = dat, mapping = aes(x = x, y = y)) +
         geom_pointdensity(adjust = 4) +
         scale_color_viridis()       

https://github.com/LKremer/ggpointdensity
https://datanerdses.org/2019/08/29/resumen-del-encuentro-data-nerds-es-ago-2019-ppts-codigo-r/
  http://rpubs.com/angiescetta/actividad-comercial