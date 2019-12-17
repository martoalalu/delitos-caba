library(data.table)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)
library(data.table)
library(ggmap)
library(janitor)
library(ggdark)

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
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/",
       fill="Nivel")+
  scale_fill_distiller(palette = "Spectral")+
  theme_void()+
  facet_wrap(~tipo_delito)

radios <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/informacion-censal-por-radio/caba_radios_censales.geojson")

#Poblacion por barrio
barrios <- radios %>% 
  group_by(BARRIO) %>%
  summarise(poblacion_barrio=sum(POBLACION)) %>% 
  clean_names()

#Mapeamos poblacion
ggplot() +
  geom_sf(data = barrios, aes(fill = poblacion_barrio), color = NA) +
  labs(title = "Poblacion por Barrio",
       subtitle="Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad",
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/")+
  scale_fill_viridis_c(alpha = 0.9)

#Delitos por barrio
delitos_barrio <- delitos %>% 
  group_by(barrio) %>%
  summarise(cant_delitos=n())

barrios <- barrios %>% 
  left_join(delitos_barrio)

#Agregamos cantidad de delitos por habitante
barrios <- barrios %>% 
  mutate(delitos_hab=cant_delitos/poblacion_barrio)

ggplot() +
  geom_sf(data = barrios, aes(fill = delitos_hab), color = NA) +
  labs(title = "Delitos por habitante en barrios",
       subtitle="Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad",
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/")+
  scale_fill_viridis_c(alpha = 0.9)


calles2 <- read_sf("C:/Users/20332842324/Desktop/callejero.shp")

#Paso a misma proyección que delitos
calles <- st_transform(calles2,crs = 4326)

#Nos quedamos con los delitos de caba
delitos_caba <- delitos_geo[st_within(delitos_geo, barrios) %>% lengths > 0,]

#Chequeo coordenadas
st_crs(calles)
st_crs(delitos_caba)

ggplot()+
  geom_sf(data = calles[200:230,], alpha = 0)+
  labs(title = "Uso comercial por cada cuadra",
       subtitle="Ciudad Autónoma de Buenos Aires",
       color = "Porcentaje (%)",
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/")

#Unimos la calle mas cercana al delito
calles_delitos <- st_join(delitos_caba, calles, join = st_nearest_feature)

View(head(calles_delitos))

#cantidad de delitos por calle
calles_delitos_q <- calles_delitos %>% 
  group_by(id.y, nomoficial) %>%
  summarise(cant_delitos=n())

st_geometry(calles_delitos_q) <- NULL

#Renombramos para poder hacer el join
calles_delitos_q <- calles_delitos_q %>% 
  rename(id=id.y)

calles <- left_join(calles,calles_delitos_q)

ggplot()+
  geom_histogram(data=calles,aes(x=cant_delitos))

calles <- calles %>% 
  mutate(cant_delitos_log=log(cant_delitos))

ggplot()+
  geom_sf(data = calles, aes(color=cant_delitos_log), size=0.7)+
  labs(title = "Cantidad de delitos por calle",
       subtitle="Ciudad Autónoma de Buenos Aires",
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/")+
  scale_colour_viridis_c(option="inferno",guide=FALSE)+
  dark_theme_void()

ggplot()+
  geom_histogram(data=calles,aes(x=cant_delitos_log))


calles_delitos_q_unique <- calles_delitos_q %>% 
  group_by(nomoficial) %>% 
  summarise(total_delitos=sum(cant_delitos)) %>% 
  arrange(desc(total_delitos))


ggplot(data=calles_delitos_q_unique[1:30,],
       aes(x=reorder(nomoficial, total_delitos),
           y=total_delitos,
           fill=factor(nomoficial)))+
  geom_bar(width=0.75,
           stat='identity',
           position='stack')+
  geom_text(size=3.5, color="black",aes(x = nomoficial, 
                         y = total_delitos + 400, label = total_delitos))+
  coord_flip() +
  labs(title = "30 calles con mayor cantidad de delitos",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2017",
       x = "Calle",
       y = "Cantidad")+
  guides(fill=FALSE)


geom_text(aes(label = y), position = position_nudge(y = -0.1))


View(head(calles))

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


# https://github.com/LKremer/ggpointdensity
# https://datanerdses.org/2019/08/29/resumen-del-encuentro-data-nerds-es-ago-2019-ppts-codigo-r/
# http://rpubs.com/angiescetta/actividad-comercial