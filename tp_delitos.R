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
library(leaflet)
library(lubridate)

delitos <- fread("https://raw.githubusercontent.com/martoalalu/delitos-caba/master/data/delitos.csv")
delitos <- delitos %>% 
  mutate(fecha=ymd(fecha)) %>% 
  mutate(anio=year(fecha)) %>% 
  mutate(anio_mes = floor_date(as_date(fecha), "month"))
  
#Cantidad de delitos por año y porcentaje
delitos %>% 
  count(anio) %>% 
  mutate(pct=(n/sum(n))*100)

#Evolucion de delitos y cantidad por año (barras)
delitos %>% 
  ggplot(aes(anio))+
    geom_bar(aes(fill=as.factor(tipo_delito)))

#Evolución lineas de delitos

delitos %>% 
  count(anio_mes,tipo_delito) %>% 
  ggplot() +
  geom_line(aes(x=anio_mes, y=n, color=tipo_delito))+
  xlab("")
  
  
#https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html




delitos_geo <- delitos %>% 
  filter(!is.na(longitud), !is.na(latitud)) %>%
  filter(longitud != 0) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

colnames(delitos_geo)
unique(delitos_geo$tipo_delito)



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

#Delitos facetados por anio
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
  facet_wrap(~anio)

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


calles <- read_sf("C:/Users/20332842324/Desktop/callejero.shp")

#Paso a misma proyección que delitos
calles <- st_transform(calles,crs = 4326)

#Nos quedamos con los delitos que están dentro de CABA
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
  geom_histogram(data=calles,aes(x=cant_delitos_log))


ggplot()+
  geom_sf(data = calles, aes(color=cant_delitos_log), size=0.7)+
  labs(title = "Cantidad de delitos por calle",
       subtitle="Ciudad Autónoma de Buenos Aires",
       caption= "Fuente de datos: https://mapa.seguridadciudad.gob.ar/")+
  scale_colour_viridis_c(option="inferno",guide=FALSE)+
  dark_theme_void()

binpal <- colorBin("viridis", calles$cant_delitos_log, 6, pretty = FALSE)

leaflet(calles) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~binpal(cant_delitos_log), weight = 1.5, smoothFactor = 0.5, 
               opacity = 1.0, fillOpacity = 0.65,highlightOptions = highlightOptions(color = "white", weight = 2,
                bringToFront = TRUE))


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

View(head(delitos_caba))










# TEST I DE MORAN

caba <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/perimetro/caba-perimetro.geojson")

#Creamos grilla
grilla<-st_make_grid(caba, cellsize = 0.003)


#Vemos como queda
ggplot() + 
  geom_sf(data = caba, fill=NA)+
  geom_sf(data=grilla, fill=NA)

#Convertimos la grilla a objeto sf
grilla <- st_sf(grilla)
grilla <- mutate(grilla, id=seq(1,3960))

grilla_caba <- st_intersection(grilla, caba)

delitos_robo <- delitos_caba %>% 
  filter(tipo_delito =="Robo (Con violencia)")

ggplot()+
  geom_sf(data=grilla_caba)+
  geom_sf(data=delitos_robo)

delitos_robo <- delitos_robo %>%  
  st_join(grilla_caba)

delitos_robo_grilla <- delitos_robo %>% 
  st_set_geometry(NULL) %>% 
  group_by(id.y) %>% 
  summarise(robos = n())


ggplot() +
  geom_histogram(data = delitos_robo_grilla, aes(robos))

grilla_caba <- grilla_caba %>% 
  left_join(delitos_robo_grilla, by=c("id"="id.y"))

grilla_caba <- grilla_caba %>% 
  mutate(robo = ifelse(is.na(robos), 0, robos))

plot(grilla_caba["mueblerias"], border = NA)

ggplot()+
  geom_sf(data=grilla_caba, aes(fill=robo))


library(spdep)
grilla_caba <- st_as_sf(grilla_caba)

sp.matrix <- poly2nb(as(grilla_caba, "Spatial"), queen=TRUE)
summary(sp.matrix)
sp.weights <- nb2listw(sp.matrix, style='W')

moran.mc(grilla_caba$robo, listw = sp.weights, nsim = 999)

#Distancia a comisarias
grilla_caba_c <- st_point_on_surface(grilla_caba)

ggplot()+
  geom_sf(data=grilla_caba_c, aes(color=robo))

comisarias<-read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv")

comisarias <- comisarias %>% 
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot()+
  geom_sf(data=grilla_caba_c, aes(color=robo))+
  geom_sf(data=comisarias, color="red", size=5)

#Distancia centroides delitos a comisarias
comisarias_grilla <- st_join(grilla_caba_c, comisarias, st_nearest_feature)

#Agregamos una columna con la distancia y lo pasamos a numero
comisarias_grilla <- comisarias_grilla %>% 
  mutate(distancia=st_distance(grilla_caba_c, comisarias[st_nearest_feature(grilla_caba_c, comisarias),], by_element = TRUE)) %>% 
  mutate(distancia=as.numeric(distancia))

modelo<- lm(robos~distancia, data=comisarias_grilla)
summary(modelo)


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