library(data.table)
library(tidyverse)
library(xlsx)
library(readxl)
library(sf)

library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)

setwd("C:/Users/20332842324/Desktop/delitos")

file_list <- list.files(path="C:/Users/20332842324/Desktop/delitos")


dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read_excel(file_list[i], range = cell_cols("A:K")) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Class <- sapply(strsplit(gsub(".xlsx", "", file_list[i]), "_"), function(x){x[2]}) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
}


write.csv(dataset,"C:/Users/20332842324/Desktop/delitos/delitos.csv",fileEncoding = 'UTF-8')

delitos<-dataset

comisarias<-read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv")

comisarias <- comisarias %>% 
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

delitos <- delitos %>% 
  filter(!is.na(LONGITUD), !is.na(LATITUD)) %>%
  filter(LONGITUD != 0) %>% 
  st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = 4326)


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
