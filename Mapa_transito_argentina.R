library(sf)
library(ggplot2)
library(tidyverse)
library(mapdata)
library(maps)
library(dplyr)
library(openxlsx)
library(elevatr)
library(ggnewscale)
municipios <- st_read("SHP/barrio.json")

##st_drivers()
data  <- read.xlsx("Excel/dataset_flujo_vehicular.xlsx", sheet="dataset_flujo_vehicular") 
municipios_xy    <- cbind(municipios , st_coordinates(st_centroid(municipios$geometry)))
ggplot(data = municipios) +
  geom_sf()

elev = get_elev_raster(municipios, z=10)

Poligo_alt    <- crop(elev, municipios)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, municipios)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Mapa =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill() +
  geom_sf(data= municipios, aes(fill=BARRIO ),color="black", show.legend = F, alpha=0.6)+
  scale_fill_viridis_d()+
  geom_point(data =data,  aes(x=LONGITUD, y=LATITUD, color=SENTIDO) ,size=2, alpha=0.5)+
  ggtitle("Mapa del catastro de Flujo Vehicular\nde la Ciudad del Gran Buenos Aires")+
  labs(subtitle="División Administrativa por Barrios", 
       caption="Fuente: @Diego Sipes (Argentina)", 
       color="Tipo",
       x="Longitud",y="Latitud")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  geom_label(data =  municipios_xy  , aes(x= X, y=Y, label = BARRIO), size = 2, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  theme_bw()+
  theme(legend.position = c(.87, .10),
        panel.border = element_rect(size = 2, color="white"),
        panel.background = element_rect(fill = "black"),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "springgreen", family="serif", face = "italic"))+
  annotate(geom = "text", x = -58.38, y = -34.68, hjust = 0, vjust = 1, 
           label = "Ing.Gorky Florez Castillo",size = 3, family="serif", color = "springgreen",  fontface="italic")

ggsave(plot = Mapa  ,"Mapa/Transito.png", units = "cm", 
       width = 21,height = 25, dpi = 1200)

