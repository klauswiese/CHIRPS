#Código para extraer a nivel de polígonos información de CHIRPS
#K. Wiese Julio 2023

# 0. Librerias ----
pacman::p_load(tidyverse,
               ggtext,
               ggimage,
               patchwork,
               showtext,
               sf, 
               SPEI,
               ggthemes,
               magick,
               RColorBrewer,
               ggthemes,
               lwgeom,
               raster,
               rasterVis,
               exactextractr
)

# 0.1 Crear directorios para guardar resultados
if(!dir.exists("Media")) {dir.create("Media")}
if(!dir.exists("MediaAnual")) {dir.create("MediaAnual")}
if(!dir.exists("ResultadosPNG")) {dir.create("ResultadosPNG")}

# 1. Data ----
# 1.1 Precipitaci?n ----
CHIRPS <- stack("ChirpsOriginal/chirps-v2.0.monthly.nc")
CHIRPS_CO <- stack("data/CHIRPS_CO.TIFF")
names(CHIRPS_CO) <- names(CHIRPS)

# 1.2 Departamentos COlombia ----
CO <- read_sf("GPKG/ColombiaAdmin1.GPKG")

#Estaditicos 
############

#Crear vectores con meses 
listaMeses <- list()
for(i in 1:12){
  
  listaMeses[[i]] <- seq(from=i, 
                         to=length(names(CHIRPS_CO)), 
                         by=12)
}

#Media mensual de precipitación
for(i in 1:12){
  media <- function(x){mean(x, na.rm=TRUE)}
  Prec <- calc(CHIRPS_CO[[listaMeses[[i]]]], 
              fun = media)
  if(i <=9){
    writeRaster(Prec, 
                paste0("Media/0", i ,"_Precipitacion_Media.tiff"), 
                overwrite=TRUE)  
  } else writeRaster(Prec, 
                     paste0("Media/", i ,"_Precipitacion_Media.tiff"), 
                     overwrite=TRUE)
}

#Crear vectores por años
listaAños <- list()
for(i in 1:42){
  
  listaAños[[i]] <-  grep(i+1980, 
                          names(CHIRPS_CO))
  
}

#Cálculo de media anual
for(i in 1:length(listaAños)){
  suma <- function(x){sum(x, na.rm=TRUE)}
  PrecipitacionAnual <- calc(CHIRPS_CO[[listaAños[[i]]]], 
              fun = suma)
    writeRaster(PrecipitacionAnual, 
                paste0("MediaAnual/", 1980 + i ,"_Precipitacion_.tiff"), 
                overwrite=TRUE)  
}

#Mapas anuales
#1980
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE, pattern = "198") 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año", 1981:1989, sep="_")
p1980 <- levelplot(CO_Prec, 
                 par.settings = RdBuTheme, 
                 margin = list(FUN = median), 
                 xlab="Longitud",
                 ylab="Latitud", 
                 contour=FALSE,
                 main="Precipitación Acumulada Anual \n 1981-1989 CHIRPS"
)

#guardar imagen
png("ResultadosPNG/Precipitacion_Decada80s.png", width = 1000, height = 1000)
p1980
dev.off()

#1990
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE, pattern = "199") 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año", 1990:1999, sep="_")
p1990 <- levelplot(CO_Prec, 
                   par.settings = RdBuTheme, 
                   margin = list(FUN = median), 
                   xlab="Longitud",
                   ylab="Latitud", 
                   contour=FALSE,
                   main="Precipitación Acumulada Anual \n 1990-1999 CHIRPS"
)

#dir.create("PrecipitacionResultados")
png("ResultadosPNG/Precipitacion_Decada90s.png", width = 1000, height = 1000)
p1990
dev.off()

#2000
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE, pattern = "200") 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año", 2000:2009, sep="_")
p2000 <- levelplot(CO_Prec, 
                   par.settings = RdBuTheme, 
                   margin = list(FUN = median), 
                   xlab="Longitud",
                   ylab="Latitud", 
                   contour=FALSE,
                   main="Precipitación Acumulada Anual \n 2000-2009 CHIRPS"
)

#Guardar Imagen
png("ResultadosPNG/Precipitacion_Decada2000s.png", width = 1000, height = 1000)
p2000
dev.off()

#2000
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE, pattern = "201") 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año", 2010:2019, sep="_")
p2010 <- levelplot(CO_Prec, 
                   par.settings = RdBuTheme, 
                   margin = list(FUN = median), 
                   xlab="Longitud",
                   ylab="Latitud", 
                   contour=FALSE,
                   main="Precipitación Acumulada Anual \n 2010-2019 CHIRPS"
)

#Guardar resultados
png("ResultadosPNG/Precipitacion_Decada2010s.png", width = 1000, height = 1000)
p2010
dev.off()

#2020
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE, pattern = "202") 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año", 2020:2022, sep="_")
p2020 <- levelplot(CO_Prec, 
                   par.settings = RdBuTheme, 
                   margin = list(FUN = median), 
                   xlab="Longitud",
                   ylab="Latitud", 
                   contour=FALSE,
                   main="Precipitación Acumulada Anual \n 2020-2022 CHIRPS"
)

#Guardar Resultados
png("ResultadosPNG/Precipitacion_Decada2020s.png", width = 1000, height = 1000)
p2020
dev.off()

#Todos los años
PrecipitacionMediaAnual <- list.files("MediaAnual", full.names = TRUE) 
CO_Prec <- stack(PrecipitacionMediaAnual) %>%
  mask(CO)
names(CO_Prec) <- paste("Año_",1981:2022, sep="")
precipitacionCO <- levelplot(CO_Prec, 
                   par.settings = RdBuTheme, 
                   margin = list(FUN = median), 
                   xlab="Longitud",
                   ylab="Latitud", 
                   contour=FALSE,
                   main="Precipitación Media Anual \n 1981-2022 CHIRPS"
)

#Guardar gráfico
png("ResultadosPNG/Precipitacion_Anual.png", width = 1000, height = 1000)
precipitacionCO
dev.off()


#Capa media anual 
media <- function(x){mean(x, na.rm=TRUE)}
CO_Prec_Media <- stack(PrecipitacionMediaAnual) %>%
  mask(CO) %>% 
  calc(fun = media)


precipitacionCO_MediaAnual <- levelplot(CO_Prec_Media, 
                              par.settings = RdBuTheme, 
                              margin = list(FUN = median), 
                              xlab="Longitud",
                              ylab="Latitud", 
                              contour=TRUE,
                              main="Precipitación Media Anual \n 1981-2022 CHIRPS"
)

png("ResultadosPNG/Precipitacion_Anual_2.png", width = 1000, height = 1000)
precipitacionCO_MediaAnual
dev.off()

#Extract por Departamento
CO$PCPM <- exact_extract(CO_Prec_Media, 
                             CO, 
                             'weighted_mean', 
                             weights = area(CO_Prec_Media))
st_write(CO, "Precipitacion_Colombia_Departamentos.GPKG")


#Graficar resulatdos
# Import fonts ----
font_add_google(name = "Montserrat", family = "Montserrat")
font_add_google(name = "Open Sans", family = "Open Sans")
showtext_auto()


#Colors and typesetting
#Now build the plot without annotations and try to match the theme.
titulo <- "<span style='font-size:35pt;color:#19233E;'>**Precipitación Media** </span>" 

subtitulo <- "<span style='font-size:25pt;color:#19233E;'> Basado en CHIRPS </span>" 

caption <- "<span style='font-size:20pt;color:#19233E;'>Por: IM </span>"

#Construir mapa
Precipitacion <- ggplot(CO) + 
  geom_sf(aes(fill=-PCPM)) +
  labs(title = titulo,
       subtitle = subtitulo,
       caption = caption) +
  theme_map(base_family = "Montserrat", 
            base_size = 12) +
  theme(
    plot.title = element_markdown(hjust = 0.5, 
                                  face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 1, 
                                    color = "#19233E", 
                                    size = 12),
    panel.background =  element_rect(fill = "white", 
                                     colour = "white"),
    plot.background = element_rect(fill = "white", 
                                   colour = "white")
  )

Precipitacion

#Guardar mapa
ggsave(Precipitacion,
       filename = "ResultadosPNG/Precipitacion_Deptos.png",
       height = 1200,
       width = 900,
       units = "px")
