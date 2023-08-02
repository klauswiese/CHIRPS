#Cálculo SPI a partir de CHIRPS
#Código para calcular SPI usando como entrada los dato mensuales de CHIRPS

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
               ncdf4
)

# 0.1 Crear directorios para guardar resultados
if(!dir.exists("data")) {dir.create("data")}
if (!dir.exists("Resultados")) {dir.create("Resultados")}

# 1. Data ----
# 1.1 Precipitación ----
#Poner archivo de datos original CHIRPS https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc

#######################################################################################################################################
#MODULO PARA DESCARGAR CHIRPS DESDE R
#Dependiendo de su conexión el proceso puede durar entre 30 minutos y dos horas. el archivo tiene 6Gb
#Quitar comment para ejecutarlo si aún no se cuenta cn el archivo
#Descargar desde R
##Definir directorio de descarga
#archivodestino <- "ChirpsOriginal/chirps-v2.0.monthly_Julio28.nc"
##Descargar chirps, versión más reciente
# download.file("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc",
#              archivodestino)
#######################################################################################################################################

# 1.2 Polígono de corte, delimita el área de trabajo
CO <- read_sf("GPKG/ColombiaAdmin1.GPKG") %>%
  st_transform(4326)

# Chirps
CHIRPS <- stack("ChirpsOriginal/chirps-v2.0.monthly.nc")

# 2. Procesamiento de datos ----
# 2.1 Crop CHIRPS para polígono de interés ----
CHIRPS_CO <- crop(CHIRPS, CO) %>%
             mask(CO)

#Guardar resulatdo
writeRaster(CHIRPS_CO, 
            "data/CHIRPS_CO.TIFF", 
            overwrite=TRUE)

# 2.2 Cálculo del SPI ----

# 2.2.1 Función para el cálculo espacial del SPI-3 ----
funSPI3 <- function(x, na.rm=TRUE, ...) as.numeric((spi(x, 
                                                        scale=3, #Ajusta meses de retardo para calculo de spi
                                                        na.rm=na.rm, ...))$fitted)
SPI3 <- calc(CHIRPS_CO, 
             fun = funSPI3)

# 2.2.2 Guardar resultados SPI-3 ---- 
writeRaster(SPI3, paste("Resultados/CHIRPS_SPI3_CO_", 
                        today(),
                        ".TIFF"))

#Cargar resultados de SPI
CO_SPI <- stack(imagen[[1]])

plot(CO_SPI)
