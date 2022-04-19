
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(readr, leaflegend, gstat, mapview, leaflet.extras, rgdal, leaflet, fs, ggspatial, ggthemes, RColorBrewer, raster, sf, spatstat, maptools, rgeos, tmap, gstat, sp, rpostgis, RPostgreSQL, srt, glue, tidyverse, sf)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

setwd('./Workdirectory') # Setup the path

# Load data ---------------------------------------------------------------
poly <- shapefile('./shp/Badajoz.shp')
pnts <- read_delim("./tbl/temperatura_maxima_media_agosto2.csv", 
           ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)
prvn <- shapefile('./shp/provincias_extremadura.shp')

# Coordinate system
utm30 <- "+init=epsg:25830"
coordinates(pnts) <- ~ X + Y
crs(pnts) <- utm30

# Filtering Badajoz -------------------------------------------------------
prvn$nameunit
prvn$nameunit <- iconv(prvn$nameunit, from = 'UTF-8', to = 'latin1')
prvn <- prvn[prvn@data$nameunit == 'Badajoz',]

# Crop 
pnts <- raster::crop(x = pnts, y = prvn)

tm_shape(prvn) + 
  tm_borders("blue", lwd = .5)


# Create a empty grid
grd0 <- as.data.frame(spsample(pnts, "regular", n=50000))
points(grd0$x1, grd0$x2)
names(grd0) <- c("X", "Y")
coordinates(grd0) <- c("X", "Y")
gridded(grd0)  <- TRUE #crear el objeto SpatialPixel
fullgrid(grd0) <- TRUE 
raster::crs(grd0) <- utm30

# Make the interpolation 
idw <- gstat::idw(temperatura ~ 1, pnts, newdata = grd0, nmax = 8, idp = 2.0)
idw <- raster(idw)
idw <- raster::mask(idw, prvn)

# To make the map
tm_shape(idw) +
  tm_raster(n = 10, palette = rev("RdBu"), title = "Temperatura") +
  tm_shape(pnts) + 
  tm_dots(size = 0.05) +
  tm_layout(main.title = "Datos de temperatura de la provincia de Badajoz",
            main.title.size = 0.95, 
            frame = FALSE) +
  tm_legend(legend.outside = FALSE, text.size = 0.5)+
  tm_shape(prvn) +
  tm_borders("gray", lwd = 4)
