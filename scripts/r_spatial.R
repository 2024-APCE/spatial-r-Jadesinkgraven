# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("~/Desktop/Msc Ecology & Conservation/APCE 2024/apce2024gis")

# restore the libraries of the project 
renv::restore()

rm(list = ls())
gc()  # Run garbage collection

# terra via github
install.packages("remotes")
remotes::install_github("rspatial/terra")


Sys.setenv(PROJ_LIB = "/opt/homebrew/Cellar/proj/9.5.0/share/proj")

# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
my_colors <- c("red", "white", "blue")
my_colors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))

library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
plot(protected_areas)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)


# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

str(woodybiom)

woodybiom <- raster::raster("./2016_WoodyVegetation/TBA_gam_utm36S.tif") |>  as.data.frame(xy=T)

protected_areas <- raster::raster("./2022_protected_areas/protected_areas.gpkg") |>  as.data.frame(xy=T)

file.exists("/Users/jadesinkgraven/Desktop/Msc Ecology & Conservation/APCE 2024/apce2024gis/2022_protected_areas/protected_areas.gpkg")

st_crs(woodybiom)
st_crs(protected_areas)


ggplot()+
  tidyterra::geom_spatraster(data= woodybiom) + #check in dataframe what fill is
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name = "TBA/ha")  +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA, color = "red", linewidth = 1) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "royalblue", linewidth = 0.75) +
  tidyterra::geom_spatvector(data = lakes,
                             fill = "blue")


# plot the rainfall map
ggplot()+
  geom_raster(data= rainfall, aes(x=x, y=y, fill=CHIRPS_MeanAnnualRainfall))+
  scale_fill_gradientn(colours=rev(viridis::viridis(6)),
                       limits=c(0, 2000),
                       oob=squish,
                       name = "Mean annual rainfall (mm)")+
  ggspatial::scale_bar(location = "bottomright", dist = 100, dist_unit = "km", transform = TRUE)


# plot the elevation map

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png


############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)

# crop the woody biomass to the extent of the studyarea


# plot the woody biomass


# make maps also for the other layers that you found

# create 500 random points in our study area


# and add them to the previous map

# make distance to river map



### put all maps together



# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


