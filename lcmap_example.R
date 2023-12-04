#####################################################################################################
### Example use of USGS LCMAP (Land Change Monitoring, Assessment, and Projection) data 
### https://www.usgs.gov/special-topics/lcmap
#####################################################################################################
### aim to remove need for raster package and convert all code to compatibility with the terra package
#####################################################################################################

library(sf)
library(dplyr)
library(raster)
library(terra)
library(ggplot2)

# crop and mask function
crop_and_mask<-function(raster,polygon) {raster::mask(crop(raster,polygon),polygon)}

# path to internal KC GIS files
kc_gis<-'//kc.kingcounty.lcl/dnrp/GIS/kclib/'

#####################################################################################################

#lake washington catchments generalized to basin
lw_catchments<-st_read(dsn=paste0(kc_gis,'Plibrary2/hydro/shapes/polygon/topo_catchment.shp')) %>%
  filter(WRIA_NO==8&BASIN_NAME!='Lake Union'&WTRSHD_NAM!='Central Puget Sound') %>%
  #mutate(BASIN_NAME=ifelse(WTRSHD_NAM=='Sammamish River','Sammamish River',as.character(BASIN_NAME))) %>%
  group_by(BASIN_NAME) %>%
  summarise(Area.Acres=sum(Shape_area)/43560) %>%
  st_transform(2926) 

lw_bbox<-st_bbox(st_transform(lw_catchments,5070))[c(1,3,2,4)] #Albers
#plot(lw_catchments['Area.Acres'],axes=T)

##################################################################################################################
washington_bsn <- lw_catchments %>% 
  filter(!BASIN_NAME %in% c("Water - Lake Washington","Water - Lake Sammamish")) %>% 
  mutate(Area.Acres = sum(Area.Acres)) %>% 
  st_union() %>% st_sf() %>% mutate(BASIN_NAME = "Washington", Area.Acres = 357666.5)
# quick check to see if I can easily convert this to work on just the Sammamish basin
lw_catchments <- washington_bsn
#################################################################################################################

##################################################################################################################
sammamish_bsn <- lw_catchments %>% 
  filter(BASIN_NAME %in% c("Issaquah Creek","Tibbetts Creek","West Lake Sammamish","East Lake Sammamish")) %>% 
  mutate(Area.Acres = sum(Area.Acres)) %>% 
  st_union() %>% st_sf() %>% mutate(BASIN_NAME = "Sammamish", Area.Acres = 57860)
# quick check to see if I can easily convert this to work on just the Sammamish basin
lw_catchments <- sammamish_bsn
#################################################################################################################
