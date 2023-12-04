#####################################################################################################
### Example use of USGS LCMAP (Land Change Monitoring, Assessment, and Projection) data 
### https://www.usgs.gov/special-topics/lcmap
### ...and use of OpenLand package
### https://cran.r-project.org/web/packages/OpenLand/vignettes/openland_vignette.html
#####################################################################################################
### To get started I had downloaded a clipped portion of the CONUS dataset (LCPRI) using the LCMAP Viewer
### https://eros.usgs.gov/lcmap/viewer/index.html
#####################################################################################################
### aim to remove need for raster package and convert all code to compatibility with the terra package
### another goal is to better understand the coordinate projections
### plenty of work that could be done to make this all more flexible, readable, and streamlined
#####################################################################################################

library(sf)
library(dplyr)
library(raster)
library(terra)
library(OpenLand)
library(ggplot2)

# crop and mask function
crop_and_mask<-function(raster,polygon) {raster::mask(crop(raster,polygon),polygon)}

# path to internal KC GIS files
kc_gis<-'//kc.kingcounty.lcl/dnrp/GIS/kclib/'

#####################################################################################################

# lake washington catchments generalized to basin
# ...not sure why Clark transformed to same projection (EPSG:2926 -  NAD83(HARN) / Washington North (ftUS))
lw_catchments<-st_read(dsn=paste0(kc_gis,'Plibrary2/hydro/shapes/polygon/topo_catchment.shp')) %>%
  filter(WRIA_NO==8&BASIN_NAME!='Lake Union'&WTRSHD_NAM!='Central Puget Sound') %>%
  #mutate(BASIN_NAME=ifelse(WTRSHD_NAM=='Sammamish River','Sammamish River',as.character(BASIN_NAME))) %>%
  group_by(BASIN_NAME) %>%
  summarise(Area.Acres=sum(Shape_area)/43560) %>%
  st_transform(2926) 

lw_bbox<-st_bbox(st_transform(lw_catchments,5070))[c(1,3,2,4)] # EPSG:5070 Albers Equal Area projection for the US
# plot(lw_catchments['Area.Acres'],axes=T)

##################################################################################################################
washington_bsn <- lw_catchments %>% 
  filter(!BASIN_NAME %in% c("Water - Lake Washington","Water - Lake Sammamish")) %>% 
  mutate(Area.Acres = sum(Area.Acres)) %>% 
  st_union() %>% st_sf() %>% mutate(BASIN_NAME = "Washington", Area.Acres = 357666.5)
# quick check to see if I can easily convert this to work on just the Sammamish basin
# plot(lw_catchments['Area.Acres'],axes=T)
wa_basin <- "Washington"
#################################################################################################################

##################################################################################################################
#### uncomment to perform this analysis on the Lake Sammamish basin
sammamish_bsn <- lw_catchments %>%
  filter(BASIN_NAME %in% c("Issaquah Creek","Tibbetts Creek","West Lake Sammamish","East Lake Sammamish")) %>% 
  mutate(Area.Acres = sum(Area.Acres)) %>% 
  st_union() %>% st_sf() %>% mutate(BASIN_NAME = "Sammamish", Area.Acres = 57860)
# quick check to see if I can easily convert this to work on just the Sammamish basin
# plot(sammamish_bsn['Area.Acres'],axes=T)
sam_basin <- 'Sammamish'
#################################################################################################################

#################################################################################################################
### Explore LCMAP data...using 1985 as an example
### ...here using terra package tools
#################################################################################################################
describe("data/raw/LCMAP_CU_1985_V13_LCPRI.tiff")

#################################################################################################################
### Raster codes 1-8
### 1 = Developed, 2 = Cropland, 3 = Grass/Shrub, 4 = Tree Cover, 5 = Water, 6 = Wetland, 7 = Ice/Snow, 8 = Barren
#################################################################################################################

lcmap_1985_info <- capture.output(
  describe("data/raw/LCMAP_CU_1985_V13_LCPRI.tiff")
)
# ESPG:4326
# using terra package
lcmap_1985 <- rast("data/raw/LCMAP_CU_1985_V13_LCPRI.tiff") %>%
  crop(lw_bbox)
# using raster package
# lcmap_1985 <- raster("data/raw/LCMAP_CU_1985_V13_LCPRI.tiff") %>%
#  crop(lw_bbox)
lcmap_1985
nlyr(lcmap_1985)

lcmap_1985_df <- as.data.frame(lcmap_1985, xy = TRUE)

ggplot() +
  geom_raster(data = lcmap_1985_df, aes(x = x, y = y, fill = LCMAP_CU_1985_V13_LCPRI)) +
  scale_fill_viridis_c(na.value = 'deeppink') +
  coord_quickmap()

ggplot() +
  geom_histogram(data = lcmap_1985_df, aes(LCMAP_CU_1985_V13_LCPRI))

#################################################################################################################
### This is the part that would benefit from conversion to use of terra
### Just do a simple assessment of change between the beginning and ending years (1985 vs 2021)
#################################################################################################################
lcmap_1985 <- rast("data/raw/LCMAP_CU_1985_V13_LCPRI.tiff") %>%
  crop(lw_bbox)
lcmap_2021 <- rast("data/raw/LCMAP_CU_2021_V13_LCPRI.tiff") %>%
  crop(lw_bbox)
lcmap<-rast(list(lcmap_1985,lcmap_2021))
# lcmap <- stack(lcmap_1985,lcmap_2021)
lcmap <- crop_and_mask(lcmap,st_transform(washington_bsn,5070))
# not sure why this warning...CRS do not match...not absolutely sure of LCMAP projection

#####################################################################################
lcmap_change_85_21<-(lcmap$LCMAP_CU_1985_V13_LCPRI*10+lcmap$LCMAP_CU_2021_V13_LCPRI)
plot(lcmap_change_85_21)
unique(lcmap_change_85_21$LCMAP_CU_1985_V13_LCPRI)
# 51 unique changes for Lake Washington

### it seems like there should be a more elegant way to to do this, but here I'm reclassifying the change codes into a 
### smaller number of categories

lcmap_change_85_21[ lcmap_change_85_21 == 11 ] <- 1 # Developed no change 
lcmap_change_85_21[ lcmap_change_85_21 == 22 ] <- 2 # Cropland no change
lcmap_change_85_21[ lcmap_change_85_21 == 33 ] <- 3 # Grass/Shrub no change
lcmap_change_85_21[ lcmap_change_85_21 == 44 ] <- 4 # Tree Cover no change
lcmap_change_85_21[ lcmap_change_85_21 == 55 ] <- 5 # Water no change
lcmap_change_85_21[ lcmap_change_85_21 == 66 ] <- 6 # Wetland no change
lcmap_change_85_21[ lcmap_change_85_21 == 88 ] <- 8 # Barren no change
lcmap_change_85_21[ lcmap_change_85_21 %in% c(21,31,41,61,81) ] <- 9 # Change to Developed
lcmap_change_85_21[ lcmap_change_85_21 %in% c(41,42,43,46,48) ] <- 10 # Change from Tree Cover
lcmap_change_85_21[ lcmap_change_85_21 > 10 ] <- 11 # other changes # all other
unique(lcmap_change_85_21$LCMAP_CU_1985_V13_LCPRI)
# not 10 change categories

lcmap_change_key<-read.table(sep=',',header=T,text="
          Description,Code,Key,Color
           'Developed no change',1,1,gray
           'Cropland no change',2,2,darkseagreen1
           'Grass/Shrub no change',3,3,darkgoldenrod1
           'Tree Cover no change',4,4,forestgreen
           'Water',5,5,blue
           'Wetland no change',6,6,pink
           'Barren no change',8,8,darkred
           'Developed gain',9,9,red
           'Tree Cover loss',10,10,yellow
           'Other',11,11,darkorange")
lcmap_change_key$Description <- stringr::str_trim(lcmap_change_key$Description, "left")
key_colors<-as.character(lcmap_change_key$Color)
names(key_colors)<-lcmap_change_key$Description

# I'm not getting something about the projections...but this is working
lcmap_change_85_21 %>% 
  # crop_and_mask(.,st_transform(lw_catchments,5070)) %>%
  # projectRaster(.,crs=projection(lw_catchments),method='ngb') %>%
  # project(., "EPSG:4326") %>% 
  project(., "EPSG:2285") %>% 
  # project(., crs(lw_catchments)) %>% 
  as.data.frame(.,xy=T,na.rm=T) %>%
  merge(lcmap_change_key,by.x='LCMAP_CU_1985_V13_LCPRI',by.y='Code') %>%
  ggplot() +
  geom_raster(aes(x=x,y=y,fill=Description)) +
  scale_fill_manual(values = key_colors) +
  geom_sf(data=st_geometry(washington_bsn),fill=NA) +
  geom_sf(data=st_geometry(sammamish_bsn),fill=NA, lwd = 1.0) +
  theme_bw() +
  theme(panel.grid = element_line(colour=NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend.position = c(.75,.75))
        legend.position = 'bottom')
ggsave(paste0('figs/',wa_basin,'_lcmap_change_1985_2021','.png'), scale = 2)

####### for testing of the above
tst <-lcmap_change_85_21 %>% 
  # crop_and_mask(.,st_transform(lw_catchments,5070)) %>%
  # projectRaster(.,crs=projection(lw_catchments),method='ngb') %>%
  # project(., "EPSG:4326") %>% 
  project(., crs(lw_catchments)) %>% 
  as.data.frame(.,xy=T,na.rm=T) %>%
  merge(lcmap_change_key,by.x='LCMAP_CU_1985_V13_LCPRI',by.y='Code')
##################################

#################################################################################################################
### This is the OpenLand example. This package provides for a much more detailed analysis of the changes over time.
### ...this is also the part that would benefit from conversion to use of terra
#################################################################################################################

tiffs <- list.files(path = "data/raw/", pattern = '.tiff$', full.names=T)
lcmap <- raster::stack(tiffs)
# lcmap <- tiffs %>% raster(.) %>% crop_and_mask(tiffs,st_transform(lw_catchments,5070)))
lcmap
names(lcmap)
### OpenLand requires names "_Year" with no additional underscores "_"
names(lcmap) <- substr(names(lcmap), 7, 13)
names(lcmap)
lcmap <- crop_and_mask(lcmap,st_transform(lw_catchments,5070))
lcmap 
plot(lcmap)

### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
lcmap_1985_2021 <- contingencyTable(input_raster = lcmap, pixelresolution = 30)
lcmap_1985_2021

## editing the category name (if necessary)
lcmap_1985_2021$tb_legend$categoryName <- factor(c("Developed", "Cropland", "Grass/Shrub", 
                                                   "Tree Cover", "Water", "Wetland", "Ice/Snow", "Barren"),
                                                 levels = c("Developed", "Cropland", "Grass/Shrub", 
                                                            "Tree Cover", "Water", "Wetland", "Ice/Snow", "Barren"))

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
lcmap_1985_2021$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
                                     "#FFAEB9", "#68228B", "#636363")
lcmap_1985_2021$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = lcmap_1985_2021,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the LCMAP dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',wa_basin,'_lcmap_intensity_1985_2021','.png'))

plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Ap (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Ap (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of SG (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of SG (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 1/10, y = 5/10))

netgrossplot(dataset = lcmap_1985_2021$lulc_Multistep,
             legendtable = lcmap_1985_2021$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',wa_basin,'_lcmap_netgrossplot_1985_2021','.png'))

chordDiagramLand(dataset = lcmap_1985_2021$lulc_Onestep,
                 legendtable = lcmap_1985_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_lcmap_chorddiagram_1985_2021','.png'))

sankeyLand(dataset = lcmap_1985_2021$lulc_Multistep,
           legendtable = lcmap_1985_2021$tb_legend)

sankeyLand(dataset = lcmap_1985_2021$lulc_Onestep,
           legendtable = lcmap_1985_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_sankeyonestep_1985_2021','.png'))

barplotLand(dataset = lcmap_1985_2021$lulc_Multistep, 
            legendtable = lcmap_1985_2021$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',wa_basin,'_barplot_1985_2021','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- lcmap_1985_2021$lulc_Multistep

legendtable = lcmap_1985_2021$tb_legend
area_km2 = TRUE
datachange <- dataset %>% left_join(legendtable, by = c(From = "categoryValue")) %>% 
  left_join(legendtable, by = c(To = "categoryValue")) %>% 
  dplyr::select(-c(From, To)) %>% rename(From = "categoryName.x", 
                                         To = "categoryName.y", colorFrom = "color.x", colorTo = "color.y")
areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")
datannual <- datachange %>% group_by(yearTo, To) %>% summarise(area = sum(!!as.name(areaif))) %>% 
  rename(Year = "yearTo", lulc = "To") %>% rbind(datachange[datachange$yearFrom == 
                                                              first(datachange$yearFrom), ] %>% group_by(yearFrom, 
                                                                                                         From) %>% summarise(area = sum(!!as.name(areaif))) %>% 
                                                   rename(Year = "yearFrom", lulc = "From"))
datannual$basin <- wa_basin

ggplot(datannual %>% filter(lulc %in% c("Developed","Tree Cover")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1985,2021,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',wa_basin,'_lcmap_lcc_timeseries_1985_2021','.png'),scale = 1.2)

## to pair with Sammamish analysis
wa_datannual <- datannual

#################################################################################################################
### Add Sammamish basin analysis using OpenLand
### 
#################################################################################################################
lcmap <- crop_and_mask(lcmap,st_transform(sammamish_bsn,5070))
lcmap 
plot(lcmap)

### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
lcmap_1985_2021 <- contingencyTable(input_raster = lcmap, pixelresolution = 30)
lcmap_1985_2021
### no Ice/Snow in Sammamish basin

## editing the category name (if necessary)
lcmap_1985_2021$tb_legend$categoryName <- factor(c("Developed", "Cropland", "Grass/Shrub", 
                                                   "Tree Cover", "Water", "Wetland", "Barren"),
                                                 levels = c("Developed", "Cropland", "Grass/Shrub", 
                                                            "Tree Cover", "Water", "Wetland", "Barren"))

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
lcmap_1985_2021$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
                                     "#FFAEB9", "#636363")
lcmap_1985_2021$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = lcmap_1985_2021,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the LCMAP dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',sam_basin,'_lcmap_intensity_1985_2021','.png'))

netgrossplot(dataset = lcmap_1985_2021$lulc_Multistep,
             legendtable = lcmap_1985_2021$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',sam_basin,'_lcmap_netgrossplot_1985_2021','.png'))

chordDiagramLand(dataset = lcmap_1985_2021$lulc_Onestep,
                 legendtable = lcmap_1985_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_lcmap_chorddiagram_1985_2021','.png'))

sankeyLand(dataset = lcmap_1985_2021$lulc_Multistep,
           legendtable = lcmap_1985_2021$tb_legend)

sankeyLand(dataset = lcmap_1985_2021$lulc_Onestep,
           legendtable = lcmap_1985_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_sankeyonestep_1985_2021','.png'))

barplotLand(dataset = lcmap_1985_2021$lulc_Multistep, 
            legendtable = lcmap_1985_2021$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',sam_basin,'_barplot_1985_2021','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- lcmap_1985_2021$lulc_Multistep

legendtable = lcmap_1985_2021$tb_legend
area_km2 = TRUE
datachange <- dataset %>% left_join(legendtable, by = c(From = "categoryValue")) %>% 
  left_join(legendtable, by = c(To = "categoryValue")) %>% 
  dplyr::select(-c(From, To)) %>% rename(From = "categoryName.x", 
                                         To = "categoryName.y", colorFrom = "color.x", colorTo = "color.y")
areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")
datannual <- datachange %>% group_by(yearTo, To) %>% summarise(area = sum(!!as.name(areaif))) %>% 
  rename(Year = "yearTo", lulc = "To") %>% rbind(datachange[datachange$yearFrom == 
                                                              first(datachange$yearFrom), ] %>% group_by(yearFrom, 
                                                                                                         From) %>% summarise(area = sum(!!as.name(areaif))) %>% 
                                                   rename(Year = "yearFrom", lulc = "From"))
datannual$basin <- sam_basin

ggplot(datannual %>% filter(lulc %in% c("Developed","Tree Cover")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1985,2021,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',sam_basin,'_lcmap_lcc_timeseries_1985_2021','.png'),scale = 1.2)

##################################################################################################
### pair this last timeseries plot with the one for Washington basin
##################################################################################################
library(patchwork)

p1 <- ggplot(wa_datannual %>% filter(lulc %in% c("Developed","Tree Cover")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1985,2021,5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- ggplot(datannual %>% filter(lulc %in% c("Developed","Tree Cover")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1985,2021,5)) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

plot(p1+p2)

ggsave(paste0('figs/','wa_samm_lcmap_lcc_timeseries_1985_2021','.png'),scale = 1)
