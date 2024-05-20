# LULC change example
# https://catalog.data.gov/dataset/modeled-historical-land-use-and-land-cover-for-the-conterminous-united-states-1938-1992

#####################################################################################################
### Example use of USGS LULC change model 1938-1992
### https://www.usgs.gov/special-topics/land-use-land-cover-modeling/land-cover-modeling-methodology-fore-sce-model
### https://catalog.data.gov/dataset/modeled-historical-land-use-and-land-cover-for-the-conterminous-united-states-1938-1992
### https://www.usgs.gov/data/conterminous-united-states-land-cover-projections-1992-2100 
### ...and use of OpenLand package
### https://cran.r-project.org/web/packages/OpenLand/vignettes/openland_vignette.html
#####################################################################################################
### To get started I downloaded files from USGS
#####################################################################################################
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
### Explore USGS LULC change data...using 1985 as an example
### ...here using terra package tools
#################################################################################################################
describe("data/raw/usgs_lulc/backcast/CONUS_Backcasting_y1985.tif")
# proj: EPSG:9822

#################################################################################################################
usgs_code <- c(1,2,6,7,8,9,10,11,12,13,14,15,16)
### 17 = "Perennial Snow/Ice"
usgs_key <- c("Open Water","Urban Developed","Mining","Barren",
              "Deciduous Forest","Evergreen Forest","Mixed Forest",
              "Grassland","Shrubland","Cultivated Crops","Hay/Pasture","Emergent Herbaceuous Wetlands","Woody Wetlands")
usgs_rgb <- c( "70 107 159", "235 0 0", "171 0 0", "179 172 159", "104 171 95", "28 95 44",
               "181 197 143", "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")
colors <- sapply(strsplit(usgs_rgb, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

# "#466B9F" "#EB0000" "#AB0000" "#B3AC9F" "#68AB5F" "#1C5F2C" "#B5C58F" "#CCB879" "#DFDFC2" "#DCD939" "#AB6C28" "#B8D9EB" "#6C9FB8"
#################################################################################################################

usgs_1985_info <- capture.output(
  describe("data/raw/usgs_lulc/backcast/CONUS_Backcasting_y1985.tif")
)

usgs_1985 <- rast("data/raw/usgs_lulc/backcast/CONUS_Backcasting_y1985.tif") %>%
  crop(lw_bbox)
# using raster package
# nlcd_1985 <- raster("data/raw/nlcd_CU_1985_V13_LCPRI.tiff") %>%
#  crop(lw_bbox)
usgs_1985
nlyr(usgs_1985)
st_crs(usgs_1985)

usgs_1985_df <- as.data.frame(usgs_1985, xy = TRUE)

ggplot() +
  geom_raster(data = usgs_1985_df, aes(x = x, y = y, fill = CONUS_Backcasting_y1985)) +
  # scale_fill_discrete(na.value = 'deeppink') +
  theme(aspect.ratio=1) +
  coord_quickmap()

ggplot() +
  geom_histogram(stat = 'count', data = usgs_1985_df, aes(CONUS_Backcasting_y1985))

#####################################################################################

#####################################################################################

#################################################################################################################
### This is the OpenLand example. This package provides for a much more detailed analysis of the changes over time.
### ...this is also the part that would benefit from conversion to use of terra
#################################################################################################################
tiffs <- list.files(path = "data/raw/usgs_lulc/backcast/", pattern = 'CONUS_Backcasting_y', full.names=T)
###
### 9 rasters with names that change (argh!) CONUS_Backcasting_y1938....
Years <- seq(1938,1992,1)
#################################################################################################################

usgs <- raster::stack(tiffs)
# nlcd <- tiffs %>% raster(.) %>% crop_and_mask(tiffs,st_transform(lw_catchments,5070)))
usgs
names(usgs)
### OpenLand requires names "_Year" with no additional underscores "_"
names(usgs) <- paste0("USGS_",Years)
names(usgs)
usgs <- crop_and_mask(usgs,st_transform(lw_catchments,5070))
usgs 
plot(usgs)

# #################################################################################################################
# ### can we consolidate some of these categories?
# 
# nlcd[ nlcd %in% c(21,22,23,24) ] <- 25 # Developed
 usgs[ usgs %in% c(8,9,10) ] <- 10 # Forested
# 
#################################################################################################################
usgs_code <- c(1,2,6,7,10,11,12,13,14,15,16)
### 17 = "Perennial Snow/Ice"
# usgs_key <- c("Open Water","Urban Developed","Mining","Barren",
usgs_key <- c("Open Water","Developed","Mining","Barren",
              "Forest",
              "Grassland","Shrubland","Cultivated Crops","Hay/Pasture","Emergent Herbaceuous Wetlands","Woody Wetlands")
usgs_rgb <- c( "70 107 159", "235 0 0", "171 0 0", "179 172 159", "104 171 95", 
                "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")
colors <- sapply(strsplit(usgs_rgb, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

# "#466B9F" "#EB0000" "#AB0000" "#B3AC9F" "#68AB5F" "#CCB879" "#DFDFC2" "#DCD939" "#AB6C28" "#B8D9EB" "#6C9FB8"
#################################################################################################################

#################################################################################################################

#################################################################################################################
### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
usgs_1938_1992 <- contingencyTable(input_raster = usgs, pixelresolution = 250)
usgs_1938_1992

## editing the category name (if necessary)
usgs_1938_1992$tb_legend$categoryName <- factor(usgs_key,
                                                levels = usgs_key)

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
# nlcd_2001_2021$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
#                                      "#FFAEB9", "#68228B", "#636363")
usgs_1938_1992$tb_legend$color <- colors
usgs_1938_1992$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = usgs_1938_1992,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the usgs dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',wa_basin,'_usgs_intensity_2001_2021','.png'))

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

netgrossplot(dataset = usgs_1938_1992$lulc_Multistep,
             legendtable = usgs_1938_1992$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',wa_basin,'_usgs_netgrossplot_2001_2021','.png'))

chordDiagramLand(dataset = usgs_1938_1992$lulc_Onestep,
                 legendtable = usgs_1938_1992$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_usgs_chorddiagram_2001_2021','.png'))

sankeyLand(dataset = usgs_1938_1992$lulc_Multistep,
           legendtable = usgs_1938_1992$tb_legend)

sankeyLand(dataset = usgs_1938_1992$lulc_Onestep,
           legendtable = usgs_1938_1992$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_sankeyonestep_usgs_2001_2021','.png'))

barplotLand(dataset = usgs_1938_1992$lulc_Multistep, 
            legendtable = usgs_1938_1992$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',wa_basin,'_barplot_usgs_2001_2021','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- usgs_1938_1992$lulc_Multistep

legendtable = usgs_1938_1992$tb_legend
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

ggplot(datannual %>% filter(lulc %in% c("Urban Developed","Forest")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',wa_basin,'_usgs_lcc_timeseries_2001_2021','.png'),scale = 1.2)

## to pair with Sammamish analysis
wa_datannual <- datannual
wa_datannual$period <- 'backcast'

#################################################################################################################
### Add Sammamish basin analysis using OpenLand
### 
#################################################################################################################
#################################################################################################################
usgs_code <- c(1,2,6,10,11,12,13,14,15,16)
### 17 = "Perennial Snow/Ice"
# usgs_key <- c("Open Water","Urban Developed","Mining",
usgs_key <- c("Open Water","Developed","Mining",
              "Forest",
              "Grassland","Shrubland","Cultivated Crops","Hay/Pasture","Emergent Herbaceuous Wetlands","Woody Wetlands")
usgs_rgb <- c( "70 107 159", "235 0 0", "171 0 0", "104 171 95", 
               "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")
colors <- sapply(strsplit(usgs_rgb, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

 "#466B9F" "#EB0000" "#AB0000" "#B3AC9F" "#68AB5F" "#CCB879" "#DFDFC2" "#DCD939" "#AB6C28" "#B8D9EB" "#6C9FB8"
#################################################################################################################
usgs <- crop_and_mask(usgs,st_transform(sammamish_bsn,5070))
usgs
plot(usgs)

usgs_1985_df <- as.data.frame(usgs, xy = TRUE)

ggplot() +
  geom_histogram(stat = 'count', data = usgs_1985_df, aes(USGS_1938))

### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
usgs_1938_1992 <- contingencyTable(input_raster = usgs, pixelresolution = 250)
usgs_1938_1992
### no Ice/Snow in Sammamish basin

## editing the category name (if necessary)
usgs_1938_1992$tb_legend$categoryName <- factor(usgs_key,
                                                levels = usgs_key)

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
# usgs_1938_1992$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
#                                      "#FFAEB9", "#636363")
usgs_1938_1992$tb_legend$color <- colors
usgs_1938_1992$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = usgs_1938_1992,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the usgs dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',sam_basin,'_usgs_intensity_1938_1992','.png'))

netgrossplot(dataset = usgs_1938_1992$lulc_Multistep,
             legendtable = usgs_1938_1992$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',sam_basin,'_usgs_netgrossplot_1938_1992','.png'))

chordDiagramLand(dataset = usgs_1938_1992$lulc_Onestep,
                 legendtable = usgs_1938_1992$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_usgs_chorddiagram_1938_1992','.png'))

sankeyLand(dataset = usgs_1938_1992$lulc_Multistep,
           legendtable = usgs_1938_1992$tb_legend)

sankeyLand(dataset = usgs_1938_1992$lulc_Onestep,
           legendtable = usgs_1938_1992$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_sankeyonestep_1938_1992','.png'))

barplotLand(dataset = usgs_1938_1992$lulc_Multistep, 
            legendtable = usgs_1938_1992$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',sam_basin,'_barplot_1938_1992','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- usgs_1938_1992$lulc_Multistep

legendtable = usgs_1938_1992$tb_legend
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

# ggplot(datannual %>% filter(lulc %in% c("Urban Developed","Forest")),aes(Year,area,color=lulc)) +
ggplot(datannual %>% filter(lulc %in% c("Developed","Forest")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',sam_basin,'_nlcd_lcc_timeseries_1938_1992','.png'),scale = 1.2)

sa_datannual <- datannual
sa_datannual$period <- 'backcast'

data_backcast <- bind_rows(wa_datannual,sa_datannual)

##################################################################################################
### pair this last timeseries plot with the one for Washington basin
##################################################################################################
library(patchwork)

p1 <- ggplot(wa_datannual %>% filter(lulc %in% c("Urban Developed","Forest")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,4)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- ggplot(datannual %>% filter(lulc %in% c("Urban Developed","Forest")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,4)) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

plot(p1+p2)

ggsave(paste0('figs/','wa_samm_usgs_lcc_timeseries_2001_2021','.png'),scale = 1)

p1 <- ggplot(wa_datannual %>% filter(lulc %in% c("Urban Developed","Forest","Cultivated Crops","Hay/Pasture")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#EB0000", "#68AB5F", "#DCD939", "#AB6C28")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,4)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- ggplot(datannual %>% filter(lulc %in% c("Urban Developed","Forest","Cultivated Crops","Hay/Pasture")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#EB0000", "#68AB5F", "#DCD939", "#AB6C28")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(1938,1992,4)) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

plot(p1+p2)

ggsave(paste0('figs/','wa_samm_usgs_lcc_timeseries_2001_2021','.png'),scale = 1)
