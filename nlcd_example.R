#####################################################################################################
### Example use of USGS NLCD (National Land Cover Database)
### https://www.usgs.gov/centers/eros/science/national-land-cover-database
### ...and use of OpenLand package
### https://cran.r-project.org/web/packages/OpenLand/vignettes/openland_vignette.html
#####################################################################################################
### To get started I downloaded files fromm MRLC Viewer
#####################################################################################################
### also see https://smalltownbigdata.github.io/feb2021-landcover/feb2021-landcover.html
### Working with National Land Cover Database (NLCD) in R: Raster package and treemaps with ggplot2
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
describe("C:/temp/NLCD/NLCD_2001_Land_Cover_L48_20210604_JfbSHT4Blj6NnR7u8Nba.tiff")

#################################################################################################################
nlcd_code <- c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
nlcd_key <- c("Open Water","Developed, Open Space","Developed, Low Intensity","Developed, Medium Intensity",
              "Developed, High Intensity","Barren Land","Deciduous Forest","Evergreen Forest","Mixed Forest",
              "Shrub/Scrub","Herbaceuous","Hay/Pasture","Cultivated Crops","Woody Wetlands","Emergent Herbaceuous Wetlands")
nlcd_key2 <- c("Open Water","Developed, Open","Developed, Low","Developed, Medium",
               "Developed, High","Barren","Deciduous Forest","Evergreen Forest","Mixed Forest",
               "Shrub/Scrub","Herbaceuous","Hay/Pasture","Cultivated","Woody Wetlands","Herbaceuous Wetlands")
### 12 = "Perennial Snow/Ice"
nlcd_rgb <- c( "70 107 159", "222 197 197", "217 146 130", "235 0 0", "171 0 0", "179 172 159", "104 171 95", "28 95 44",
               "181 197 143", "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")
colors <- sapply(strsplit(nlcd_rgb, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))
#################################################################################################################

nlcd_2001_info <- capture.output(
  describe("C:/temp/NLCD/NLCD_2001_Land_Cover_L48_20210604_JfbSHT4Blj6NnR7u8Nba.tiff")
)
# ESPG:4326
# using terra package
nlcd_2001 <- rast("C:/temp/NLCD/NLCD_2001_Land_Cover_L48_20210604_JfbSHT4Blj6NnR7u8Nba.tiff") %>%
  crop(lw_bbox)
# using raster package
# nlcd_1985 <- raster("data/raw/nlcd_CU_1985_V13_LCPRI.tiff") %>%
#  crop(lw_bbox)
nlcd_2001
nlyr(nlcd_2001)

nlcd_2001_df <- as.data.frame(nlcd_2001, xy = TRUE)

ggplot() +
  geom_raster(data = nlcd_2001_df, aes(x = x, y = y, fill = Layer_1)) +
  scale_fill_discrete(na.value = 'deeppink') +
  coord_quickmap()

ggplot() +
  geom_histogram(stat = 'count', data = nlcd_2001_df, aes(Layer_1))

#####################################################################################

#####################################################################################

#################################################################################################################
### This is the OpenLand example. This package provides for a much more detailed analysis of the changes over time.
### ...this is also the part that would benefit from conversion to use of terra
#################################################################################################################
tiffs <- list.files(path = "C:/temp/NLCD/", pattern = 'Land_Cover_L48_20210604_JfbSHT4Blj6NnR7u8Nba.tiff$', full.names=T)
tiffs2 <- list.files(path = "C:/temp/NLCD/", pattern = 'Land_Cover_L48_20230630_JfbSHT4Blj6NnR7u8Nba.tiff$', full.names=T)
tiffs <- c(tiffs,tiffs2)
###
### 9 rasters with names = Layer_1
Years <- c(2001,2004,2006,2008,2011,2013,2016,2019,2021)
#################################################################################################################

nlcd <- raster::stack(tiffs)
# nlcd <- tiffs %>% raster(.) %>% crop_and_mask(tiffs,st_transform(lw_catchments,5070)))
nlcd
names(nlcd)
### OpenLand requires names "_Year" with no additional underscores "_"
names(nlcd) <- paste0("NLCD_",Years)
names(nlcd)
nlcd <- crop_and_mask(nlcd,st_transform(lw_catchments,5070))
nlcd 
plot(nlcd)

#################################################################################################################
### can we consolidate some of these categories?

nlcd[ nlcd %in% c(21,22,23,24) ] <- 25 # Developed
nlcd[ nlcd %in% c(41,42,43) ] <- 45 # Forested

#################################################################################################################
# nlcd_code <- c(11,21,25,31,45,52,71,81,82,90,95)
nlcd_code <- c(11,25,31,45,52,71,81,82,90,95)
nlcd_key <- c("Open Water","Developed, Open Space","Developed","Barren Land","Forested",
              "Shrub/Scrub","Herbaceuous","Hay/Pasture","Cultivated Crops","Woody Wetlands","Emergent Herbaceuous Wetlands")
nlcd_key <- c("Open Water","Developed","Barren Land","Forested",
              "Shrub/Scrub","Herbaceuous","Hay/Pasture","Cultivated Crops","Woody Wetlands","Emergent Herbaceuous Wetlands")
### 12 = "Perennial Snow/Ice"
nlcd_rgb <- c( "70 107 159", "222 197 197", "171 0 0", "179 172 159", "28 95 44",
               "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")
nlcd_rgb <- c( "70 107 159", "171 0 0", "179 172 159", "28 95 44",
               "204 184 121", "223 223 194", "220 217 57", "171 108 40", "184 217 235", "108 159 184")

colors <- sapply(strsplit(nlcd_rgb, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))
#################################################################################################################

#################################################################################################################

#################################################################################################################
### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
nlcd_2001_2021 <- contingencyTable(input_raster = nlcd, pixelresolution = 30)
nlcd_2001_2021

## editing the category name (if necessary)
nlcd_2001_2021$tb_legend$categoryName <- factor(nlcd_key,
                                                 levels = nlcd_key)

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
# nlcd_2001_2021$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
#                                      "#FFAEB9", "#68228B", "#636363")
nlcd_2001_2021$tb_legend$color <- colors
nlcd_2001_2021$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = nlcd_2001_2021,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the nlcd dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',wa_basin,'_nlcd_intensity_2001_2021','.png'))

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

netgrossplot(dataset = nlcd_2001_2021$lulc_Multistep,
             legendtable = nlcd_2001_2021$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',wa_basin,'_nlcd_netgrossplot_2001_2021','.png'))

chordDiagramLand(dataset = nlcd_2001_2021$lulc_Onestep,
                 legendtable = nlcd_2001_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_nlcd_chorddiagram_2001_2021','.png'))

sankeyLand(dataset = nlcd_2001_2021$lulc_Multistep,
           legendtable = nlcd_2001_2021$tb_legend)

sankeyLand(dataset = nlcd_2001_2021$lulc_Onestep,
           legendtable = nlcd_2001_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',wa_basin,'_sankeyonestep_nlcd_2001_2021','.png'))

barplotLand(dataset = nlcd_2001_2021$lulc_Multistep, 
            legendtable = nlcd_2001_2021$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',wa_basin,'_barplot_nlcd_2001_2021','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- nlcd_2001_2021$lulc_Multistep

legendtable = nlcd_2001_2021$tb_legend
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

ggplot(datannual %>% filter(lulc %in% c("Developed","Forested")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(2001,2021,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',wa_basin,'_nlcd_lcc_timeseries_2001_2021','.png'),scale = 1.2)

## to pair with Sammamish analysis
wa_datannual <- datannual

#################################################################################################################
### Add Sammamish basin analysis using OpenLand
### 
#################################################################################################################
nlcd <- crop_and_mask(nlcd,st_transform(sammamish_bsn,5070))
nlcd 
plot(nlcd)

### After data extraction contingencyTable() saves multiple grid information in tables for the next processing steps. 
### The function returns 5 objects: lulc_Multistep, lulc_Onestep, tb_legend, totalArea, totalInterval.
nlcd_2001_2021 <- contingencyTable(input_raster = nlcd, pixelresolution = 30)
nlcd_2001_2021
### no Ice/Snow in Sammamish basin

## editing the category name (if necessary)
nlcd_2001_2021$tb_legend$categoryName <- factor(nlcd_key,
                                                 levels = nlcd_key)

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
# nlcd_2001_2021$tb_legend$color <- c("#EE6363","#FFE4B5","#CAFF70","#228B22", "#436EEE", 
#                                      "#FFAEB9", "#636363")
nlcd_2001_2021$tb_legend$color <- colors
nlcd_2001_2021$tb_legend

### Intensity Analysis (IA) is a quantitative method to analyze LUC maps at several time steps, using cross-tabulation matrices, 
### where each matrix summarizes the LUC change at each time interval. 
### IA evaluates in three levels the deviation between observed change intensity and hypothesized uniform change intensity.
testSL <- intensityAnalysis(dataset = nlcd_2001_2021,
                            category_n = "Developed", category_m = "Tree Cover")
names(testSL)

### Example plot tools...some of these need work because of the large number of years in the nlcd dataset
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
# I don't understand why this isn't working...only saves portion of image
# ggsave(paste0('figs/',sam_basin,'_nlcd_intensity_2001_2021','.png'))

netgrossplot(dataset = nlcd_2001_2021$lulc_Multistep,
             legendtable = nlcd_2001_2021$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)
ggsave(paste0('figs/',sam_basin,'_nlcd_netgrossplot_2001_2021','.png'))

chordDiagramLand(dataset = nlcd_2001_2021$lulc_Onestep,
                 legendtable = nlcd_2001_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_nlcd_chorddiagram_2001_2021','.png'))

sankeyLand(dataset = nlcd_2001_2021$lulc_Multistep,
           legendtable = nlcd_2001_2021$tb_legend)

sankeyLand(dataset = nlcd_2001_2021$lulc_Onestep,
           legendtable = nlcd_2001_2021$tb_legend)
# this also doesn't work...saves previous plot because image is in Viewer rather than Plots
# ggsave(paste0('figs/',sam_basin,'_sankeyonestep_2001_2021','.png'))

barplotLand(dataset = nlcd_2001_2021$lulc_Multistep, 
            legendtable = nlcd_2001_2021$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)
ggsave(paste0('figs/',sam_basin,'_barplot_2001_2021','.png'))

#################################################################################################################
### Here, I extracted the code from barplotLand [type barplotLand at the command line...without the following "()"]
### 
#################################################################################################################

dataset <- nlcd_2001_2021$lulc_Multistep

legendtable = nlcd_2001_2021$tb_legend
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

ggplot(datannual %>% filter(lulc %in% c("Developed","Forested")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(2001,2021,2)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('figs/',sam_basin,'_nlcd_lcc_timeseries_2001_2021','.png'),scale = 1.2)

##################################################################################################
### pair this last timeseries plot with the one for Washington basin
##################################################################################################
library(patchwork)

p1 <- ggplot(wa_datannual %>% filter(lulc %in% c("Developed","Forested")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(2001,2021,5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- ggplot(datannual %>% filter(lulc %in% c("Developed","Forested")),aes(Year,area,color=lulc)) +
  geom_line(linewidth=1.2) +
  facet_wrap(~basin) +
  scale_color_manual(values = c("#28E2E5", "#CD0BBC")) + 
  labs(color = "LUC Categories") + xlab("Year") + ylab(bquote("Area (" ~ km^2~ ")")) + 
  ggtitle(NULL) + 
  scale_x_continuous(breaks = seq(2001,2021,5)) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

plot(p1+p2)

ggsave(paste0('figs/','wa_samm_nlcd_lcc_timeseries_2001_2021','.png'),scale = 1)
