library("dplyr")
library("rgdal")
library("rgeos")
library("tmap")
library("ggplot2")

library("sf")
library("sp")

library("RColorBrewer")
# get colour hex codes for greens palatte
brewer.pal(n = 9, name = "Greens")


#Bring in shapefiles
NZAreas <- readOGR("OriginalDataFiles/Area/area-unit-2013.shp")
proj4string(NZAreas)
# plot(NZAreas)

#remove the areas that are offshore
NZAreas@data$AU2013_V1_ <- as.numeric(as.character(NZAreas@data$AU2013_V1_))
NZAreas <- NZAreas[NZAreas@data$AU2013_V1_ < 614000,]
NZAreas <- NZAreas[!grepl("Inlet", NZAreas$AU2013_V_1),]
NZAreas <- NZAreas[!grepl("Inland Water", NZAreas$AU2013_V_1),]
NZAreas <- NZAreas[!grepl("Coastal", NZAreas$AU2013_V_1),]
# plot(NZAreas)

# create dataframe to understand the data
NZAreasDF <- as.data.frame(NZAreas)
NZAreasDF[duplicated(NZAreasDF$AU2013_V1_), ]

# All area units, no concordance
# bring in Areas meta file
AreasTable <- read.csv("OriginalDataFiles/GISData/2013_Areas_Table.txt", header=TRUE)
#create short areas table file to merge with NZMap file
#AU2013_V1_ is integer, convert to numeric
AreasTable$AU2013_code <- as.numeric(AreasTable$AU2013_code)
#restrict areas to those that are not coastal
AreasTable <- AreasTable[AreasTable$AU2013_code < 614000,]

AreasNoDups2013 <- AreasTable %>%
  distinct(AU2013_code, AU2013_label, UA2013_code, UA2013_label, TA2013_code, TA2013_label, DHB_code, DHB_label) %>%
  rename(AU2013_V1_ = AU2013_code)

AreasNoDups2013[duplicated(AreasNoDups2013$AU2013_V1_), ]
#remove the two duplicates
AreasNoDups2013 <- AreasNoDups2013 %>%
  filter(!((AU2013_V1_==534200 & DHB_label == "Area outside District Health Board") | 
            (AU2013_V1_==581812 & DHB_label == "Area outside District Health Board") |
             grepl("Inlet", AU2013_label) | grepl("Inland Water", AU2013_label) |
             grepl("Coastal", AU2013_label) | grepl("Inland Water", AU2013_label)))

# check to see if there is a one-to-one match with the two 
AreasNoDups2013$AU2013_V1_[!(AreasNoDups2013$AU2013_V1_ %in% NZAreasDF$AU2013_V1_)]
NZAreasDF$AU2013_V1_[!(NZAreasDF$AU2013_V1_ %in% AreasNoDups2013$AU2013_V1_)]
# the inlets and water inland are still in the AreasDF file

#both exactly match on the area unit code
# merge on area unit code
NZAreasFinal <- merge(NZAreasDF, AreasNoDups2013)

# need to merge this summary level data with the original GIS data
NZAreasMerged <- merge(NZAreas, NZAreasFinal)

# get shape merge so that I only have the Territorial Authority plotted, but include Timaru
# need to merge all TA polygons into one
NZAreasMerged@data$TA2013_label <- as.character(NZAreasMerged@data$TA2013_label)
MergedtoTA <- data.frame(unique(NZAreasMerged@data$TA2013_label))
names(MergedtoTA)[names(MergedtoTA)=="unique.NZAreasMerged.data.TA2013_label."] <- "TA2013_label"
row.names(MergedtoTA) <- MergedtoTA$TA2013_label
NZUAsDissolved <- gUnaryUnion(NZAreasMerged, NZAreasMerged$TA2013_label)
NZAreasNew <- SpatialPolygonsDataFrame(NZUAsDissolved, MergedtoTA)

 plot(NZAreasNew)

rm(MergedtoTA, NZAreasDF, NZAreasFinal, NZAreasFixed, NZAreasMerged, NZAreasTest,
   NZUAsDissolved)

############################################################################################################
############################################################################################################
# Repeat to get South Island data only

SouthIslandAreas <- NZAreas
SouthIslandAreas <- SouthIslandAreas[SouthIslandAreas@data$AU2013_V1_ >=580200,]
SouthIslandAreas <- SouthIslandAreas[!grepl("Chatham", SouthIslandAreas$AU2013_V_1),]
plot(SouthIslandAreas)

SouthIslandAreasDF <- as.data.frame(SouthIslandAreas)
SIAreasNoDups2013 <- AreasNoDups2013 %>%
  filter(!(grepl("Chatham", AU2013_label) | AU2013_V1_ < 580200))

# check to see if there is a one-to-one match with the two 
SIAreasNoDups2013$AU2013_V1_[!(SIAreasNoDups2013$AU2013_V1_ %in% SouthIslandAreasDF$AU2013_V1_)]
SouthIslandAreasDF$AU2013_V1_[!(SouthIslandAreasDF$AU2013_V1_ %in% SIAreasNoDups2013$AU2013_V1_)]

#both exactly match on the area unit code
# merge on area unit code
SouthIslandAreasFinal <- merge(SouthIslandAreasDF, SIAreasNoDups2013)

# need to merge this summary level data with the original GIS data
SIAreasMerged <- merge(SouthIslandAreas, SouthIslandAreasFinal)

# get shape merge so that I only have the Territorial Authority plotted
# need to merge all TA polygons into one
SIAreasMerged@data$TA2013_label <- as.character(SIAreasMerged@data$TA2013_label)
SIMergedtoTA <- data.frame(unique(SIAreasMerged@data$TA2013_label))
names(SIMergedtoTA)[names(SIMergedtoTA)=="unique.SIAreasMerged.data.TA2013_label."] <- "TA2013_label"
row.names(SIMergedtoTA) <- SIMergedtoTA$TA2013_label
SIMergedtoTA$TimaruID = ifelse(SIMergedtoTA$TA2013_label=="Timaru District", 1, 0)
SIMergedtoTA$DistrictShortNames = gsub(" District.*","",SIMergedtoTA$TA2013_label)
SIUAsDissolved <- gUnaryUnion(SIAreasMerged, SIAreasMerged$TA2013_label)
SouthIslandTAs <- SpatialPolygonsDataFrame(SIUAsDissolved, SIMergedtoTA)

# gah, names don't match
# see what happened
# sapply(slot(SIUAsDissolved, "polygons"), function(x) slot(x, "ID"))
# names look the same :(

#plot(SouthIslandTAs)

#now show Timaru District on the map
tm_shape(SouthIslandTAs) + tm_polygons("TimaruID", group= "TA2013_label", border.col="grey", style = "jenks", 
                                       palette = "Greens") +
  tm_layout(legend.show = FALSE, frame = FALSE) +
  tm_text("DistrictShortNames", size=.6, col= "black", auto.placement = TRUE)

sf_SouthIsland <- sf::st_as_sf(SouthIslandTAs, plot = FALSE, fill = TRUE)
sf_SouthIsland %>% mutate(COLOR = ifelse(TimaruID ==1,1,0)) %>%
  ggplot() + geom_sf(aes(fill = as.factor(COLOR)), show.legend = FALSE)+
  scale_fill_manual(values = c("red","green"))

sf_SouthIsland %>%
  ggplot() + geom_sf(aes(fill = as.factor(TimaruID)), show.legend = FALSE, colour = "grey")+
  scale_fill_manual(values = c("darkseagreen1","seagreen")) +
  geom_sf_text(size=3, aes(label = DistrictShortNames)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(c(0,0,0,0), "cm"),
        rect = element_blank(),
        axis.ticks.length = unit(0, "pt"))
# 
# ggsave("test.pdf", plot = last_plot())

############################################################################################################
# Create Timaru District for plotting
Timaru <- readOGR("OriginalDataFiles/Area/area-unit-2013.shp")
Timaru@data$AU2013_V1_ <- as.numeric(as.character(Timaru@data$AU2013_V1_))
Timaru <- Timaru[((Timaru@data$AU2013_V1_ >= 598000 & Timaru@data$AU2013_V1_ <= 599900) |
                    Timaru@data$AU2013_V1_ == 625300),]

Timaru$TimaruUAvsRU = ifelse(Timaru$AU2013_V_1 %in% c("Fairview-Scarborough", "Fraser Park", 
                                                                         "Gleniti", "Glenwood", "Highfield", 
                                                                         "Maori Park", "Marchwiel", 
                                                                         "Otipua Creek-Washdyke Flat", "Parkside", 
                                                                         "Redruth", "Seaview", "Timaru Gardens", 
                                                                         "Waimataitai", "Washdyke", "Watlington"),
                                    1, 0)

tm_shape(Timaru) + tm_polygons(border.col="grey")

tm_shape(Timaru) + tm_polygons("TimaruUAvsRU", group= "AU2013_V_1", border.col="grey", style = "jenks", 
                               palette = "Greens") +
  tm_layout(legend.show = FALSE, frame = FALSE)



############################################################################################################
# Limit to actual AUs used
TUrban <- Timaru
TUrban@data<-droplevels(TUrban@data)

TUrban <- TUrban[TUrban@data$AU2013_V_1 %in% c("Fairview-Scarborough", "Fraser Park", 
                                                 "Gleniti", "Glenwood", "Highfield", 
                                                 "Maori Park", "Marchwiel", 
                                                 "Otipua Creek-Washdyke Flat", "Parkside", 
                                                 "Redruth", "Seaview", "Timaru Gardens", 
                                                 "Waimataitai", "Washdyke", "Watlington"),]

#now show TUrban District on the map
tm_shape(TUrban) + tm_polygons(border.col="white", "#74C476") +
  tm_layout(legend.show = FALSE, frame = FALSE) +
  tm_text("AU2013_V_1", size=.6, col= "black")



############################################################################################################
############################################################################################################
# look at plotting roads
############################################################################################################
############################################################################################################
# Roading file has AUs
NZRoading <- readOGR("OriginalDataFiles/nz-named-roads-points/nz-named-roads-points.shp")
proj4string(NZRoading)
plot(NZRoading)

NZRoading@data$tacode <- as.numeric(as.character(NZRoading@data$tacode))
NZRoading <- NZRoading[(NZRoading@data$tla == "Timaru District"),]


  