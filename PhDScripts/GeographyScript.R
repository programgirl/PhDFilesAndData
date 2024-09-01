#clear workspace
#  rm(list = ls())

library("dplyr")
library("tmap")
library("ggplot2")
library("sf")
library("sp")

library("RColorBrewer")
# get colour hex codes for greens palatte
brewer.pal(n = 9, name = "Greens")
brewer.pal(n = 9, name = "Greys")



############################################################################################################
############################################################################################################
# geography figures
# pretty sure I didn't save, but took a snapshot from the plot area
############################################################################################################
############################################################################################################


#Bring in shapefiles
NZAreas <- st_read("GISData/Area/area-unit-2013.shp")
# plot(st_geometry(NZAreas))


#remove the areas that are offshore
NZAreas$AU2013_V1_ <- as.numeric(as.character(NZAreas$AU2013_V1_))
NZAreas <- NZAreas[NZAreas$AU2013_V1_ < 614000,]
NZAreas <- NZAreas[!grepl("Inlet", NZAreas$AU2013_V_1),]
NZAreas <- NZAreas[!grepl("Inland Water", NZAreas$AU2013_V_1),]
NZAreas <- NZAreas[!grepl("Coastal", NZAreas$AU2013_V_1),]
# plot(NZAreas)

# create dataframe to understand the data
NZAreasDF <- as.data.frame(NZAreas)
NZAreasDF[duplicated(NZAreasDF$AU2013_V1_), ]

# All area units, no concordance
# bring in Areas meta file
AreasTable <- read.csv("GISData/2013_Areas_Table.txt", header=TRUE)
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
NZAreasMerged$TA2013_label <- as.character(NZAreasMerged$TA2013_label)
MergedtoTA <- data.frame(unique(NZAreasMerged$TA2013_label))
names(MergedtoTA)[names(MergedtoTA)=="unique.NZAreasMerged.data.TA2013_label."] <- "TA2013_label"
row.names(MergedtoTA) <- MergedtoTA$TA2013_label
# NZUAsDissolved <- st_union(NZAreasMerged, by = "TA2013_label")

# new method
NZUAsDissolved <- NZAreasMerged %>%
  group_by(TA2013_label) %>%
  summarise() %>%
  ungroup()

# plot(NZUAsDissolved)
rm(MergedtoTA, NZAreasDF, NZAreasFinal, NZUAsDissolved)

############################################################################################################
############################################################################################################
# Repeat to get South Island data only

SouthIslandAreas <- NZAreas
SouthIslandAreas <- SouthIslandAreas[SouthIslandAreas$AU2013_V1_ >=580200,]
SouthIslandAreas <- SouthIslandAreas[!grepl("Chatham", SouthIslandAreas$AU2013_V_1),]
# plot(SouthIslandAreas)

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
SIAreasMerged$TA2013_label <- as.character(SIAreasMerged$TA2013_label)
SIMergedtoTA <- data.frame(unique(SIAreasMerged$TA2013_label))
names(SIMergedtoTA)[names(SIMergedtoTA)=="unique.SIAreasMerged.TA2013_label."] <- "TA2013_label"
row.names(SIMergedtoTA) <- SIMergedtoTA$TA2013_label
SIMergedtoTA$TimaruID = ifelse(SIMergedtoTA$TA2013_label=="Timaru District", 1, 0)
SIMergedtoTA$DistrictShortNames = gsub(" District.*","",SIMergedtoTA$TA2013_label)

CombinedSIAreas <- left_join(SIAreasMerged, SIMergedtoTA, by = "TA2013_label")

SouthIslandTAs <- CombinedSIAreas %>%
  group_by(DistrictShortNames) %>%
  summarise() %>%
  ungroup() 

SouthIslandTAsText <- SouthIslandTAs %>%
  mutate(PointWithin = st_point_on_surface(geometry)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  st_as_sf()
    

SouthIslandMap <- tm_shape(SouthIslandTAs) +
  tm_polygons("DistrictShortNames",
              col = "darkgrey",
              fill.scale = tm_scale_ordinal(values = c("Ashburton" = "#F7FCF5", "Buller" = "#F7FCF5", "Central Otago" = "#F7FCF5",
                          "Christchurch City" = "#F7FCF5", "Clutha" = "#F7FCF5", "Dunedin City" = "#F7FCF5",
                          "Gore" = "#F7FCF5", "Grey" = "#F7FCF5", "Hurunui" = "#F7FCF5", 
                          "Invercargill City" = "#F7FCF5", "Kaikoura" = "#F7FCF5", "Mackenzie" = "#F7FCF5",
                          "Marlborough" = "#F7FCF5", "Nelson City" = "#F7FCF5", "Queenstown-Lakes" = "#F7FCF5",
                          "Selwyn" = "#F7FCF5", "Southland" = "#F7FCF5", "Tasman" = "#F7FCF5",
                          "Timaru" = "#74C476", "Waimakariri" = "#F7FCF5", "Waimate" = "#F7FCF5",
                          "Waitaki" = "#F7FCF5", "Westland" = "#F7FCF5"))) +
  tm_layout(legend.show = FALSE, frame = FALSE) +
  tm_shape(SouthIslandTAsText) +
  tm_text("DistrictShortNames", size=.6, col= "black")

#  tmap_save(SouthIslandMap, "SouthIslandMap.pdf", width=7.78, height=9.32, units="in")





############################################################################################################
# Create Timaru District for plotting

TimaruDistrict <- SIAreasMerged %>%
  filter(TA2013_code == 64)

# plot

TimaruDistrictMap <- tm_shape(TimaruDistrict) +
  # tm_polygons("DistrictShortNames", fill.scale = tm_scale(values = "greens"))
  tm_polygons("AU2013_label",
              col = "#525252",
              fill.scale = tm_scale_ordinal(values = c("Ben Mcleod" = "#F7FCF5", "Fairview-Scarborough" = "#74C476",
                                                       "Fraser Park" = "#74C476", "Geraldine" = "#F7FCF5",
                                                       "Gleniti" = "#74C476", "Glenwood" = "#74C476",
                                                       "Highfield" = "#74C476", "Levels" = "#F7FCF5",
                                                       "Maori Park" = "#74C476", "Marchwiel" = "#74C476",
                                                       "Orari" = "#F7FCF5", "Otipua Creek-Washdyke Flat" = "#74C476",
                                                       "Pareora" = "#F7FCF5", "Parkside" = "#74C476",
                                                       "Pleasant Point" = "#F7FCF5", "Redruth" = "#74C476",
                                                       "Seaview" = "#74C476", "Temuka" = "#F7FCF5",
                                                       "Timaru Gardens" = "#74C476", "Waimataitai" = "#74C476",
                                                       "Washdyke" = "#74C476", "Watlington" = "#74C476",
                                                       "Winchester" = "#F7FCF5"))) +
  tm_layout(legend.show = FALSE, frame = FALSE) 

#  tmap_save(TimaruDistrictMap, "TimaruDistrictMap.pdf", width=7.78, height=9.32, units="in")



############################################################################################################
# Limit to actual AUs used

TimaruUrbanArea <- TimaruDistrict %>%
  filter(AU2013_label %in% c("Fairview-Scarborough", "Fraser Park", "Gleniti", "Glenwood", "Highfield",
                             "Maori Park", "Marchwiel", "Otipua Creek-Washdyke Flat", "Parkside", 
                             "Redruth", "Seaview", "Timaru Gardens", "Waimataitai", "Washdyke", "Watlington"))

TimaruUrbanAreaText <- TimaruUrbanArea %>%
  mutate(PointWithin = st_point_on_surface(geometry)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  st_as_sf()

TimaruUrbanAreaMap <- tm_shape(TimaruUrbanArea) +
  tm_polygons("AU2013_label",
              col = "#737373",
              fill.scale = tm_scale_ordinal(values = c("Fairview-Scarborough" = "#C7E9C0","Fraser Park" = "#C7E9C0", 
                                                       "Gleniti" = "#C7E9C0", "Glenwood" = "#C7E9C0",
                                                       "Highfield" = "#C7E9C0", "Maori Park" = "#C7E9C0", 
                                                       "Marchwiel" = "#C7E9C0", "Otipua Creek-Washdyke Flat" = "#C7E9C0",
                                                       "Parkside" = "#C7E9C0", "Redruth" = "#C7E9C0",
                                                       "Seaview" = "#C7E9C0", "Timaru Gardens" = "#C7E9C0", 
                                                       "Waimataitai" = "#C7E9C0", "Washdyke" = "#C7E9C0", 
                                                       "Watlington" = "#C7E9C0"))) + 
  tm_layout(legend.show = FALSE, frame = FALSE) +
  tm_shape(TimaruUrbanAreaText) +
  tm_text("AU2013_label", size=.7, col= "black")

              
#  tmap_save(TimaruUrbanAreaMap, "TimaruUrbanAreaMap.pdf", width=7.78, height=9.32, units="in")



# delete the large maps
rm(SouthIslandMap, TimaruDistrictMap, TimaruUrbanAreaMap)


# ############################################################################################################
# ############################################################################################################
# # Add schools
# ############################################################################################################
# ############################################################################################################

# get gecords from school street address

TimaruSchoolGeoCoded <- data.frame(SchoolName = c("Barton Rural School", "Bluestone School", "Craighead Diocesan School",
                                                  "Gleniti School", "Grantlea Downs School", "Highfield School",
                                                  "Mountainview High School", "Oceanview Heights School", "Roncalli College",
                                                  "Sacred Heart School", "St Joseph's School",
                                                  "Timaru Boys' High School", "Timaru Christian School",
                                                  "Timaru Girls' High School","Timaru South School", "Waimataitai School"),
                                   Address = c("462 Fairview Road, Fairview, Timaru", "46 Raymond Street, West End, Timaru",
                                               "1 Wrights Avenue, Highfield, Timaru", "22 Heath Street, Gleniti, Timaru",
                                               "65 Grants Road, Marchwiel, Timaru", "26 Rimu Street, Highfield, Timaru",
                                               "97 Pages Road, Timaru", "241 Selwyn Street, Marchwiel, Timaru", 
                                               "19 Wellington Street, Parkside, Timaru", "54 Heaton Street, Parkside, Timaru",
                                               "50 Kelvin Street, Marchwiel, Timaru", "211 North Street, Timaru",
                                               "10 Quarry Road, Watlington, Timaru", "68 Cain Street, Parkside, Timaru",
                                               "44 Queen Street, Parkside, Timaru", "25 Trafalgar Street, Waimataitai, Timaru"),
                                   Type = c("Primary", "Primary", "Secondary", "Primary", "Primary", "Primary", "Secondary",
                                            "Primary", "Secondary", "Primary", "Primary", "Secondary", "Primary",
                                            "Secondary", "Primary", "Primary"))

TimaruSchoolGeoCoded <- TimaruSchoolGeoCoded %>%
  tidygeocoder::geocode(Address, method = "arcgis") %>%
  mutate(SchoolID = row_number())

TimaruSchoolSP <- st_as_sf(TimaruSchoolGeoCoded, coords = c("long", "lat"), crs = 4326)


TimaruUrbanAreaSchools <- tm_shape(TimaruUrbanArea) +
  tm_polygons("AU2013_label",
              col = "grey",
              fill.scale = tm_scale_ordinal(values = c("Fairview-Scarborough" = "#F7FCF5","Fraser Park" = "#F7FCF5", 
                                                       "Gleniti" = "#F7FCF5", "Glenwood" = "#F7FCF5",
                                                       "Highfield" = "#F7FCF5", "Maori Park" = "#F7FCF5",
                                                       "Marchwiel" = "#F7FCF5", "Otipua Creek-Washdyke Flat" = "#F7FCF5",
                                                       "Parkside" = "#F7FCF5", "Redruth" = "#F7FCF5",
                                                       "Seaview" = "#F7FCF5", "Timaru Gardens" = "#F7FCF5",
                                                       "Waimataitai" = "#F7FCF5", "Washdyke" = "#F7FCF5",
                                                       "Watlington" = "#F7FCF5")),
              fill.legend = tm_legend_hide()) + 
  tm_layout(frame = FALSE) +
  
  tm_shape(TimaruSchoolSP) +
  tm_dots("Type", fontface = "bold", fill.legend = tm_legend_hide()) +
  tm_text("SchoolName", size = .5, col = "Type", fontface = "bold", xmod = .2, ymod=.5, col.legend = tm_legend_hide()) #+
  # tm_add_legend(title = "School", labels = TimaruSchoolSP$SchoolName, text = TimaruSchoolSP$SchoolID)

#  tmap_save(TimaruUrbanAreaSchools, "TimaruUrbanAreaSchools.pdf", width=8.5, height=12, units="in", dpi = 600)








############################################################################################################
############################################################################################################
# Count for Timaru for census night population
############################################################################################################
############################################################################################################

TABLECODE8002 <- read.csv("Stats NZ csv files/Timaru District Age by Sex for the census night population count/Age by sex/TABLECODE8002_Data_74b8c782-bb4d-4b52-995f-37e1dbe47d8d.csv")

TABLECODE8002 <- TABLECODE8002 %>%
  select(-c(Flags, Year))


write.table(TABLECODE8002, file = "Table8002.csv", quote = FALSE, sep = ";",
            row.names = FALSE)

# get the counts

InUA <- TABLECODE8002 %>%
  filter(Area %in% c("Fairview-Scarborough", "Fraser Park", "Gleniti", "Glenwood", "Highfield", "Maori Park", "Marchwiel", 
                     "Otipua Creek-Washdyke Flat", "Parkside", "Redruth", "Seaview", "Timaru Gardens", "Waimataitai", 
                     "Washdyke", "Watlington"))

NotInUA <- TABLECODE8002 %>%
  filter(!(Area %in% c(InUA$Area)), !Area== "Timaru District")

PropUA <- sum(InUA$Value)/as.numeric(TABLECODE8002[1,4])