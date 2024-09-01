#clear workspace
#  rm(list = ls())

library("dplyr")
library("readxl")
library("ggplot2")
library("pals")


################################################################################################
################################################################################################
# School data preparation
################################################################################################
################################################################################################

# bring in population data
File12PopulationHouseholds <- readRDS("PhDRData/File12PopulationHouseholds.rds")

# get counts by age in the population
CountsByAge <- File12PopulationHouseholds %>%
  filter(between(Age, 5, 18), EducationStatus == "Y") %>%
  group_by(Age) %>%
  summarise(Count = n())


# #Bring in 2013 school rolls data from the Ministry of Education
StudentRollsBySchool2013 <- read_excel("Ministry of Education files/School Rolls/2-Student-rolls-by-School_2010-2023.xlsx", 
                                       sheet = "2013", 
                                       col_types = c("text", "text", "numeric", "text", "text", "text", "skip", "skip", "text",
                                                     "text", "text", "text", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip"), 
                                       skip = 3)


#remove all schools outside of Timaru District
TimaruStudentRolls <- StudentRollsBySchool2013 %>%
  rename(SchoolName = `School Name`) %>%
  filter(`TA with Auckland Local Board` == "Timaru District") %>%
  mutate(Total = as.numeric(Total),
         Female = as.numeric(Female),
         Male = as.numeric(Male),
         InternationalFeePaying = as.numeric(`International Fee Paying`),
         Age5 = as.numeric(`Age 5`),
         Age6 = as.numeric(`Age 6`),
         Age7 = as.numeric(`Age 7`),
         Age8 = as.numeric(`Age 8`),
         Age9 = as.numeric(`Age 9`),
         Age10 = as.numeric(`Age 10`),
         Age11 = as.numeric(`Age 11`),
         Age12 = as.numeric(`Age 12`),
         Age13 = as.numeric(`Age 13`),
         Age14 = as.numeric(`Age 14`),
         Age15 = as.numeric(`Age 15`),
         Age16 = as.numeric(`Age 16`),
         Age17 = as.numeric(`Age 17`),
         Age18 = as.numeric(`Age 18`),
         Age19 = as.numeric(`Age 19`),
         Age20 = as.numeric(`Age 20`),
         Age21 = as.numeric(`Age 21`),
         Age22Plus = as.numeric(`Age 22+`)) %>%
  select(-c(`Age 5`, `Age 6`, `Age 7`, `Age 8`, `Age 9`, `Age 10`, `Age 11`, `Age 12`, `Age 13`, `Age 14`, `Age 15`,
            `Age 16`, `Age 17`, `Age 18`, `Age 19`, `Age 20`, `Age 21`, `Age 21`, `Age 22+`)) %>%
  replace(is.na(.), 0)

 




# Get down to the TAU level, confirmed this with Ministry of Education
#addresses of Timaru Schools and meshblocks
# #note: when multiple streets of that name in same district, Google Maps checked for surrounding streets and
# #correct street located that way
# #1 Geraldine High School, 93 McKenzie Road, Geraldine = meshblock 2849700,Aviemore, Waitaki District (no)
# #2 Opihi College, Richard Pearse Drive, Temuka = meshblock 2767000, Temuka, Timaru District (no)
# #3 Craighead Diocesan School, Wrights Avenue, Highfield = meshblock 2787800, Highfield, Timaru District (yes)
# #4 Roncalli College, 19 Wellington Street, Parkside = meshblock 2799000, Parkside, Timaru District (yes)
# #5 Mountainview High School, 97 Pages Road, Timaru = meshblock 2784600, Marchwiel, Timaru District (yes)
# #6 Timaru Boys' High School, 211 North Street, Timaru = meshblock 2789800, Fraser Park, Timaru District (yes)
# #7 Timaru Girls' High School, Cain Street, Timaru = meshblock 2798700, Parkside, Timaru District (yes)
# 
# #8 Timaru Christian School, 10 Quarry Road = meshblock 2796000, Watlington, Timaru District (yes)
# #9 Geraldine Primary School, 73 Wilson Street, Geraldine = meshblock 2764200, Geraldine, Timaru District (no)
# #10 Winchester Rural School, 14 Rise Road, Winchester = meshblock 2769200, Winchester, Timaru District (no)
# #11 Temuka Primary School, 9 Hayhurst Street, Temuka = meshblock 2766600, Temuka, Timaru District (no)
# #12 Oceanview Heights School, 241 Selwyn Street, Marchwiel, Timaru = meshblock 2783500, Marchwiel, 
# #Timaru District (yes)
# #13 Grantlea Downs School, 65 Grants Road, Marchwiel, Timaru = meshblock 2782700, Marchwiel, Timaru District (yes)
# #14 Barton Rural School, 462 Fairview Road, Fairview = meshblock 2777000, Fairview-Scarborough, Timaru District (yes)
# #15 Bluestone School, 46 Raymond Street, West End, Timaru = meshblock 2789500, Fraser Park, Timaru District (yes)
# #16 Beaconsfield School, 40 Guscott Road, Pareora West = meshblock 2776800, Levels, Timaru District (no)
# #17 Timaru South School, has bloody two campuses!
# #Timaru South School, 44 Queen Street, Parkside = meshblock 280220, Timaru Gardens, Timaru District (yes)
# #Timaru South School Pareora Campus, 15 Elworthy Street, Pareora = meshblock 2776300, Pareora, Timaru District (no)
# #18 Arowhenua Maori School, Huirapa Street, Temuka = meshblock 2771100, Orari, Timaru District (no)
# #19 Gleniti School, 22 Heath Street, Gleniti, Timaru = meshblock 2774607, Gleniti, Timaru District (yes)
# #20 Highfield School, Rimu Street, Highfield = meshblock 2775300, Glenwood, Timaru District (yes)
# #21 Pleasant Point Primary School, 33 Halstead Road, Pleasant Point = meshblock 2779500, Pleasant Point, 
# #Timaru District (no)
# #22 Sacred Heart School (Timaru), 54 Heaton Street, Parkside, Timaru = meshblock 2799200, Parkside, 
# #Timaru District (yes)
# #23 St Joseph's School (Pleasant Point), 29 Afghan Street, Pleasant Point = meshblock 2779900, Pleasant Point, 
# #Timaru District (no)
# #24 St Joseph's School (Temuka), Wilkin Street, Temuka = meshblock in Temuka, cannot ID exact, Temuka, 
# #Timaru District (no)
# #25 St Joseph's School (Timaru), 50 Kelvin Street, Marchwiel, Timaru = meshblock 2783400, Marchwiel, 
# #Timaru District (yes)
# #26 Waimataitai School, Trafalgar Street, Waimataitai, Timaru = meshblock 2780300, Maori Park, Timaru District (yes)
# #27 Woodbury School, 559 Woodbury Road, Woodbury = meshblock 2761200, Orari, Timaru District (no)
# #28 Waihi School, 611 Temuka-Orari Highway, Temuka = meshblock 2760400, Orari, Timaru District (no)
# 
# #So, remove all schools not in relevant meshblocks
# # cannot distinguish students at the Pareroa campus for Timaru South School, so retain all students
# # retention will result in about 50 extra students - see personal communication from school in May 2018

TimaruUrbanAreaRolls <- TimaruStudentRolls %>%
  filter(SchoolName %in% c("Craighead Diocesan School", "Roncalli College", "Mountainview High School", "Timaru Boys' High School",
                           "Timaru Girls' High School", "Timaru Christian School", "Oceanview Heights School",
                           "Grantlea Downs School", "Barton Rural School", "Bluestone School", "Timaru South School",
                           "Gleniti School", "Highfield School", "Sacred Heart School (Timaru)", "St Joseph's School (Timaru)",
                           "Waimataitai School"))



# produce the primary schools
# fix roll count for Timaru South

TimaruSouthSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Timaru South School") %>%
  select(SchoolName, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, Age15, Age16, Age17, Age18, Age19, Age20,
         Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  mutate(Prop = RollCount/sum(RollCount),
         NumToLose = round(Prop*50,0),
         FinalCount = RollCount - NumToLose) %>%
  select(-c(RollCount, Prop, NumToLose)) %>%
  rename(RollCount = FinalCount) 


PrimarySchoolsNotTimaruSouth <- TimaruUrbanAreaRolls %>%
  filter(!SchoolName == "Timaru South School")


PrimaryAgeRollCounts <- data.frame("Age" = c(5, 6, 7, 8, 9, 10, 11, 12),
                                   "TUACounts" = c(as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age5)+TimaruSouthSchool[1,3]), 
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age6)+TimaruSouthSchool[2,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age7)+TimaruSouthSchool[3,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age8)+TimaruSouthSchool[4,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age9)+TimaruSouthSchool[5,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age10)+TimaruSouthSchool[6,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age11)+TimaruSouthSchool[7,3]),
                                                   as.numeric(sum(PrimarySchoolsNotTimaruSouth$Age12)+TimaruSouthSchool[8,3])))

PrimaryKidsRollCount <- sum(PrimaryAgeRollCounts$TUACounts)

AllPrimaryCounts <- left_join(PrimaryAgeRollCounts, CountsByAge, by = "Age") %>%
  mutate(Undercount = TUACounts - Count,
         PercentUC = round(100*(Undercount/TUACounts),1))

# total undercount
TotalPrimaryUndercount <- sum(AllPrimaryCounts$Undercount)

PrimaryUndercountProp <- round(TotalPrimaryUndercount/PrimaryKidsRollCount,3) 




# secondary schools

SecondaryAgesRolls <- TimaruUrbanAreaRolls %>%
  filter(!SchoolName == "Timaru South School") %>%
  select(c(Age13, Age14, Age15, Age16, Age17, Age18)) 

SecondaryAgeRollCounts <- data.frame("Age" = c(13, 14, 15, 16, 17, 18),
                                     "TUACounts" = c(as.numeric(sum(SecondaryAgesRolls$Age13)+TimaruSouthSchool[9,3]), 
                                                     as.numeric(sum(SecondaryAgesRolls$Age14)),
                                                     as.numeric(sum(SecondaryAgesRolls$Age15)),
                                                     as.numeric(sum(SecondaryAgesRolls$Age16)),
                                                     as.numeric(sum(SecondaryAgesRolls$Age17)),
                                                     as.numeric(sum(SecondaryAgesRolls$Age18))))

SecondaryKidsRollCount <- sum(SecondaryAgeRollCounts$TUACounts)


AllSecondaryCounts <- left_join(SecondaryAgeRollCounts, CountsByAge, by = "Age") %>%
  mutate(Undercount = TUACounts - Count,
         PercentUC = round(100*(Undercount/TUACounts),1))

# total undercount
TotalSecondaryUndercount <- sum(AllSecondaryCounts$Undercount)

SecondaryUndercountProp <- round(TotalSecondaryUndercount/SecondaryKidsRollCount,3) 








################################################################################################
# UNDERCOUNT COMPARISONS HERE
################################################################################################


KidsInSchoolsRollCountData <- sum(PrimaryKidsRollCount, SecondaryKidsRollCount) 

# num synthetic population
NumKidsInSchoolSP <- as.numeric(sum(CountsByAge$Count))

DiffInSchool <- KidsInSchoolsRollCountData - NumKidsInSchoolSP

MissingProp <- 1-(NumKidsInSchoolSP/KidsInSchoolsRollCountData)



rm(AllPrimaryCounts, AllSecondaryCounts, CountsByAge, PrimaryAgeRollCounts, PrimarySchoolsNotTimaruSouth, 
   SecondaryAgesRolls, StudentRollsBySchool2013, TimaruStudentRolls, DiffInSchool, KidsInSchoolsRollCountData,
   MissingProp, NumKidsInSchoolSP, PrimaryKidsRollCount, PrimaryUndercountProp, SecondaryAgeRollCounts,
   SecondaryKidsRollCount, SecondaryUndercountProp, TotalPrimaryUndercount, TotalSecondaryUndercount, 
   File12PopulationHouseholds)



################################################################################################
# finalise school roll data frame
################################################################################################

# create one data frame per school
# then append together

# Craighead Diocesan School
CraigheadDiocesanSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Craighead Diocesan School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20,Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`)


# Roncalli College School
RoncalliCollege <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Roncalli College") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Mountainview High School
MountainviewHighSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Mountainview High School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
 rename(SchoolID = `School ID`) 


# Timaru Boys' High School
TimaruBoys<- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Timaru Boys' High School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Timaru Girls' High School
TimaruGirls<- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Timaru Girls' High School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Timaru Christian School
TimaruChristianSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Timaru Christian School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Oceanview Heights School
OceanviewHeightsSchool<- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Oceanview Heights School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Grantlea Downs School
GrantleaDownsSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Grantlea Downs School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Barton Rural School
BartonRuralSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Barton Rural School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Bluestone School
BluestoneSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Bluestone School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Timaru South School - includes the school ID
TimaruSouthSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Timaru South School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  mutate(Prop = RollCount/sum(RollCount),
         NumToLose = round(Prop*50,0),
         FinalCount = RollCount - NumToLose) %>%
  select(-c(RollCount, Prop, NumToLose)) %>%
  rename(RollCount = FinalCount,
         SchoolID = `School ID`) 


# Gleniti School
GlenitiSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Gleniti School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Highfield School
HighfieldSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Highfield School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Sacred Heart School (Timaru)
SacredHeartSchoolTimaru <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Sacred Heart School (Timaru)") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# St Joseph's School (Timaru)
StJosephsSchoolTimaru <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "St Joseph's School (Timaru)") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


# Waimataitai School
WaimataitaiSchool <- TimaruUrbanAreaRolls %>%
  filter(SchoolName == "Waimataitai School") %>%
  select(`School ID`, SchoolName, Gender, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, Age13, Age14, 
         Age15, Age16, Age17, Age18, Age19, Age20, Age21, Age22Plus) %>%
  tidyr::pivot_longer(cols = Age5:Age22Plus,
                      names_to = "Age",
                      values_to = "RollCount") %>%
  rename(SchoolID = `School ID`) 


File13SchoolsDataFrame <- bind_rows(BartonRuralSchool, BluestoneSchool, CraigheadDiocesanSchool, GlenitiSchool,
                                    GrantleaDownsSchool, HighfieldSchool, MountainviewHighSchool,
                                    OceanviewHeightsSchool, RoncalliCollege, SacredHeartSchoolTimaru,
                                    StJosephsSchoolTimaru, TimaruBoys, TimaruChristianSchool, TimaruGirls,
                                    TimaruSouthSchool, WaimataitaiSchool) %>%
  mutate(SchoolType = ifelse(Gender == "Co-Ed", "C",
                             ifelse(Gender == "Single Sex-Boys", "M", "F")))



saveRDS(File13SchoolsDataFrame, file = "PhDRData/File13SchoolsDataFrame.rds")

rm(list = ls())















################################################################################################
################################################################################################
# Add the schools
################################################################################################
################################################################################################

# create the school data frame in the necessary format

File13SchoolsDataFrame <- readRDS("PhDRData/File13SchoolsDataFrame.rds")

WorkingSchoolsData <- File13SchoolsDataFrame %>%
  filter(!Age %in% c("Age19", "Age20", "Age21", "Age22Plus"),
         RollCount > 0)

# get some stats
min(WorkingSchoolsData$RollCount)
max(WorkingSchoolsData$RollCount)
median(WorkingSchoolsData$RollCount)

# get stats by school type


SecondarySchools <- WorkingSchoolsData %>%
  filter(SchoolID %in% c(361, 360, 358, 357, 359))

PrimarySchools <- WorkingSchoolsData %>%
  filter(!SchoolID %in% SecondarySchools$SchoolID)


median(PrimarySchools$RollCount)
median(SecondarySchools$RollCount)

# size by school

PrimarySchoolSizes <- PrimarySchools %>%
  group_by(SchoolID) %>%
  summarise(NumInSchool = sum(RollCount))

SecondarySchoolSizes <- SecondarySchools %>%
  group_by(SchoolID) %>%
  summarise(NumInSchool = sum(RollCount))

  
  

# bring in the population

File12PopulationHouseholds <- readRDS("PhDRData/File12PopulationHouseholds.rds")

# add sex marker required

PopulationWorkingData <- File12PopulationHouseholds %>%
  mutate(SexMarker = ifelse(Sex == "Male", "M", "F"))

# bring in the random seeds

TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")



SchoolsForUse <- WorkingSchoolsData %>%
  filter(RollCount > 4) %>%
  rename(AgeCat = Age) %>%
  mutate(Age = as.numeric(gsub("Age", "", AgeCat)))


# # check child counts versus school counts
# 
# TotalSchoolRollByAgeSex <- SchoolsFormatted %>%
#   ungroup() %>%
#   select(Age, Roll, Sex) %>%
#   group_by(Age, Sex) %>%
#   summarise(RollCount = sum(Roll))
# 
# TotalSchoolRollByAge <-  SchoolsFormatted %>%
#   ungroup() %>%
#   select(Age, Roll) %>%
#   group_by(Age) %>%
#   summarise(RollCount = sum(Roll))
# 
# 
# File7SchoolInd <- readRDS("PhDRData/SchoolSetupWork/File7SchoolInd.rds")
# File7SchoolInd <- File7SchoolInd %>%
#   mutate(SexMatch = ifelse(Sex == "Female", "F", "M"))
# 
# ChildrenInSchool <- File7SchoolInd %>%
#   filter(InSchool == "Y") %>%
#   group_by(Age) %>%
#   summarise(AgeCount = n())
# 
# # comparison of counts
# DiffInTots <- left_join(TotalSchoolRollByAge, ChildrenInSchool, by = "Age") %>%
#   mutate(AgeDiffs = RollCount - AgeCount)
# 
# # how many school roll places now?
# sum(DiffInTots$RollCount)








# add the schools

SchoolsAdded <- pairschoolwt(PopulationWorkingData, "ID", "Age", "SexMarker", "EducationStatus", "HouseholdID",
                                 SchoolsForUse, "SchoolID", "Age", "RollCount", "SchoolType", 0, sameprob = 0.9,
                                 userseed = TheRandomSeeds[113])            #################### seed 113 %%%%%%%%%%%%%%%%



File13SchoolsAdded <- SchoolsAdded$Population
File13SchoolsLeftOver <- SchoolsAdded$Schools

duplicates <- File13SchoolsAdded %>%
  group_by(ID, HouseholdID) %>%
  summarise(count=n()) %>%
  filter(count > 1)

table(File13SchoolsAdded$schoolID, File13SchoolsAdded$EducationStatus)

table(File13SchoolsAdded$EducationStatus)
sum(File13SchoolsLeftOver$spacesUsed)



saveRDS(File13SchoolsAdded, file = "PhDRData/File13SchoolsAdded.rds")
saveRDS(File13SchoolsLeftOver, file = "PhDRData/File13SchoolsLeftOver.rds")

# get stats on how many school spots were used
min(File13SchoolsLeftOver$spacesUsed)
median(File13SchoolsLeftOver$spacesUsed)
max(File13SchoolsLeftOver$spacesUsed)


ChildrenByAge <- File13SchoolsAdded %>%
  filter(!(schoolID=="0")) %>%
  ungroup() %>%
  select(Age) %>%
  group_by(Age) %>%
  summarise(Count = n())

OriginalKids <- PopulationWorkingData %>%
  filter(EducationStatus == "Y") %>%
  select(Age) %>%
  group_by(Age) %>%
  summarise(Count = n())

# number children in school
sum(OriginalKids$Count)


ChildAgeDiffs <- left_join(OriginalKids, ChildrenByAge, by = c("Age")) %>%
  mutate(CountDiff = Count.x - Count.y)



# locate any missing kids
ChildrenMissing <- PopulationWorkingData %>%
  filter(EducationStatus == "Y",
         !(ID %in% File13SchoolsAdded$ID))

# any duplicate kids?
ChildrenDoubleUps <- File13SchoolsAdded %>%
  group_by(ID) %>%
  summarise(NumID = n()) %>%
  filter(NumID > 1)

# compare school counts as applied to the kids versus those output from the function

schoolsToKids <- File13SchoolsAdded %>%
  filter(!(schoolID == "0")) %>%
  select(schoolID, Age) %>%
  group_by(schoolID, Age) %>%
  summarise(fromKids = n())

OutputVsCalc <- left_join(File13SchoolsLeftOver, schoolsToKids, by = c("SchoolID" = "schoolID", "Age")) %>%
  mutate(FromKids = ifelse(is.na(fromKids), 0, fromKids),
         Diff = spacesUsed - fromKids) %>%
  filter(Diff > 0)
  
# get the average number of kids by school, by age

SchoolNumSummary <- File13SchoolsLeftOver %>%
  group_by(SchoolName) %>%
  summarise(MeanInitial = mean(RollCount),
            MinInitial = min(RollCount),
            MaxInitial = max(RollCount),
            NumKidsInitial = sum(RollCount),
            MeanPlaced = mean(spacesUsed),
            MinPlaced = min(spacesUsed),
            MaxPlaced = max(spacesUsed),
            NumKidsPlaced = sum(spacesUsed))

SchoolPropPlaced <- File13SchoolsLeftOver %>%
  mutate(PropPlaced = spacesUsed/RollCount)

mean(SchoolPropPlaced$PropPlaced)

# compare to original differences

NumSchoolPlaces <- sum(File13SchoolsLeftOver$RollCount)
OriginalNumSchoolPlaces <- sum(SchoolsForUse$RollCount)


# show roll count prop by roll count size
RollPropByCount <- ggplot(SchoolPropPlaced, aes(x=RollCount, y=PropPlaced, color=SchoolName)) +
  geom_point(size = 1) +
  scale_colour_manual(values=unname(glasbey())) +
  xlab("Roll count (Age within school)") + ylab("Proportion of roll used") + labs(color = "School") +
  scale_x_continuous(breaks = seq(0,180, by = 20)) +
  scale_y_continuous(breaks = seq(0, 1, by = .2)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        legend.position = "bottom")

#  ggsave(RollPropByCount, file="RollPropByCount.pdf", width=9.32, height=7.78, units="in")

