#clear workspace
#  rm(list = ls())

library(dplyr)
# library(forcats)


#############################################################
# table 230
#############################################################

TABLECODE230 <- read.csv("~/Sync/PhD/Stats NZ csv files/CantRegionCensusUsuallyResidentPop199620012006/Age Group by Sex/TABLECODE230_Data_6ab1aeef-664b-4394-8e95-89bb1aa28a1f.csv", stringsAsFactors=FALSE)

TABLECODE230 <- TABLECODE230 %>%
  select(-Flags)


write.table(TABLECODE230, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table230.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())

####################################################################################################
####################################################################################################
# Table 8001
####################################################################################################
####################################################################################################

####################################################################################################
# Table 8001a
####################################################################################################

TimaruDistrictSexAge <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex/TABLECODE8001_Data_1746e3c7-2263-4fd5-9dc1-6aef89a0874d.csv")
str(TimaruDistrictSexAge)

patterns <- c("-", "Total", "Median", "105", "106", "107", "108", "109", "110", "111", "112",
              "113", "114", "115", "116", "117", "118", "119", "120", "over")

TDAgeSexOrig <- TimaruDistrictSexAge %>%
  select(-c("Year", "Flags")) %>%
  filter(!grepl(paste(patterns, collapse="|"), Age.group))

SubSetTDAgeSexOrig <- TDAgeSexOrig %>%
  slice_head(n=30)

str(SubSetTDAgeSexOrig)

# output the table for the appendix, into csv format

write.table(SubSetTDAgeSexOrig, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001a.csv", quote = FALSE, sep = ",",
            row.names = FALSE)

TDAgeSexMod <- TDAgeSexOrig


TDAgeSexMod$IntegerAge <- as.numeric(gsub("[^[:digit:]]", "", TDAgeSexMod$Age))

# fix the years that are not digits, but are words
TDAgeSexMod$IntegerAge <- ifelse(TDAgeSexMod$Age=="Less than one year", 0,
                              ifelse(TDAgeSexMod$Age=="One year",1,
                                     ifelse(TDAgeSexMod$Age=="Two years",2,
                                            ifelse(TDAgeSexMod$Age=="Three years",3,
                                                   ifelse(TDAgeSexMod$Age=="Four years",4,
                                                          ifelse(TDAgeSexMod$Age=="Five years",5,
                                                                 ifelse(TDAgeSexMod$Age=="Six years",6,
                                                                        ifelse(TDAgeSexMod$Age=="Seven years",7,
                                                                               ifelse(TDAgeSexMod$Age=="Eight years",8,
                                                                                      ifelse(TDAgeSexMod$Age=="Nine years",9, 
                                                                                             TDAgeSexMod$IntegerAge))))))))))

SubSetTDAgeSexMod <- TDAgeSexMod %>%
  slice_head(n=30)

write.table(SubSetTDAgeSexMod, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001aMod.csv", quote = FALSE, sep = ",",
            row.names = FALSE)










####################################################################################################
# Table 8001b
####################################################################################################

TimaruDistrictAgeTotals8001 <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex groups/TABLECODE8001_Data_b339765f-cf78-4e38-aa57-332c2e066b02.csv")

TimaruDistrict8001b <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Flags)) 

SubSetTimaruDistrict8001b <- TimaruDistrict8001b %>%
  slice_head(n=30)

write.table(SubSetTimaruDistrict8001b, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001b.csv", quote = FALSE, 
            sep = ";", row.names = FALSE)

TimaruDistrictAgeTotalsCleaned <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("10-14 years", "25-29 years", "30-34 years", "35-39 years",
                          "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years")) %>%
  rename(ChildAge = Age.group) %>%
  mutate(ChildAge = gsub(" y", " Y", ChildAge))

TimaruDistrict15to17 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("15 years", "16 years", "17 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "15-17 Years")

TimaruDistrict18to24 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("18 years", "19 years", "20-24 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "18-24 Years")

TimaruDistrict65to84 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "65 Years and Over")

TimaruDistrictAgeTotalsCleaned <- bind_rows(TimaruDistrictAgeTotalsCleaned, TimaruDistrict15to17,
                                            TimaruDistrict18to24, TimaruDistrict65to84)


write.table(TimaruDistrictAgeTotalsCleaned, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001bMod.csv", 
            quote = FALSE, sep = ",", row.names = FALSE)














####################################################################################################
# Table 8001c
####################################################################################################

CRAgePyramid <- read.csv("OriginalDataFiles/Usually Resident/Canterbury Region/TABLECODE8001_Data_6001290a-09ac-4761-b81c-d1f1a2887506.csv")

CRAgeSexOrig <- CRAgePyramid %>%
  select(-c("Year", "Flags")) %>%
  filter(!grepl(paste(patterns, collapse="|"), Age.group))

SubSetCRAgeSexOrig <- CRAgeSexOrig %>%
  slice_head(n=30)

write.table(SubSetCRAgeSexOrig, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001c.csv", quote = FALSE, sep = ",",
            row.names = FALSE)

CRAgeSexMod <- CRAgeSexOrig %>%
  filter(Age.group %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years")) %>%
  mutate(CurrentAge = as.numeric(sub("([0-9]+).*$", "\\1", Age.group)))

write.table(CRAgeSexMod, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001cMod.csv", quote = FALSE, sep = ",",
            row.names = FALSE)








####################################################################################################
# Table 8001d
####################################################################################################

NZAgePyramid <- read.csv("OriginalDataFiles/Usually Resident/New Zealand/TABLECODE8001_Data_b4b82516-2eb6-495d-9ddf-7485e5147e5c.csv")

NZAgeSexOrig <- NZAgePyramid %>%
  select(-c("Year", "Flags")) %>%
  filter(!grepl(paste(patterns, collapse="|"), Age.group)) 

SubSetTNZAgeSexOrig <- NZAgeSexOrig %>%
  slice_head(n=25)

write.table(SubSetTNZAgeSexOrig, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001d.csv", quote = FALSE, sep = ";",
            row.names = FALSE)


TNZAgeSexMod <- NZAgeSexOrig %>%
  filter(Age.group %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years")) %>%
  mutate(CurrentAge = as.numeric(sub("([0-9]+).*$", "\\1", Age.group)))

write.table(TNZAgeSexMod, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8001dMod.csv", quote = FALSE, sep = ";",
            row.names = FALSE)


rm(list = ls())







#############################################################
# table 8084a
#############################################################

# show no people in prison
TABLECODE8084a<- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied dwelling type by age group of usual residents, for usual residents in occupied dwellings, 2013 Census (RC, TA)NotPrivate/TABLECODE8084_Data_69ed6c76-b16f-4ed0-8a05-1380a7be2c2c.csv", 
                                      col_types = cols(Flags = col_skip()))


TABLECODE8084a1 <- TABLECODE8084a %>%
  slice(1:11) %>%
  rename(Occupied.dwelling.type = `Occupied dwelling type`,
         Age.group = `Age group`)

write.table(TABLECODE8084a1, file = "~/Sync/PhD/ThesisVersions/Thesis2024/Table8084a.csv", quote = FALSE, sep = ";",
            row.names = FALSE)


# get non suppressed

NonSuppressed <- TABLECODE8084a %>%
  filter(!(is.na(Value)),
         !`Age group` == "Total people, age group",
         !`Occupied dwelling type` == "Total usual residents in institutions")






#############################################################
# table 8084b
#############################################################


TABLECODE8084b <- TABLECODE8084a %>%
  filter(`Occupied dwelling type` == "Residential care for older people") %>%
  rename(Occupied.dwelling.type = `Occupied dwelling type`,
         Age.group = `Age group`)


write.table(TABLECODE8084b, file = "~/Sync/PhD/ThesisVersions/Thesis2024/Table8084b.csv", quote = FALSE, sep = ";",
            row.names = FALSE)







#############################################################
# table 8141
#############################################################

ChildDepend <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type by child dependency status/TABLECODE8141_Data_da61e1a1-c19c-41a8-ac22-c025640ce237.csv")

ChildDepend <- ChildDepend %>%
  select(-c(Year, Flags))

write.table(ChildDepend, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8141.csv", quote = FALSE, sep = ";",
            row.names = FALSE)


rm(list = ls())






#############################################################
# table 8143
#############################################################


CouplesChildDepend <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/NumDependentChildrenAndTotalNumberChildrenForCouplesWithChildren/TABLECODE8143_Data_d3e4c3d9-4ade-4c59-894d-34ff717bff31.csv")


CouplesChildDepend <- CouplesChildDepend %>%
  select(-c(Year, Flags))


write.table(CouplesChildDepend, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/CouplesChildDepend.csv", 
            quote = FALSE, sep = ",", row.names = FALSE)


rm(list = ls())




#############################################################
# table 8151
#############################################################

TABLECODE8151 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Number of dependent children and total number of children one parent families/TABLECODE8151_Data_5dc79e76-3c16-46cc-b089-1629feb346e0.csv")

TABLECODE8151 <- TABLECODE8151 %>%
  select(-c(Year, Flags))

write.table(TABLECODE8151, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8151.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())






#############################################################
# table 8153
#############################################################

SoleParents <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Sex of sole parent with dependent children families in occupied private dwellings/TABLECODE8153_Data_eb5ff063-cb1d-483e-9d7e-32ae56967124.csv")

SoleParents <- SoleParents %>%
  select(-c(Year, Flags))

write.table(SoleParents, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8153.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())






#############################################################
# table 8158
#############################################################

Grandparents <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type for grandparents in a parental role in families 2013 census/TABLECODE8158_Data_5d605f25-7a1f-4cbd-b850-189ddedf8f2e.csv")

Grandparents <- Grandparents %>%
  select(-Flags)

write.table(Grandparents, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8158.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())






####################################################################################################
# Table 8160
####################################################################################################


FamilyTypes <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type with type of couple/TABLECODE8160_Data_c9558eb4-30f9-4edc-9ad5-c091ac76f6d1.csv")

FamilyTypes <- FamilyTypes %>%
  select(-Flags)


write.table(FamilyTypes, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8160.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())




#############################################################
# table 8161
#############################################################

TABLECODE8161 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Age group of people in same-sex couples in occupied private dwellings/TABLECODE8161_Data_9ae7be30-bdba-49ab-9bff-50b093df0335.csv", stringsAsFactors=FALSE)

TABLECODE8161 <- TABLECODE8161 %>%
  select(-Flags)


write.table(TABLECODE8161, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8161.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


rm(list = ls())





####################################################################################################
####################################################################################################
# Table 8165
####################################################################################################
####################################################################################################

HouseholdType <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Household composition for households/TABLECODE8165_Data_HouseholdCompositionforHouseholds.csv")

HouseholdType <- HouseholdType %>%
  select(-c(Flags, Year))


write.table(HouseholdType, file = "~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/Table8165.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


HouseholdTotals <- HouseholdType %>%
  filter(grepl('Total', `Household composition`)) 

NumHouseholds <- as.numeric(HouseholdTotals[1,4])

HouseholdNoTotals <- HouseholdType %>%
  filter(!(grepl('Total', `Household composition`)) & !(grepl('Average', `Household composition`))) %>%
  mutate(Proportion = Value/NumHouseholds) %>%
  arrange(desc(Proportion))

HouseholdTotals <- HouseholdTotals %>%
  mutate(Proportion = Value/NumHouseholds)

rm(list = ls())