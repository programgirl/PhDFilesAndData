#clear workspace
#  rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(conflicted)
library(stringi)

conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")



File3PopulationAge <- readRDS("PhDRData/File3PopulationAge.rds")










####################################################################################################
####################################################################################################
# what proportion of 5-year-olds are in school?
# no MinEd data on this
####################################################################################################
####################################################################################################

# bring in school roll data
SchoolRollDataAllSchools <- read.csv("OriginalDataFiles/Schools2013MOEData.csv", na.strings="..C")

# get the Cant Region counts
SchoolRoll2013CR <- SchoolRollDataAllSchools %>%
  filter(Regional.Council == "Canterbury Region") %>%
  select(c(School.ID, School.Name, Type, Gender, Regional.Council, Education.Region, TA.with.Auckland.Local.Board, Age.5)) %>%
  filter(Age.5 > 0)

SchoolRoll2013TD <- SchoolRollDataAllSchools %>%
  filter(TA.with.Auckland.Local.Board == "Timaru District") %>%
  select(c(School.ID, School.Name, Type, Gender, Regional.Council, Education.Region, TA.with.Auckland.Local.Board, Age.5)) %>%
  filter(Age.5 > 0)

# and the number of 5-year-olds in school is:

#TD:
RollTD <- as.numeric(sum(SchoolRoll2013TD$Age.5))
#CR:
RollCR <- as.numeric(sum(SchoolRoll2013CR$Age.5))

# bring in the counts for five-year-olds, census 2013

# TD counts of 5-year-olds
TimaruDistrictAgeTotals8001 <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex groups/TABLECODE8001_Data_b339765f-cf78-4e38-aa57-332c2e066b02.csv")

CountTD <- TimaruDistrictAgeTotals8001 %>%
  filter(Age.group == "Five years") %>%
  select(Value) %>%
  pull()
  
round((RollTD/TimaruDistrictAgeTotals8001[29,5]),2)


CRAgePyramid <- read_csv("OriginalDataFiles/Usually Resident/Canterbury Region/TABLECODE8001_Data_6001290a-09ac-4761-b81c-d1f1a2887506.csv", col_types = cols(Area = col_skip(), Flags = col_skip()))

CRProp <- round((RollCR/sum(CRAgePyramid[11,4],CRAgePyramid[12,4])),2)


# how many five year olds in synthetic population?
KidsUnder5 <- File4AllPeople %>%
  filter(Age == 5)

# expected prop in school
nrow(KidsUnder5) * CRProp

rm(list = ls())




















####################################################################################################
####################################################################################################
# Get school leaver proportions
####################################################################################################
####################################################################################################


####################################################################################################
# Ministry of Education school leaver data
####################################################################################################


AllSchoolLeavers <- read_csv("Ministry of Education files/Machine-Readable-School-Leavers.csv")




####################################################################################################
# Timaru District
TDSchoolLeavers <- AllSchoolLeavers %>%
  filter(`Region: Territorial Authority`=="Timaru District",
         `Student: Ethnic Group` == "Total",
         `School: School Sector` != "Not Applicable") %>%
  mutate(Age = stri_extract_last_regex(`Student: Student Age`, "([0-9]+)"),
         YearLeft = as.numeric(sub("([0-9]+).*$", "\\1", `Year: Left School`)),
         Sex = as.character(`Student: Student Gender`),
         Count = `Students (∑ Values)`) %>%
  select(YearLeft, Sex, Age, Count) %>%
  mutate(Age = as.numeric(Age))

TDLeaversSummary <- TDSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup()



# Bring in the Timaru District data from the 2013 census
TimaruDistrictSexAge <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex/TABLECODE8001_Data_1746e3c7-2263-4fd5-9dc1-6aef89a0874d.csv")

# remove extraneous columns - we already know it's Timaru District data for 2013 - and extraneous rows
# remove ages 105 and up as there are zero people that age
# remove all totals
patterns <- c("-", "Total", "Median", "105", "106", "107", "108", "109", "110", "111", "112",
              "113", "114", "115", "116", "117", "118", "119", "120", "over")
TimaruDistrictSexAgeNoTotals <- TimaruDistrictSexAge %>%
  select(Age.group, Sex, Value) %>%
  filter(!grepl(paste(patterns, collapse="|"), Age.group))

#need to fix age so it is integer, not character
#remove " years" from those rows
TimaruDistrictSexAgeNoTotals$IntegerAge <- as.numeric(gsub("[^[:digit:]]", "", TimaruDistrictSexAgeNoTotals$Age))

# fix the years that are not digits, but are words
TimaruDistrictSexAgeNoTotals$IntegerAge <- ifelse(TimaruDistrictSexAgeNoTotals$Age=="Less than one year", 0,
                                                  ifelse(TimaruDistrictSexAgeNoTotals$Age=="One year",1,
                                                         ifelse(TimaruDistrictSexAgeNoTotals$Age=="Two years",2,
                                                                ifelse(TimaruDistrictSexAgeNoTotals$Age=="Three years",3,
                                                                       ifelse(TimaruDistrictSexAgeNoTotals$Age=="Four years",4,
                                                                              ifelse(TimaruDistrictSexAgeNoTotals$Age=="Five years",5,
                                                                                     ifelse(TimaruDistrictSexAgeNoTotals$Age=="Six years",6,
                                                                                            ifelse(TimaruDistrictSexAgeNoTotals$Age=="Seven years",7,
                                                                                                   ifelse(TimaruDistrictSexAgeNoTotals$Age=="Eight years",8,
                                                                                                          ifelse(TimaruDistrictSexAgeNoTotals$Age=="Nine years",9, TimaruDistrictSexAgeNoTotals$IntegerAge))))))))))

#delete factor age
TimaruDistrictSexAgeNoTotals$Age.group <- NULL

rm(patterns, TimaruDistrictSexAge)



# get the proportions who have left for TD
TDSchoolLeaversCumsum <- TDLeaversSummary %>%
  filter(YearLeft <= 2012) %>%
  mutate(Deduction = 2012 - YearLeft,
         CurrentAge = Age + Deduction) %>%
  group_by(Sex, CurrentAge) %>%
  summarise(TotalLeaverCount = sum(Total)) %>%
  filter(CurrentAge <= 18)


TDAdolescentCounts <- TimaruDistrictSexAgeNoTotals %>%
  filter(IntegerAge %in% c(15,16,17,18)) %>%
  rename(CurrentAge = IntegerAge)

TDAdolescentPercents <- left_join(TDSchoolLeaversCumsum, TDAdolescentCounts, by = c("CurrentAge", "Sex")) %>%
  mutate(PropLeft = TotalLeaverCount/Value)


TDAdolescentPercents










####################################################################################################
# Canterbury Region

CRSchoolLeavers <- AllSchoolLeavers %>%
  filter(`Region: Regional Council`=="Canterbury Region",
         `Student: Ethnic Group` == "Total",
         `School: School Sector` != "Not Applicable") %>%
  mutate(Age = stri_extract_last_regex(`Student: Student Age`, "([0-9]+)"),
         YearLeft = as.numeric(sub("([0-9]+).*$", "\\1", `Year: Left School`)),
         Sex = as.character(`Student: Student Gender`),
         Count = `Students (∑ Values)`) %>%
  select(YearLeft, Sex, Age, Count) %>%
  mutate(Age = as.numeric(Age))

CRLeaversSummary <- CRSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup()

# Bring in the Canterbury Region data from the 2013 census
CRAgePyramid <- read_csv("OriginalDataFiles/Usually Resident/Canterbury Region/TABLECODE8001_Data_6001290a-09ac-4761-b81c-d1f1a2887506.csv", col_types = cols(Area = col_skip(), Flags = col_skip()))

CRPyramid <- CRAgePyramid %>%
  filter(`Age group` %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years")) %>%
  mutate(CurrentAge = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`)))


# get the proportions who have left for CR
CRSchoolLeaversCumsum <- CRLeaversSummary %>%
  filter(YearLeft <= 2012) %>%
  mutate(Deduction = 2012 - YearLeft,
         CurrentAge = Age + Deduction) %>%
  group_by(Sex, CurrentAge) %>%
  summarise(TotalLeaverCount = sum(Total)) %>%
  filter(CurrentAge <= 18)

CRAdolescentPercents <- left_join(CRSchoolLeaversCumsum, CRPyramid, by = c("CurrentAge", "Sex")) %>%
  mutate(PropLeft = TotalLeaverCount/Value)

CRExitsByYear <- CRLeaversSummary %>%
  filter(YearLeft <= 2012,
         Age < 19) %>%
  group_by(YearLeft, Sex) %>%
  summarise(NumLeavers = sum(Total))

CRFemaleExits <- CRExitsByYear %>%
  filter(Sex == "Female") %>%
  rename(NumFLeaders = NumLeavers)

CRMaleExits <- CRExitsByYear %>%
  filter(Sex == "Male") %>%
  rename(NumMLeaders = NumLeavers)

CRBothSexExits <- left_join(CRFemaleExits, CRMaleExits, by = "YearLeft") %>%
  mutate(PropOfFemales = round(NumFLeaders/(NumFLeaders + NumMLeaders),3))

TABLECODE230 <- read_csv("CantRegionCensusUsuallyResidentPop199620012006/Age Group by Sex/TABLECODE230_Data_6ab1aeef-664b-4394-8e95-89bb1aa28a1f.csv", col_types = cols(Flags = col_skip()))

CRAgePyramid15to19F <- CRPyramid %>%
  filter(`Age group` %in% c("15 years", "16 years", "17 years", "18 years", "19 years"),
         Sex == "Female") %>%
  group_by(Sex, Year) %>%
  summarise(NumF15to19 = sum(Value)) 
  
CRAgePyramid15to19M <- CRPyramid %>%
  filter(`Age group` %in% c("15 years", "16 years", "17 years", "18 years", "19 years"),
         Sex == "Male") %>%
  group_by(Sex, Year) %>%
  summarise(NumM15to19 = sum(Value)) 

CRBothSex2013 <- left_join(CRAgePyramid15to19F, CRAgePyramid15to19M, by = c("Year")) %>%
  mutate(PropOfFemales = NumF15to19/(NumF15to19 +NumM15to19))


CR1996To2006F <- TABLECODE230 %>%
  filter(Sex == "Female") %>%
  rename(NumF15to19 = Value)

CR1996To2006M <- TABLECODE230 %>%
  filter(Sex == "Male") %>%
  rename(NumM15to19 = Value)

CRBothSex1996To2006 <- left_join(CR1996To2006F, CR1996To2006M, by = c("Area", "Year")) %>%
  select(c(Year, Sex.x, NumF15to19, Sex.y, NumM15to19)) %>%
  mutate(PropOfFemales = NumF15to19/(NumF15to19 +NumM15to19))

PropFemales1996to2013 <- bind_rows(CRBothSex1996To2006, CRBothSex2013)




####################################################################################################
# New Zealand

NZSchoolLeavers <- AllSchoolLeavers %>%
  filter(`Student: Ethnic Group` == "Total",
         `School: School Sector` != "Not Applicable") %>%
  mutate(Age = stri_extract_last_regex(`Student: Student Age`, "([0-9]+)"),
         YearLeft = as.numeric(sub("([0-9]+).*$", "\\1", `Year: Left School`)),
         Sex = as.character(`Student: Student Gender`),
         Count = `Students (∑ Values)`) %>%
  select(YearLeft, Sex, Age, Count) %>%
  mutate(Age = as.numeric(Age))

NZLeaversSummary <- NZSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup()

# Bring in the Canterbury Region data from the 2013 census
NZAgePyramid <- read_csv("OriginalDataFiles/Usually Resident/New Zealand/TABLECODE8001_Data_b4b82516-2eb6-495d-9ddf-7485e5147e5c.csv", col_types = cols(Area = col_skip(), Flags = col_skip()))

NZPyramid <- NZAgePyramid %>%
  filter(`Age group` %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years")) %>%
  mutate(CurrentAge = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`)))

# get the proportions who have left for CR
NZSchoolLeaversCumsum <- NZLeaversSummary %>%
  filter(YearLeft <= 2012) %>%
  mutate(Deduction = 2012 - YearLeft,
         CurrentAge = Age + Deduction) %>%
  group_by(Sex, CurrentAge) %>%
  summarise(TotalLeaverCount = sum(Total)) %>%
  filter(CurrentAge <= 18)

NZAdolescentPercents <- left_join(NZSchoolLeaversCumsum, NZPyramid, by = c("CurrentAge", "Sex")) %>%
  mutate(PropLeft = TotalLeaverCount/Value)




####################################################################################################
# Delete all files except for the Canterbury Region Proportions

rm(AllSchoolLeavers, CRAgePyramid, CRLeaversSummary, CRPyramid, CRSchoolLeavers, 
   CRSchoolLeaversCumsum, NZAdolescentPercents, NZAgePyramid, NZLeaversSummary, NZPyramid,
   NZSchoolLeavers, NZSchoolLeaversCumsum, TDAdolescentCounts, TDAdolescentPercents, 
   TDLeaversSummary, TDSchoolLeavers, TDSchoolLeaversCumsum, TimaruDistrictSexAgeNoTotals, CR1996To2006F, CR1996To2006M,
   CRAgePyramid15to19F, CRAgePyramid15to19M, CRBothSex1996To2006, CRBothSex2013, CRBothSexExits, CRFemaleExits,
   CRMaleExits, PropFemales1996to2013, TABLECODE230, CRExitsByYear)











####################################################################################################
# Add in school indicator
####################################################################################################

AllSchoolLeavers <- read_csv("Ministry of Education files/Machine-Readable-School-Leavers.csv")

CRSchoolLeavers <- AllSchoolLeavers %>%
  filter(`Region: Regional Council`=="Canterbury Region",
         `Student: Ethnic Group` == "Total",
         `School: School Sector` != "Not Applicable") %>%
  mutate(Age = stri_extract_last_regex(`Student: Student Age`, "([0-9]+)"),
         YearLeft = as.numeric(sub("([0-9]+).*$", "\\1", `Year: Left School`)),
         Sex = as.character(`Student: Student Gender`),
         Count = `Students (∑ Values)`) %>%
  select(YearLeft, Sex, Age, Count) %>%
  mutate(Age = as.numeric(Age))

CRLeaversSummary <- CRSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup()

CRAgePyramid <- read_csv("OriginalDataFiles/Usually Resident/Canterbury Region/TABLECODE8001_Data_6001290a-09ac-4761-b81c-d1f1a2887506.csv", col_types = cols(Area = col_skip(), Flags = col_skip()))

CRPyramid <- CRAgePyramid %>%
  filter(`Age group` %in% c("15 years", "16 years", "17 years", "18 years")) %>%
  mutate(CurrentAge = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`)))



# bring in the synthetic population

File3PopulationAge <- readRDS("PhDRData/File3PopulationAge.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# There are 18-year-olds in one-person households, these cannot be in school
# Assume that any 18-year-olds who are partnered also cannot be in school
# Remove these two types of people from the dataframe so that their school indicator will be "not in school"

# first, get all 18-year-olds

Eighteens <- File3PopulationAge %>%
  filter(Age == 18)


# then remove the ones who live in one-person households or are partnered

Excluded18s <- Eighteens %>%
  filter(UsualResidents == "One Usual Resident" | PartnershipStatus == "Partnered")

table(Excluded18s$Sex)

# There are 48 males and 51 females who are excluded

MalesToDelete <- as.numeric(Excluded18s %>%
  filter(Sex == "Male") %>%
  summarise(NumMales18 = n()) %>%
  pull(NumMales18))

FemalesToDelete <- as.numeric(Excluded18s %>%
  filter(Sex == "Female") %>%
  summarise(NumFemales18 = n()) %>%
  pull(NumFemales18))

# Adjust the CRLeaversSummary dataset by decreasing the 2012 leaver counts to reflect the
# 99 people ( nrow(Excluded18s) ) who must, by definition, be school leavers
# these are in the Excluded18s data frame

CRLeaversSummary <- CRLeaversSummary %>%
  mutate(ModifiedCount = ifelse(Sex == "Male" & YearLeft == 2012 & Age == 18, Total - MalesToDelete, 
                                ifelse(Sex == "Female" & YearLeft == 2012 & Age == 18, Total - FemalesToDelete, Total)))



#reduced data frame for the function

RemainingPeople <- File3PopulationAge %>%
  filter(!(ID %in% Excluded18s$ID))

# uses the schoolind script from PopulateR

File4PopulationSchoolInd <- addind(RemainingPeople, "ID", "Sex", "Age", 2012, 5, 18, CRLeaversSummary, "Sex", "Age",
                                 "YearLeft", "ModifiedCount", CRPyramid, "Sex", "CurrentAge", "Value",
                                 "EducationStatus", userseed = TheRandomSeeds[59])  #################### seed 59 %%%%%%%%%%%%%%%%


table(File4PopulationSchoolInd$AgeGroup, File4PopulationSchoolInd$EducationStatus)

# Add the omitted 18-year-olds back in
Excluded18s <- Excluded18s %>%
  mutate(EducationStatus = "N")

# append to the final file

File4SchoolIndAdded <- bind_rows(File4PopulationSchoolInd, Excluded18s) %>%
  mutate(EducationStatus =forcats::fct_relevel(EducationStatus, c("N", "Y")))


# get the proportions of 15-18-year-olds in school now that the full data frame exists
tempdf <- File4SchoolIndAdded %>%
  filter(between(Age, 15, 18)) %>%
  group_by(Sex, Age, EducationStatus) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n),
         freqRounded = round(freq,2)) %>%
  filter(EducationStatus == "N")

tempdf

# underlyingcounts <- File7SchoolInd %>%
#   filter(between(Age, 15, 18)) %>%
#   group_by(Age, Sex) %>%
#   summarise(n = n()) 

# save the data
saveRDS(File4SchoolIndAdded, file = "PhDRData/SchoolSetupWork/File4SchoolIndAdded.rds")


rm(list = ls())















































































####################################################################################################
####################################################################################################
# Reallocate working hours so that students in school are not working incompatibly high hours
####################################################################################################
####################################################################################################

####################################################################################################
# Uncorrected hours worked
####################################################################################################

####################################################################################################

File4SchoolIndAdded <- readRDS("PhDRData/SchoolSetupWork/File4SchoolIndAdded.rds")

# show that all kids under 15 are coded to not working

table(File4SchoolIndAdded$AgeGroup, File4SchoolIndAdded$HoursWorked)

# Initial prop of hours worked for the 15-17 and 18-24 years age groups

AgeBandsOfInterest <- File4SchoolIndAdded %>%
  filter(AgeGroup %in% c("15 - 17 Years", "18 - 24 Years")) %>%
  group_by(AgeGroup, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n), 2)) %>%
  ungroup()

AgeBandsOfInterest


####################################################################################################
# graph of hours by school status, those aged 15 through 18
# prop working within sex

WorkingHrsSchoolSts <- File4SchoolIndAdded %>%
  filter(between(Age, 15, 18))

WorkingHoursSummary <- WorkingHrsSchoolSts %>%
  group_by(Sex,EducationStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

# get the working adolescents counts from WorkingHoursSunmmary

HoursCats <- WorkingHoursSummary %>%
  tidyr::expand(HoursWorked, Sex, EducationStatus)

WorkingHoursSummary2 <- left_join(HoursCats, WorkingHoursSummary) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)),
         InEducation = ifelse(EducationStatus == "N", "Not in school", "In school")) 


# compare males and females separately
WorkingHrsSummaryF <- WorkingHoursSummary2 %>%
  filter(Sex=="Female")

WorkingHrsSummaryM <- WorkingHoursSummary2 %>%
  filter(Sex == "Male")

WorkingVsSchoolingF1 <- ggplot(WorkingHrsSummaryF, aes(x=HoursWorked, y = freq, fill = InEducation)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid",'grey67')) +
  scale_y_continuous(breaks = c(.2, .4, .6 , .8),
                     limits = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of adolescents") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50+", "Other")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))


WorkingVsSchoolingM1 <- ggplot(WorkingHrsSummaryM, aes(x=HoursWorked, y = freq, fill = InEducation)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("blue",'grey67')) +
  scale_y_continuous(breaks = c(.2, .4, .6 , .8),
                     limits = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of adolescents") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50+", "Other")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(WorkingVsSchoolingF1, file="WorkingVsSchoolingF1.pdf", width=9.32, height=7.78, units="in")

#  ggsave(WorkingVsSchoolingM1, file="WorkingVsSchoolingM1.pdf", width=9.32, height=7.78, units="in")

WorkingHrsSummaryF

WorkingHrsSummaryM


rm(list = ls())















####################################################################################################
# Correct the hours - hoursfix function
####################################################################################################

File4SchoolIndAdded <- readRDS("PhDRData/SchoolSetupWork/File4SchoolIndAdded.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

TheGroups <- c("Sex", "AgeGroup", "PartnershipStatus", "UsualResidents")

File4CorrectHrs <- fixhours(File4SchoolIndAdded, "ID", "EducationStatus", "HoursWorked", 3, TheGroups, 
                            userseed = TheRandomSeeds[60])            #################### seed 60 %%%%%%%%%%%%%%%%


AgeBandsOfInterest2 <- File4CorrectHrs %>%
  filter(AgeGroup %in% c("15 - 17 Years", "18 - 24 Years")) %>%
  group_by(AgeGroup, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

AgeBandsOfInterest2

####################################################################################################
# graph of hours by school status, those aged 15 through 18
# prop working within sex

WorkingHrsSchoolSts2 <- File4CorrectHrs %>%
  filter(between(Age, 15, 18))

WorkingHoursSummary2 <- WorkingHrsSchoolSts2 %>%
  group_by(Sex, EducationStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

# get the working adolescents counts from WorkingHoursSunmmary

HoursCats <- WorkingHoursSummary2 %>%
  tidyr::expand(HoursWorked, Sex, EducationStatus)

WorkingHoursSummary2 <- WorkingHoursSummary2 %>%
  right_join(HoursCats) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)),
         InEducation = ifelse(EducationStatus == "N", "Not in school", "In school")) 



WorkingHrsSummaryF2 <- WorkingHoursSummary2 %>%
  filter(Sex=="Female")

WorkingHrsSummaryM2 <- WorkingHoursSummary2 %>%
  filter(Sex == "Male")

WorkingVsSchoolingF2 <- ggplot(WorkingHrsSummaryF2, aes(x=HoursWorked, y = freq, fill = InEducation)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid",'grey67')) +
  scale_y_continuous(breaks = c(.2, .4, .6 , .8),
                     limits = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of adolescents") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50+", "Other")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))


WorkingVsSchoolingM2 <- ggplot(WorkingHrsSummaryM2, aes(x=HoursWorked, y = freq, fill = InEducation)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("blue",'grey67')) +
  scale_y_continuous(breaks = c(.2, .4, .6 , .8),
                     limits = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of adolescents") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50+", "Other")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(WorkingVsSchoolingF2, file="WorkingVsSchoolingF2.pdf", width=9.32, height=7.78, units="in")

#  ggsave(WorkingVsSchoolingM2, file="WorkingVsSchoolingM2.pdf", width=9.32, height=7.78, units="in")


# show that the hours worked remain the same for each age group
table(File4SchoolIndAdded$AgeGroup, File4SchoolIndAdded$HoursWorked)
table(File4CorrectHrs$AgeGroup, File4CorrectHrs$HoursWorked)


# save the synthetic population file
saveRDS(File4CorrectHrs, file = "PhDRData/File4CorrectHrs.rds")
  
rm(list = ls())





####################################################################################################
# last random seed vector used was  #################### seed 60 %%%%%%%%%%%%%%%%
####################################################################################################

