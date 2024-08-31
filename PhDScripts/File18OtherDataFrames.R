
#  rm(list = ls())

library(dplyr)
library(purrr)


SyntheticPopulation <- readRDS("PhDRData/SyntheticPopulation.rds")

File13SchoolsDataFrame <- readRDS("PhDRData/File13SchoolsDataFrame.rds")

SingleSchools <- File13SchoolsDataFrame %>%
  select(SchoolID, SchoolName) %>%
  distinct()





################################################################################
################################################################################
# For the kids, schools add IndCode and IndName, need this for effect of closing schools
# for the school clone
################################################################################
################################################################################


SchoolKidsInEmp <- SyntheticPopulation %>%
  filter(EducationStatus == "Y" & !HoursWorked == "No Hours") %>%
  mutate(PlaceTwo = Company)


# ECE and school IndCodes and IndNames to kids in ECE and School

KidsInECE <- SyntheticPopulation %>%
  filter(ECEIndicator == "Child") %>%
  select(-c(IndCode, IndName)) %>%
  mutate(PlaceTwo = ECEProvider)

# get ECECodes for IndName and IndCode

ECECodes <- SyntheticPopulation %>%
  filter(ECEIndicator == "Staff") %>%
  select(IndCode, IndName, ECEProvider) %>%
  unique()

# add the ECE industry information to the children

KidsInECEPlusCodes <- left_join(KidsInECE, ECECodes, by = "ECEProvider")

# do the same for the school kids

#school
SchoolsWithIndCodes <- SyntheticPopulation %>%
  filter(Company %in% File13SchoolsDataFrame$SchoolID) %>%
  select(Company, IndCode, IndName) %>%
  unique()

SchoolKids <- SyntheticPopulation %>%
  filter(EducationStatus == "Y") %>%
  select(-c(IndCode, IndName))

SchoolKidsWithCodes <- left_join(SchoolKids, SchoolsWithIndCodes, by = c("schoolID" = "Company")) %>%
  mutate(PlaceTwo = schoolID,
         HoursWorked = "No Hours")

Kids <- bind_rows(SchoolKidsInEmp, KidsInECEPlusCodes, SchoolKidsWithCodes)




################################################################################
################################################################################
# For the others, there is either no place outside home or no place outside school 
# still need to fix the company name for non-working school students and teachers
################################################################################
################################################################################


Others <- SyntheticPopulation %>%
  filter(!ID %in% Kids$ID)

# get teachers and students

Staff <- Others %>%
  filter(Company %in% SingleSchools$SchoolID) %>%
  mutate(PlaceTwo = Company)

OtherEmployed <- Others %>%
  filter(EducationStatus == "N", !HoursWorked == "No Hours", !ID %in% Staff$ID) %>%
  mutate(PlaceTwo = Company)

Remainder <- Others %>%
  filter(EducationStatus == "N", !ID %in% c(Staff$ID, OtherEmployed$ID)) %>%
  mutate(PlaceTwo = ifelse(ECEIndicator == "Child", ECEProvider,
                           ifelse(RestHomeIndicator == "Resident", RestHome, HouseholdID)))




ABMPopInitial <- bind_rows(Kids, Staff, OtherEmployed, Remainder) %>%
  select(-c(Sex, AgeGroup, UsualResidents, EducationStatus, schoolID, Company, ECEIndicator,
            ECEProvider, RestHomeIndicator, RestHome)) %>%
  rename(Sex = SexMarker) %>%
  mutate(Working = ifelse(HoursWorked == "No Hours", 0, 
                          ifelse(HoursWorked == "1-9 Hours Worked", 1,
                                 ifelse(HoursWorked == "10-19 Hours Worked", 2,
                                        ifelse(HoursWorked == "20-29 Hours Worked", 3,
                                               ifelse(HoursWorked == "30-39 Hours Worked", 4,
                                                      ifelse(HoursWorked == "40-49 Hours Worked", 5, 6))))))) %>%
  select(-HoursWorked)

# re order the columsn to something vaguely sensible
ABMPop <- ABMPopInitial %>%
  select(ID, Sex, Age, PartnershipStatus, Type, HouseholdID, Working, PlaceTwo, IndCode, IndName)

# test for NAs
colSums(is.na(ABMPop)) > 0


saveRDS(ABMPop, file = "PhDRData/ABMPop.rds")













################################################################################
################################################################################
# CovaPop creation
################################################################################
################################################################################

ABMPop <- readRDS("PhDRData/ABMPop.rds")

# bring in existing contacts data base constructed in File17 script

File17ContactsDataframe <- readRDS("PhDRData/File17ContactsDataframe.rds")

# IndExcludes <- data.frame(DontUse = c("A011200", "C111100"))


CovaPop <- ABMToCova(ABMPop, ABMID = "ID", ABMAge = "Age", place1 = "HouseholdID", place2 = "PlaceTwo", ECE="N", 
                     PSchool = "Y", SSchool = "Y", contacts = File17ContactsDataframe)

CovaHouseholds <- CovaPop$h
CovaSchools <- CovaPop$s
CovaWorkplaces <- CovaPop$w
CovaContacts <- CovaPop$c
CovaAges <- CovaPop$age


write.csv(CovaHouseholds, file = "PhDRData/CovaHouseholds.csv", row.names = FALSE)
write.csv(CovaSchools, file = "PhDRData/CovaSchools.csv", row.names = FALSE)
write.csv(CovaWorkplaces, file = "PhDRData/CovaWorkplaces.csv", row.names = FALSE)
write.csv(CovaContacts, file = "PhDRData/CovaContacts.csv", row.names = FALSE)
write.csv(CovaAges, file = "PhDRData/CovaAges.csv", row.names = FALSE)


# THIS WAS AGE-BASED BETA TESTING, BETA CHANGES LOGIC FOR THE FOUR LAYERS NOT YET THOUGHT THROUGH
# 
# # start the Covasim script testing/construction
# 
# TheBetas <- data.frame(Age = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 65, 66, 67, 68, 69, 70),
#                        beta = c(rep(0.5,13), rep(2.1, 6)))
# 
# 
# CovaPop <- ABMToCova(ABMPop, ABMID = "ID", place1 = "HouseholdID", place2 = "PlaceTwo",
#                                    contacts = File17ContactsDataframe)
# 
# # what is the average number of contacts per person
# 
# nrow(CovaPop)/nrow(File16SyntheticPopulation)
# 
# 
# saveRDS(CovaPop, file = "PhDRData/CovaPop.rds")


# export CovaPop as csv
# 
# write.csv(CovaPop, file = "PhDRData/CovaPop.csv", row.names = FALSE)

 