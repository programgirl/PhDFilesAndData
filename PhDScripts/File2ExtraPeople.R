#clear workspace
#  rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)

File1LongData <- readRDS("PhDRData/File1LongData.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")
AllTotals <- readRDS("PhDRData/AllTotals.rds")
NoTotals <- readRDS("PhDRData/NoTotals.rds")

# get total total

EveryoneTotalOriginal <- as.numeric(AllTotals[5736,6])

EveryoneDiff <- EveryoneTotalOriginal - nrow(File1LongData)






####################################################################################################
####################################################################################################
# Check counts against synthetic population
####################################################################################################
####################################################################################################


SynthPopCounts <- File1LongData %>%
  group_by(UsualResidents) %>%
  summarise(SPCount = n()) 
  
PopDataCounts <- AllTotals %>%
  filter(Sex == "Total", AgeGroup == "Total", PartnershipStatus == "Total", HoursWorked == "Total")


CombinedTotals <- left_join(SynthPopCounts, PopDataCounts, by = "UsualResidents") %>%
  select(UsualResidents, Count, SPCount) %>%
  mutate(NumSuppressed = Count - SPCount,
    PropSuppressed = round(1-(SPCount/Count),3))

# how many lost from 6, 7, 8 person households

Totals678 <- CombinedTotals %>%
  filter(UsualResidents %in% c("Six Usual Residents", "Seven Usual Residents", "Eight or More Usual Residents"))

# in popdata
sum(Totals678$Count)
sum(Totals678$SPCount)

Totals12345 <- CombinedTotals %>%
  filter(!UsualResidents %in% c("Six Usual Residents", "Seven Usual Residents", "Eight or More Usual Residents"))

CountPopData <- sum(Totals12345$Count)
CountSPData <- sum(Totals12345$SPCount)

# new prop
CountSPData/CountPopData

MaxUnsuppressed <- 258

# proportion suppressed rows by household size

####################################################################################################
# One person
####################################################################################################

OnePersonTots <- NoTotals %>%
  filter(UsualResidents == "One Usual Resident")

OnePersonNoNA <- OnePersonTots %>%
  filter(!(is.na(Count)))

OnePersonPropNA <- round(1 -(nrow(OnePersonNoNA) / 224), 3) # contains no-one under age 18


####################################################################################################
# Two people
####################################################################################################

TwoPersonTots <- NoTotals %>%
  filter(UsualResidents == "Two Usual Residents")

TwoPersonNoNA <- TwoPersonTots %>%
  filter(!(is.na(Count)))

TwoPersonPropNA <- round(1-(nrow(TwoPersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Three people
####################################################################################################

ThreePersonTots <- NoTotals %>%
  filter(UsualResidents == "Three Usual Residents")

ThreePersonNoNA <- ThreePersonTots %>%
  filter(!(is.na(Count)))

ThreePersonPropNA <- round(1-(nrow(ThreePersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Four people
####################################################################################################

FourPersonTots <- NoTotals %>%
  filter(UsualResidents == "Four Usual Residents")

FourPersonNoNA <- FourPersonTots %>%
  filter(!(is.na(Count)))

FourPersonPropNA <- round(1-(nrow(FourPersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Five people
####################################################################################################

FivePersonTots <- NoTotals %>%
  filter(UsualResidents == "Five Usual Residents")

FivePersonNoNA <- FivePersonTots %>%
  filter(!(is.na(Count)))

FivePersonPropNA <- round(1-(nrow(FivePersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Six people
####################################################################################################

SixPersonTots <- NoTotals %>%
  filter(UsualResidents == "Six Usual Residents")

SixPersonNoNA <- SixPersonTots %>%
  filter(!(is.na(Count)))

SixPersonPropNA <- round(1-(nrow(SixPersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Seven people
####################################################################################################

SevenPersonTots <- NoTotals %>%
  filter(UsualResidents == "Seven Usual Residents")

SevenPersonNoNA <- SevenPersonTots %>%
  filter(!(is.na(Count)))

SevenPersonPropNA <- round(1-(nrow(SevenPersonNoNA) / MaxUnsuppressed), 3)


####################################################################################################
# Eight people
####################################################################################################

EightPersonTots <- NoTotals %>%
  filter(UsualResidents == "Eight or More Usual Residents")

EightPersonNoNA <- EightPersonTots %>%
  filter(!(is.na(Count)))

EightPersonPropNA <- round(1-(nrow(EightPersonNoNA) / MaxUnsuppressed), 3)





















####################################################################################################
####################################################################################################
# Delete six, seven, and eight or more household sizes
####################################################################################################
####################################################################################################

File2ReducedPopulation <- File1LongData %>%
  filter(!UsualResidents %in% c("Six Usual Residents", "Seven Usual Residents", "Eight or More Usual Residents"))



saveRDS(File2ReducedPopulation, file = "PhDRData/ExtraPeople/File2ReducedPopulation.rds")


ReducedSNZPop <- AllTotals %>%
  filter(Sex == "Total", AgeGroup == "Total", PartnershipStatus == "Total", HoursWorked == "Total", 
         !UsualResidents %in% c("Six Usual Residents", "Seven Usual Residents", "Eight or More Usual Residents", "Total"))


ReducedSNZTotal <- sum(ReducedSNZPop$Count)

NewProp <- round(nrow(File2ReducedPopulation)/ReducedSNZTotal, 3)


rm(list = ls())

























####################################################################################################
####################################################################################################
# Add extra people into the remaining households
####################################################################################################
####################################################################################################

AllTotals <- readRDS("PhDRData/AllTotals.rds")
File2ReducedPopulation <- readRDS("PhDRData/ExtraPeople/File2ReducedPopulation.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")



####################################################################################################
# One person households
####################################################################################################

StatsNZ1P <- AllTotals %>%
  filter(UsualResidents == "One Usual Resident") %>%
  filter(!(AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "15 - 17 Years")))

TotalOriginal1P <- as.numeric(StatsNZ1P[424,6])

SynPop1P <- File2ReducedPopulation %>%
  filter(UsualResidents == "One Usual Resident")

# what is the difference?
DiffP1 <- TotalOriginal1P - nrow(SynPop1P)

SubMarginals1P <- SynPop1P %>%
  select(-PartnershipStatus) %>%
  count(Sex, AgeGroup, HoursWorked) %>%
  mutate(HoursWorked = as.character(HoursWorked))


Original1PHrsByAge <- StatsNZ1P %>%
  filter(PartnershipStatus == "Total", !(HoursWorked == "Total"), !(Sex == "Total"), !(AgeGroup == "Total")) %>%
  select(-PartnershipStatus) %>%
  mutate(Count = ifelse(is.na(Count), 3, Count),
         HoursWorked = as.character(HoursWorked))


Merged1PAgeHrs <- left_join(Original1PHrsByAge, SubMarginals1P, by = c("Sex", "AgeGroup", "HoursWorked")) %>%
  filter(!(Sex == "Total"), !(AgeGroup == "Total")) %>%
  mutate(n = ifelse(is.na(n), 0, n), 
         Diff = Count - n) 

# filter out unrealistic combinations
Merged1PAgeHrs <- Merged1PAgeHrs %>%
  filter(Diff > 0)

nrow(Merged1PAgeHrs)

# remove all unrealistic combinations

FinalMarginalTotals <- Merged1PAgeHrs %>%
  filter(!AgeGroup %in% c("75 - 84 Years", "85 Years and Over")) %>%
  filter(! (Sex == "Female" & AgeGroup == "65 - 74 Years" & HoursWorked == "50 Hours or More Worked"))

# what is the diff now?

sum(FinalMarginalTotals$Diff)

# extract out all the people
# #create one row per observation
OnePersonNewPeeps <- FinalMarginalTotals[rep(seq(nrow(FinalMarginalTotals)), FinalMarginalTotals$Diff), 1:4]

# give them IDs

OnePersonID <- as.numeric(max(File2ReducedPopulation$ID))

OnePersonNewPeeps <- OnePersonNewPeeps %>%
  mutate(PartnershipStatus = "Not Partnered",
         ID = OnePersonID + row_number())

# add in relationship status
set.seed(TheRandomSeeds[1])                                          #################### seed 1 %%%%%%%%%%%%%%%%
OnePartneredMen <- OnePersonNewPeeps %>%
  filter(Sex == "Male") %>%
  slice_sample(n=15, replace = FALSE) %>%
  mutate(PartnershipStatus ="Partnered")

OnePartneredWomen <- OnePersonNewPeeps %>%
  filter(Sex == "Female") %>%
  slice_sample(n=21, replace = FALSE) %>%
  mutate(PartnershipStatus ="Partnered")

OnePersonUpdated <- OnePersonNewPeeps %>%
  filter(!ID %in% c(OnePartneredMen$ID, OnePartneredWomen$ID)) %>%
  bind_rows(OnePartneredMen, OnePartneredWomen)

table(OnePersonUpdated$HoursWorked)

OnePersonHH <- OnePersonUpdated %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))

table(OnePersonHH$HoursWorked)


# add these people to the OnePersonHousehold population


saveRDS(OnePersonHH, file = "PhDRData/ExtraPeople/OnePersonHH.rds")

rm(FinalMarginalTotals, Merged1PAgeHrs, OnePartneredMen, OnePartneredWomen, OnePersonHH, OnePersonNewPeeps,
   OnePersonUpdated, Original1PHrsByAge, StatsNZ1P, SubMarginals1P, SynPop1P, DiffP1, TotalOriginal1P, OnePersonID)



































####################################################################################################
# Two person households
####################################################################################################

NoTotals <- readRDS(file = "PhDRData/NoTotals.rds")

File1LongData <- readRDS("PhDRData/File1LongData.rds")

StatsNZ2P <- NoTotals %>%
  filter(UsualResidents == "Two Usual Residents") 


SynPop2P <- File1LongData %>%
  filter(UsualResidents == "Two Usual Residents")

# what is the difference?
StatsNZ2PTots <- AllTotals %>%
  filter(UsualResidents == "Two Usual Residents")

TotalOriginal2P <- as.numeric(StatsNZ2PTots[600,6])
DiffP2 <- TotalOriginal2P - nrow(SynPop2P)


NumSexAgeGrps2P <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Two Usual Residents", HoursWorked == "Total", 
         PartnershipStatus %in% c("Not Partnered", "Partnered"))

NumSexAgeGrps2PSynPop <- SynPop2P %>%
  group_by(Sex, AgeGroup, PartnershipStatus) %>%
  summarise(CountSP = n())

NumSexPrtAgeG2P <- left_join(NumSexAgeGrps2P, NumSexAgeGrps2PSynPop, by = c("Sex", "PartnershipStatus", "AgeGroup")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(Diff > 0)

# check to see if numbers hold up
sum(NumSexPrtAgeG2P$Diff)

rm(NumSexAgeGrps2P, NumSexAgeGrps2PSynPop)


NumSexAgeWorkStatus <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Two Usual Residents", HoursWorked != "Total",
         PartnershipStatus %in% c("Total")) %>%
  mutate(HoursWorked = as.character(HoursWorked))


NumSexAgePartStatusSynPop <- SynPop2P %>%
  group_by(Sex, AgeGroup, HoursWorked) %>%
  summarise(CountSP = n()) %>%
  mutate(HoursWorked = as.character(HoursWorked))


# remove all zeros above the last non-zero working hours + any zeros related to the No Hours category

NumSexAgeWorkStatusG2P <- left_join(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop, by = c("Sex", "AgeGroup", "HoursWorked")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(!AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "85 Years and Over"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Male" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "18 - 24 Years" & Sex == "Male" & 
              HoursWorked %in% c("No Hours", "40-49 Hours Worked", "50 Hours or More Worked")),
         ! (AgeGroup == "25 - 34 Years" & Sex == "Male" & 
              HoursWorked %in% c("No Hours", "30-39 Hours Worked", "50 Hours or More Worked")),
         ! (AgeGroup == "35 - 44 Years" & Sex == "Male" & HoursWorked %in% c("No Hours", "50 Hours or More Worked")),
         ! (AgeGroup == "45 - 54 Years" & Sex == "Male" & 
              HoursWorked %in% c("No Hours", "10-19 Hours Worked", "30-39 Hours Worked", "40-49 Hours Worked")),
         ! (AgeGroup == "55 - 64 Years" & Sex == "Male" & 
              HoursWorked %in% c("No Hours", "20-29 Hours Worked", "30-39 Hours Worked", "40-49 Hours Worked")),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Male" & 
              HoursWorked %in% c("No Hours", "10-19 Hours Worked", "20-29 Hours Worked", "50 Hours or More Worked")),
         ! (AgeGroup == "75 - 84 Years" & Sex == "Male" & HoursWorked != "1-9 Hours Worked"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Female"),
         ! (AgeGroup == "18 - 24 Years" & Sex == "Female" 
            & HoursWorked %in% c("No Hours", "20-29 Hours Worked", "30-39 Hours Worked")),
         ! (AgeGroup == "25 - 34 Years" & Sex == "Female" & HoursWorked %in% c("1-9 Hours Worked", "20-29 Hours Worked")),
         ! (AgeGroup == "35 - 44 Years" & Sex == "Female" & HoursWorked %in% c("10-19 Hours Worked","50 Hours or More Worked")),
         ! (AgeGroup == "45 - 54 Years" & Sex == "Female" & 
              HoursWorked %in% c("1-9 Hours Worked","20-29 Hours Worked", "30-39 Hours Worked", "40-49 Hours Worked")),
         ! (AgeGroup == "55 - 64 Years" & Sex == "Female" & 
              HoursWorked %in% c("No Hours", "20-29 Hours Worked", "30-39 Hours Worked")),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Female" & 
              ! HoursWorked %in% c("No Hours","1-9 Hours Worked","20-29 Hours Worked","50 Hours or More Worked")),
         ! (AgeGroup == "75 - 84 Years" & Sex == "Female" & 
              ! HoursWorked %in% c("No Hours", "1-9 Hours Worked")),
         Diff > -1)

# check to see if numbers hold up
sum(NumSexAgeWorkStatusG2P$Diff)


table(NumSexAgeWorkStatusG2P$Diff)

# Add 3 to all diffs of zero

NumSexAgeWorkStatus2 <- NumSexAgeWorkStatusG2P %>%
  mutate(Diff = ifelse(Diff == 0, 3, Diff))

# check to see if numbers hold up
sum(NumSexAgeWorkStatus2$Diff)


rm(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop)


# going to start matching here

# create and add ID

OnePersonHH <- readRDS("PhDRData/ExtraPeople/OnePersonHH.rds")

EndID1P <- max(OnePersonHH$ID)

Start2P <- NumSexPrtAgeG2P %>%
  tidyr::uncount(Diff) %>%
  select(c(Sex, AgeGroup, PartnershipStatus, UsualResidents)) %>%
  mutate(ID = row_number()+EndID1P)

StartWork <- NumSexAgeWorkStatus2 %>%
  tidyr::uncount(Diff) %>%
  select(Sex, AgeGroup, HoursWorked)


Rolling2P <- Start2P %>%
  filter(AgeGroup %in% c("5 - 9 Years", "10 - 14 Years")) %>%
  mutate(HoursWorked = "No Hours")

Male1517 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years")

set.seed(TheRandomSeeds[2])                                          #################### seed 2 %%%%%%%%%%%%%%%%
Male1517Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Male1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1517, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male1517Work)

rm(Male1517, Male1517Work)

Male1824 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[3])                                           #################### seed 3 %%%%%%%%%%%%%%%%
Male1824Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Male1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1824, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male1824Work)

rm(Male1824, Male1824Work)

Male2534 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[4])                                         #################### seed 4 %%%%%%%%%%%%%%%%
Male2534Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Male2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male2534, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male2534Work)

rm(Male2534, Male2534Work)

Male3544 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[5])                                       #################### seed 5 %%%%%%%%%%%%%%%%
Male3544Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Male3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male3544, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male3544Work)

rm(Male3544, Male3544Work)

Male4554 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[6])                                              #################### seed 6 %%%%%%%%%%%%%%%%
Male4554Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Male4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male4554, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male4554Work)

rm(Male4554, Male4554Work)

Male5564 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[7])                                             #################### seed 7 %%%%%%%%%%%%%%%%
Male5564Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Male5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male5564, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male5564Work)

rm(Male5564, Male5564Work)

Male6574 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[8])                                             #################### seed 8 %%%%%%%%%%%%%%%%
Male6574Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Male6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male6574, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male6574Work)

rm(Male6574, Male6574Work)

Male7584 <- Start2P %>%
  filter(Sex == "Male", AgeGroup == "75 - 84 Years")

Male7584Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "75 - 84 Years") %>%
  slice_head(n=2) 

Male7584Work[2,3] <- "No Hours"

set.seed(TheRandomSeeds[9])                                             #################### seed 9 %%%%%%%%%%%%%%%%
Male7584Work <-  Male7584Work %>%
  slice_sample(n=nrow(Male7584), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male7584, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Male7584Work)

rm(Male7584, Male7584Work)

# females

Female1824 <- Start2P %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[10])                                             #################### seed 10 %%%%%%%%%%%%%%%%
Female1824Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Female1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female1824, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Female1824Work)

rm(Female1824, Female1824Work)

Female2534 <- Start2P %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[11])                                             #################### seed 11 %%%%%%%%%%%%%%%%
Female2534Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Female2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female2534, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Female2534Work)

rm(Female2534, Female2534Work)

Female3544 <- Start2P %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[12])                                             #################### seed 12 %%%%%%%%%%%%%%%%
Female3544Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Female3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female3544, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Female3544Work)

rm(Female3544, Female3544Work)

Female6574<- Start2P %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[13])                                             #################### seed 13 %%%%%%%%%%%%%%%%
Female6574Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Female6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female6574, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Female6574Work)

rm(Female6574, Female6574Work)


Female7584<- Start2P %>%
  filter(Sex == "Female", AgeGroup == "75 - 84 Years")

set.seed(TheRandomSeeds[14])                                             #################### seed 14 %%%%%%%%%%%%%%%%
Female7584Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "75 - 84 Years") %>%
  slice_sample(n=nrow(Female7584), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female7584, by = 0) %>%
  select(-Row.names)

Rolling2P <- bind_rows(Rolling2P, Female7584Work)

rm(Female7584, Female7584Work)

# add the ordered factor back as hours worked

table(Rolling2P$HoursWorked)

TwoPersonHH <- Rolling2P %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))

table(TwoPersonHH$HoursWorked)

saveRDS(TwoPersonHH, file = "PhDRData/ExtraPeople/TwoPersonHH.rds")


rm(NumSexAgeWorkStatus2, NumSexAgeWorkStatusG2P, NumSexPrtAgeG2P, Rolling2P, Start2P, StartWork, StatsNZ2P,
   StatsNZ2PTots, SynPop2P, DiffP2, EndID1P, TotalOriginal2P)






































####################################################################################################
# Three person households
####################################################################################################

NoTotals <- readRDS(file = "PhDRData/NoTotals.rds")
File1LongData <- readRDS("PhDRData/File1LongData.rds")
AllTotals <- readRDS("PhDRData/AllTotals.rds")

StatsNZ3P <- NoTotals %>%
  filter(UsualResidents == "Three Usual Residents") 


SynPop3P <- File1LongData %>%
  filter(UsualResidents == "Three Usual Residents")

# what is the difference?
StatsNZ3PTots <- AllTotals %>%
  filter(UsualResidents == "Three Usual Residents")

TotalOriginal3P <- as.numeric(StatsNZ3PTots[600,6])
DiffP3 <- TotalOriginal3P - nrow(SynPop3P)


NumSexAgeGrps3P <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Three Usual Residents", HoursWorked == "Total", 
         PartnershipStatus %in% c("Not Partnered", "Partnered"))

NumSexAgeGrps3PSynPop <- SynPop3P %>%
  group_by(Sex, AgeGroup, PartnershipStatus) %>%
  summarise(CountSP = n())

NumSexPrtAgeG3P <- left_join(NumSexAgeGrps3P, NumSexAgeGrps3PSynPop, by = c("Sex", "PartnershipStatus", "AgeGroup")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(Diff > 0)

# check to see if numbers hold up
sum(NumSexPrtAgeG3P$Diff)

rm(NumSexAgeGrps3P, NumSexAgeGrps3PSynPop)


NumSexAgeWorkStatus <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Three Usual Residents", HoursWorked != "Total",
         PartnershipStatus %in% c("Total")) %>%
  mutate(HoursWorked = as.character(HoursWorked))


NumSexAgePartStatusSynPop <- SynPop3P %>%
  group_by(Sex, AgeGroup, HoursWorked) %>%
  summarise(CountSP = n()) %>%
  mutate(HoursWorked = as.character(HoursWorked))


# remove all zeros above the last non-zero working hours + any zeros related to the No Hours category

NumSexAgeWorkStatusG3P <- left_join(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop, by = c("Sex", "AgeGroup", "HoursWorked")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(!AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "85 Years and Over"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Male" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "75 - 84 Years" & Sex == "Male" & HoursWorked != "No Hours"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Female" & !HoursWorked %in% c("No Hours","1-9 Hours Worked","10-19 Hours Worked")),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Female" & !HoursWorked == "No Hours"),
         ! (AgeGroup == "75 - 84 Years" & Sex == "Female" & !HoursWorked == "No Hours"),
         Diff > -1)

# check to see if numbers hold up
sum(NumSexAgeWorkStatusG3P$Diff)


table(NumSexAgeWorkStatusG3P$Diff)

# Add 3 to all diffs of zero

NumSexAgeWorkStatus3 <- NumSexAgeWorkStatusG3P %>%
  mutate(Diff = ifelse(Diff == 0, 3, Diff))

# check to see if numbers hold up
sum(NumSexAgeWorkStatus3$Diff)

# lots but these will be sampled

rm(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop)


# going to start matching here

# create and add ID

TwoPersonHH <- readRDS("PhDRData/ExtraPeople/TwoPersonHH.rds")

EndID2P <- max(TwoPersonHH$ID)

# add in the older people as these are missed because of NA values for partnered/not partnered counts

OlderPeople3P <- StatsNZ3PTots %>%
  filter(AgeGroup == "85 Years and Over", !PartnershipStatus == "Total", HoursWorked == "Total", !Sex == "Total") %>%
  filter(!(is.na(Count))) %>%
  mutate(CountSP = 0,
         Diff = Count)

Start3P <- bind_rows(NumSexPrtAgeG3P, OlderPeople3P) %>%
  tidyr::uncount(Diff) %>%
  select(c(Sex, AgeGroup, PartnershipStatus, UsualResidents)) %>%
  mutate(ID = row_number()+EndID2P)

StartWork <- NumSexAgeWorkStatus3 %>%
  tidyr::uncount(Diff) %>%
  select(Sex, AgeGroup, HoursWorked)


Rolling3P <- Start3P %>%
  filter(AgeGroup %in% c("5 - 9 Years", "10 - 14 Years")) %>%
  mutate(HoursWorked = "No Hours")

Male1517 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years")


set.seed(TheRandomSeeds[15])                                             #################### seed 15 %%%%%%%%%%%%%%%%
Male1517Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Male1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1517, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male1517Work)

rm(Male1517, Male1517Work)

Male1824 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[16])                                             #################### seed 16 %%%%%%%%%%%%%%%%
Male1824Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Male1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1824, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male1824Work)

rm(Male1824, Male1824Work)

Male2534 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[17])                                             #################### seed 17 %%%%%%%%%%%%%%%%
Male2534Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Male2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male2534, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male2534Work)

rm(Male2534, Male2534Work)

Male3544 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[18])                                             #################### seed 18 %%%%%%%%%%%%%%%%
Male3544Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Male3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male3544, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male3544Work)

rm(Male3544, Male3544Work)

Male4554 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[19])                                             #################### seed 19 %%%%%%%%%%%%%%%%
Male4554Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Male4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male4554, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male4554Work)

rm(Male4554, Male4554Work)

Male5564 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[20])                                             #################### seed 20 %%%%%%%%%%%%%%%%
Male5564Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Male5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male5564, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male5564Work)

rm(Male5564, Male5564Work)

Male6574 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[21])                                             #################### seed 21 %%%%%%%%%%%%%%%%
Male6574Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Male6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male6574, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male6574Work)

rm(Male6574, Male6574Work)

Male7584 <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "75 - 84 Years")

Male7584Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "75 - 84 Years") %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male7584, by = 0) %>%
  select(-Row.names) 

Rolling3P <- bind_rows(Rolling3P, Male7584Work)

rm(Male7584, Male7584Work)

Male85Plus <- Start3P %>%
  filter(Sex == "Male", AgeGroup == "85 Years and Over")

Male85PlusWork <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "75 - 84 Years") %>%
  slice_sample(n=nrow(Male85Plus), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male85Plus, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Male85PlusWork)

rm(Male85Plus, Male85PlusWork)



# females

Female1517 <- Start3P %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years")

Female1517Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Female1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female1517, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female1517Work)

rm(Female1517, Female1517Work)


Female2534 <- Start3P %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[22])                                             #################### seed 22 %%%%%%%%%%%%%%%%
Female2534Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Female2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female2534, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female2534Work)

rm(Female2534, Female2534Work)

Female3544 <- Start3P %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[23])                                             #################### seed 23 %%%%%%%%%%%%%%%%
Female3544Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Female3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female3544, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female3544Work)

rm(Female3544, Female3544Work)


Female4554 <- Start3P %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[24])                                             #################### seed 24 %%%%%%%%%%%%%%%%
Female4554Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Female4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female4554, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female4554Work)

rm(Female4554, Female4554Work)


Female5564 <- Start3P %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[25])                                             #################### seed 25 %%%%%%%%%%%%%%%%
Female5564Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Female5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female5564, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female5564Work)

rm(Female5564, Female5564Work)


Female6574<- Start3P %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[26])                                             #################### seed 26 %%%%%%%%%%%%%%%%
Female6574Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Female6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female6574, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female6574Work)

rm(Female6574, Female6574Work)


Female7584<- Start3P %>%
  filter(Sex == "Female", AgeGroup == "75 - 84 Years")

set.seed(TheRandomSeeds[27])                                             #################### seed 27 %%%%%%%%%%%%%%%%
Female7584Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "75 - 84 Years") %>%
  slice_sample(n=nrow(Female7584), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female7584, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female7584Work)

rm(Female7584, Female7584Work)

Female85Plus<- Start3P %>%
  filter(Sex == "Female", AgeGroup == "85 Years and Over")

Female85PlusWork <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Female85Plus), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female85Plus, by = 0) %>%
  select(-Row.names)

Rolling3P <- bind_rows(Rolling3P, Female85PlusWork)

rm(Female85Plus, Female85PlusWork)


# add the ordered factor back as hours worked

table(Rolling3P$HoursWorked)

ThreePersonHH <- Rolling3P %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))

table(ThreePersonHH$HoursWorked)

saveRDS(ThreePersonHH, file = "PhDRData/ExtraPeople/ThreePersonHH.rds")


rm(NumSexAgeWorkStatus3, NumSexAgeWorkStatusG3P, NumSexPrtAgeG3P, OlderPeople3P, Rolling3P, Start3P,
   StartWork, StatsNZ3P, StatsNZ3PTots, SynPop3P, DiffP3, EndID2P, TotalOriginal3P, TwoPersonHH)











































####################################################################################################
# Four person households
####################################################################################################

NoTotals <- readRDS(file = "PhDRData/NoTotals.rds")
File1LongData <- readRDS("PhDRData/File1LongData.rds")
AllTotals <- readRDS("PhDRData/AllTotals.rds")

StatsNZ4P <- NoTotals %>%
  filter(UsualResidents == "Four Usual Residents") 


SynPop4P <- File1LongData %>%
  filter(UsualResidents == "Four Usual Residents")

# what is the difference?
StatsNZ4PTots <- AllTotals %>%
  filter(UsualResidents == "Four Usual Residents")

TotalOriginal4P <- as.numeric(StatsNZ4PTots[600,6])
DiffP4 <- TotalOriginal4P - nrow(SynPop4P)


NumSexAgeGrps4P <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Four Usual Residents", HoursWorked == "Total", 
         PartnershipStatus %in% c("Not Partnered", "Partnered"))

NumSexAgeGrps4PSynPop <- SynPop4P %>%
  group_by(Sex, AgeGroup, PartnershipStatus) %>%
  summarise(CountSP = n())

NumSexPrtAgeG4P <- left_join(NumSexAgeGrps4P, NumSexAgeGrps4PSynPop, by = c("Sex", "PartnershipStatus", "AgeGroup")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Count = ifelse(Sex == "Male" & AgeGroup == "65 - 74 Years" & PartnershipStatus == "Not Partnered", 3, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(Diff > 0)

# check to see if numbers hold up
sum(NumSexPrtAgeG4P$Diff)

rm(NumSexAgeGrps4P, NumSexAgeGrps4PSynPop)


NumSexAgeWorkStatus <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Four Usual Residents", HoursWorked != "Total",
         PartnershipStatus %in% c("Total")) %>%
  mutate(HoursWorked = as.character(HoursWorked))


NumSexAgePartStatusSynPop <- SynPop4P %>%
  group_by(Sex, AgeGroup, HoursWorked) %>%
  summarise(CountSP = n()) %>%
  mutate(HoursWorked = as.character(HoursWorked))


# remove all zeros above the last non-zero working hours + any zeros related to the No Hours category

NumSexAgeWorkStatusG4P <- left_join(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop, by = c("Sex", "AgeGroup", "HoursWorked")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(!AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "75 - 84 Years", "85 Years and Over"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Male" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Male" & HoursWorked != "No Hours"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Female" & !HoursWorked %in% c("No Hours","1-9 Hours Worked","10-19 Hours Worked")),
         ! (AgeGroup == "55 - 64 Years" & Sex == "Female" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Female" & !HoursWorked == "No Hours"),
         Diff > -1)

# check to see if numbers hold up
sum(NumSexAgeWorkStatusG4P$Diff)


table(NumSexAgeWorkStatusG4P$Diff)

# Add 3 to all diffs of zero

NumSexAgeWorkStatus4 <- NumSexAgeWorkStatusG4P %>%
  mutate(Diff = ifelse(Diff == 0, 3, Diff))

# check to see if numbers hold up
sum(NumSexAgeWorkStatus4$Diff)

# lots but these will be sampled

rm(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop)


# going to start matching here

# create and add ID

ThreePersonHH <- readRDS("PhDRData/ExtraPeople/ThreePersonHH.rds")

EndID3P <- max(ThreePersonHH$ID)

# add in the older people as these are missed because of NA values for partnered/not partnered counts

OlderPeople4P <- StatsNZ4PTots %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years", PartnershipStatus == "Not Partnered", HoursWorked == "Total") %>%
  mutate(Count = 3,
         CountSP = 0,
         Diff = Count -CountSP )

Start4P <- bind_rows(NumSexPrtAgeG4P, OlderPeople4P) %>%
  tidyr::uncount(Diff) %>%
  select(c(Sex, AgeGroup, PartnershipStatus, UsualResidents)) %>%
  mutate(ID = row_number()+EndID3P)

StartWork <- NumSexAgeWorkStatus4 %>%
  tidyr::uncount(Diff) %>%
  select(Sex, AgeGroup, HoursWorked)


Rolling4P <- Start4P %>%
  filter(AgeGroup %in% c("5 - 9 Years", "10 - 14 Years")) %>%
  mutate(HoursWorked = "No Hours")

Male1517 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years")

set.seed(TheRandomSeeds[28])                                             #################### seed 28 %%%%%%%%%%%%%%%%
Male1517Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Male1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1517, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male1517Work)

rm(Male1517, Male1517Work)

Male1824 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[29])                                             #################### seed 29 %%%%%%%%%%%%%%%%
Male1824Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Male1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1824, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male1824Work)

rm(Male1824, Male1824Work)

Male2534 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[30])                                             #################### seed 30 %%%%%%%%%%%%%%%%
Male2534Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Male2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male2534, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male2534Work)

rm(Male2534, Male2534Work)

Male3544 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[31])                                             #################### seed 31 %%%%%%%%%%%%%%%%
Male3544Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Male3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male3544, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male3544Work)

rm(Male3544, Male3544Work)

Male4554 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[32])                                             #################### seed 32 %%%%%%%%%%%%%%%%
Male4554Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Male4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male4554, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male4554Work)

rm(Male4554, Male4554Work)

Male5564 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[33])                                             #################### seed 33 %%%%%%%%%%%%%%%%
Male5564Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Male5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male5564, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male5564Work)

rm(Male5564, Male5564Work)

Male6574 <- Start4P %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[34])                                             #################### seed 34 %%%%%%%%%%%%%%%%
Male6574Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Male6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male6574, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Male6574Work)

rm(Male6574, Male6574Work)


# females

Female1824 <- Start4P %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[35])                                             #################### seed 35 %%%%%%%%%%%%%%%%
Female1824Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Female1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female1824, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female1824Work)

rm(Female1824, Female1824Work)


Female2534 <- Start4P %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[36])                                             #################### seed 36 %%%%%%%%%%%%%%%%
Female2534Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Female2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female2534, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female2534Work)

rm(Female2534, Female2534Work)

Female3544 <- Start4P %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[37])                                             #################### seed 37 %%%%%%%%%%%%%%%%
Female3544Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Female3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female3544, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female3544Work)

rm(Female3544, Female3544Work)


Female4554 <- Start4P %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[38])                                             #################### seed 38 %%%%%%%%%%%%%%%%
Female4554Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Female4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female4554, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female4554Work)

rm(Female4554, Female4554Work)


Female5564 <- Start4P %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[39])                                             #################### seed 39 %%%%%%%%%%%%%%%%
Female5564Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Female5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female5564, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female5564Work)

rm(Female5564, Female5564Work)


Female6574<- Start4P %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[40])                                             #################### seed 40 %%%%%%%%%%%%%%%%
Female6574Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Female6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female6574, by = 0) %>%
  select(-Row.names)

Rolling4P <- bind_rows(Rolling4P, Female6574Work)

rm(Female6574, Female6574Work)

# add the ordered factor back as hours worked

table(Rolling4P$HoursWorked)

FourPersonHH <- Rolling4P %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))

table(FourPersonHH$HoursWorked)

saveRDS(FourPersonHH, file = "PhDRData/ExtraPeople/FourPersonHH.rds")

rm(NumSexAgeWorkStatus4, NumSexAgeWorkStatusG4P, NumSexPrtAgeG4P, OlderPeople4P, Rolling4P, Start4P,
   StartWork, StatsNZ4P, StatsNZ4PTots, SynPop4P, ThreePersonHH, FourPersonHH, DiffP4, EndID3P, TotalOriginal4P)

















































####################################################################################################
# Five person households
####################################################################################################

NoTotals <- readRDS(file = "PhDRData/NoTotals.rds")
File1LongData <- readRDS("PhDRData/File1LongData.rds")
AllTotals <- readRDS("PhDRData/AllTotals.rds")

StatsNZ5P <- NoTotals %>%
  filter(UsualResidents == "Five Usual Residents") 


SynPop5P <- File1LongData %>%
  filter(UsualResidents == "Five Usual Residents")

# what is the difference?
StatsNZ5PTots <- AllTotals %>%
  filter(UsualResidents == "Five Usual Residents")

TotalOriginal5P <- as.numeric(StatsNZ5PTots[600,6])
DiffP5 <- TotalOriginal5P - nrow(SynPop5P)


NumSexAgeGrps5P <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Five Usual Residents", HoursWorked == "Total", 
         PartnershipStatus %in% c("Not Partnered", "Partnered"))

NumSexAgeGrps5PTots <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Five Usual Residents", HoursWorked == "Total", 
         PartnershipStatus == "Total")

NumSexAgeGrps5PSynPop <- SynPop5P %>%
  group_by(Sex, AgeGroup, PartnershipStatus) %>%
  summarise(CountSP = n())

NumSexPrtAgeG5P <- left_join(NumSexAgeGrps5P, NumSexAgeGrps5PSynPop, by = c("Sex", "PartnershipStatus", "AgeGroup")) %>%
  filter(! AgeGroup %in% c("75 - 84 Years", "85 Years and Over")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         Count = ifelse(Sex == "Male" & AgeGroup == "65 - 74 Years" & PartnershipStatus == "Partnered", 9,
                        ifelse(Sex == "Male" & AgeGroup == "65 - 74 Years" & PartnershipStatus == "Not Partnered", 3, 
                               ifelse(Sex == "Female" & AgeGroup == "65 - 74 Years", 3, Count))),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(Diff > 0)

# check to see if numbers hold up
sum(NumSexPrtAgeG5P$Diff)

rm(NumSexAgeGrps5P, NumSexAgeGrps5PSynPop)


NumSexAgeWorkStatus <- AllTotals %>%
  filter(Sex != "Total", AgeGroup != "Total",  UsualResidents == "Five Usual Residents", HoursWorked != "Total",
         PartnershipStatus %in% c("Total")) %>%
  mutate(HoursWorked = as.character(HoursWorked))


NumSexAgePartStatusSynPop <- SynPop5P %>%
  group_by(Sex, AgeGroup, HoursWorked) %>%
  summarise(CountSP = n()) %>%
  mutate(HoursWorked = as.character(HoursWorked))


# remove all zeros above the last non-zero working hours + any zeros related to the No Hours category

NumSexAgeWorkStatusG5P <- left_join(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop, by = c("Sex", "AgeGroup", "HoursWorked")) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count),
         CountSP = ifelse(is.na(CountSP), 0, CountSP),
         Diff = Count - CountSP) %>%
  filter(!AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "75 - 84 Years", "85 Years and Over"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Male" & !HoursWorked %in% c("No Hours","1-9 Hours Worked","10-19 Hours Worked")),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Male" & HoursWorked != "No Hours"),
         ! (AgeGroup == "15 - 17 Years" & Sex == "Female" & !HoursWorked %in% c("No Hours","1-9 Hours Worked","10-19 Hours Worked")),
         ! (AgeGroup == "18 - 24 Years" & Sex == "Female" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "55 - 64 Years" & Sex == "Female" & HoursWorked == "50 Hours or More Worked"),
         ! (AgeGroup == "65 - 74 Years" & Sex == "Female" & !HoursWorked == "No Hours"),
         Diff > -1)

# check to see if numbers hold up
sum(NumSexAgeWorkStatusG5P$Diff)


table(NumSexAgeWorkStatusG5P$Diff)

# Add 3 to all diffs of zero

NumSexAgeWorkStatus5 <- NumSexAgeWorkStatusG5P %>%
  mutate(Diff = ifelse(Diff == 0, 3, Diff))

# check to see if numbers hold up
sum(NumSexAgeWorkStatus5$Diff)

# lots but these will be sampled

rm(NumSexAgeWorkStatus, NumSexAgePartStatusSynPop)


# going to start matching here

# create and add ID

FourPersonHH <- readRDS("PhDRData/ExtraPeople/FourPersonHH.rds")

EndID4P <- max(FourPersonHH$ID)

Start5P <-NumSexPrtAgeG5P %>%
  tidyr::uncount(Diff) %>%
  select(c(Sex, AgeGroup, PartnershipStatus, UsualResidents)) %>%
  mutate(ID = row_number()+EndID4P)

StartWork <- NumSexAgeWorkStatus5 %>%
  tidyr::uncount(Diff) %>%
  select(Sex, AgeGroup, HoursWorked)


Rolling5P <- Start5P %>%
  filter(AgeGroup %in% c("5 - 9 Years", "10 - 14 Years")) %>%
  mutate(HoursWorked = "No Hours")

Male1517 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years")

set.seed(TheRandomSeeds[41])                                             #################### seed 41 %%%%%%%%%%%%%%%%
Male1517Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Male1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1517, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male1517Work)

rm(Male1517, Male1517Work)

Male1824 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[42])                                             #################### seed 42 %%%%%%%%%%%%%%%%
Male1824Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Male1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male1824, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male1824Work)

rm(Male1824, Male1824Work)

Male2534 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[43])                                             #################### seed 43 %%%%%%%%%%%%%%%%
Male2534Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Male2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male2534, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male2534Work)

rm(Male2534, Male2534Work)

Male3544 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[44])                                             #################### seed 44 %%%%%%%%%%%%%%%%
Male3544Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Male3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male3544, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male3544Work)

rm(Male3544, Male3544Work)

Male4554 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[45])                                             #################### seed 45 %%%%%%%%%%%%%%%%
Male4554Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Male4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male4554, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male4554Work)

rm(Male4554, Male4554Work)

Male5564 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[46])                                             #################### seed 46 %%%%%%%%%%%%%%%%
Male5564Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Male5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male5564, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male5564Work)

rm(Male5564, Male5564Work)

Male6574 <- Start5P %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[47])                                             #################### seed 47 %%%%%%%%%%%%%%%%
Male6574Work <- StartWork %>%
  filter(Sex == "Male", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Male6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Male6574, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Male6574Work)

rm(Male6574, Male6574Work)


# females

Female1517 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years")

set.seed(TheRandomSeeds[48])                                             #################### seed 48 %%%%%%%%%%%%%%%%
Female1517Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years") %>%
  slice_sample(n=nrow(Female1517), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female1517, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female1517Work)

rm(Female1517, Female1517Work)


Female1824 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years")

set.seed(TheRandomSeeds[49])                                             #################### seed 49 %%%%%%%%%%%%%%%%
Female1824Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "18 - 24 Years") %>%
  slice_sample(n=nrow(Female1824), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female1824, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female1824Work)

rm(Female1824, Female1824Work)


Female2534 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years")

set.seed(TheRandomSeeds[50])                                             #################### seed 50 %%%%%%%%%%%%%%%%
Female2534Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years") %>%
  slice_sample(n=nrow(Female2534), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female2534, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female2534Work)

rm(Female2534, Female2534Work)

Female3544 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years")

set.seed(TheRandomSeeds[51])                                             #################### seed 51 %%%%%%%%%%%%%%%%
Female3544Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "35 - 44 Years") %>%
  slice_sample(n=nrow(Female3544), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female3544, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female3544Work)

rm(Female3544, Female3544Work)


Female4554 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years")

set.seed(TheRandomSeeds[52])                                             #################### seed 52 %%%%%%%%%%%%%%%%
Female4554Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "45 - 54 Years") %>%
  slice_sample(n=nrow(Female4554), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female4554, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female4554Work)

rm(Female4554, Female4554Work)


Female5564 <- Start5P %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years")

set.seed(TheRandomSeeds[53])                                             #################### seed 53 %%%%%%%%%%%%%%%%
Female5564Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "55 - 64 Years") %>%
  slice_sample(n=nrow(Female5564), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female5564, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female5564Work)

rm(Female5564, Female5564Work)


Female6574<- Start5P %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years")

set.seed(TheRandomSeeds[54])                                             #################### seed 54 %%%%%%%%%%%%%%%%
Female6574Work <- StartWork %>%
  filter(Sex == "Female", AgeGroup == "65 - 74 Years") %>%
  slice_sample(n=nrow(Female6574), replace = TRUE) %>%
  select(-c(Sex, AgeGroup)) %>%
  merge(Female6574, by = 0) %>%
  select(-Row.names)

Rolling5P <- bind_rows(Rolling5P, Female6574Work)

rm(Female6574, Female6574Work)


# add the ordered factor back as hours worked

table(Rolling5P$HoursWorked)

FivePersonHH <- Rolling5P %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))

table(FivePersonHH$HoursWorked)

saveRDS(FivePersonHH, file = "PhDRData/ExtraPeople/FivePersonHH.rds")

rm(list = ls())















####################################################################################################
####################################################################################################
# Combine all datasets and get some stats
####################################################################################################
####################################################################################################


OnePersonHH <- readRDS("PhDRData/ExtraPeople/OnePersonHH.rds")
TwoPersonHH <- readRDS("PhDRData/ExtraPeople/TwoPersonHH.rds")
ThreePersonHH <- readRDS("PhDRData/ExtraPeople/ThreePersonHH.rds")
FourPersonHH <- readRDS("PhDRData/ExtraPeople/FourPersonHH.rds")
FivePersonHH <- readRDS("PhDRData/ExtraPeople/FivePersonHH.rds")

ExtraPeople <- bind_rows(OnePersonHH, TwoPersonHH, ThreePersonHH, FourPersonHH, FivePersonHH) %>%
  mutate(ID = as.integer(ID))

str(ExtraPeople)

AllTotals <- readRDS("PhDRData/AllTotals.rds")

File2ReducedPopulation <- readRDS("PhDRData/ExtraPeople/File2ReducedPopulation.rds")

# append the extra people

AddedPeople1 <- bind_rows(File2ReducedPopulation, ExtraPeople)

AddedPeople1 <- droplevels(AddedPeople1)






####################################################################################################
####################################################################################################
# Make sure all households are divisible by correct number
####################################################################################################
####################################################################################################

TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

OnePersonHH <- AddedPeople1 %>%
  filter(UsualResidents == "One Usual Resident")

# add people into the population to ensure the household sizes are correctly divisible

TwoPersonHH <- AddedPeople1 %>%
  filter(UsualResidents == "Two Usual Residents")

# extra person for two person households

set.seed(TheRandomSeeds[55])                                             #################### seed 55 %%%%%%%%%%%%%%%%
Extra2P <- TwoPersonHH %>%
  slice_sample(n = 1, replace = FALSE) %>%
  mutate(ID = max(AddedPeople1$ID) + 1)

TwoPersonCorrectNums <- bind_rows(TwoPersonHH, Extra2P)

ThreePersonHH <- AddedPeople1 %>%
  filter(UsualResidents == "Three Usual Residents")

# divisible by 3, no modification needed

FourPersonHH <- AddedPeople1 %>%
  filter(UsualResidents == "Four Usual Residents")

set.seed(TheRandomSeeds[56])                                             #################### seed 56 %%%%%%%%%%%%%%%%
Extra4P <- FourPersonHH %>%
  slice_sample(n = 1, replace = FALSE) %>%
  mutate(ID = max(TwoPersonCorrectNums$ID) + 1)

FourPersonCorrectNums <- bind_rows(FourPersonHH, Extra4P)


FivePersonHH <- AddedPeople1 %>%
  filter(UsualResidents == "Five Usual Residents")


set.seed(TheRandomSeeds[57])                                             #################### seed 57 %%%%%%%%%%%%%%%%
Extra5P <- FivePersonHH %>%
  slice_sample(n = 3, replace = FALSE) %>%
  mutate(ID = max(FourPersonCorrectNums$ID) + row_number())


FivePersonCorrectNums <- bind_rows(FivePersonHH, Extra5P)

File2AllPeople <- bind_rows(OnePersonHH, TwoPersonCorrectNums, ThreePersonHH, FourPersonCorrectNums, FivePersonCorrectNums)

saveRDS(File2AllPeople, file = "PhDRData/File2AllPeople.rds")




####################################################################################################
####################################################################################################
# Check counts against synthetic population now that extra people have been added
####################################################################################################
####################################################################################################

NumNewPPlCreated <- as.numeric(nrow(File2AllPeople) - nrow(File2ReducedPopulation))

# how many extra by age group?

# combine all extra people into one data frame

AllExtraPeople <- bind_rows(ExtraPeople, Extra2P, Extra4P, Extra5P)

table(AllExtraPeople$AgeGroup)

table(AllExtraPeople$Sex)

table(AllExtraPeople$PartnershipStatus)


SynthPopCounts <- File2AllPeople %>%
  group_by(UsualResidents) %>%
  summarise(SPCount = n()) 

PopDataCounts <- AllTotals %>%
  filter(Sex == "Total", AgeGroup == "Total", PartnershipStatus == "Total", HoursWorked == "Total")


CombinedTotals <- left_join(SynthPopCounts, PopDataCounts, by = "UsualResidents") %>%
  select(UsualResidents, Count, SPCount) %>%
  mutate(NumSuppressed = Count - SPCount, 
    PropSuppressed = round(1-(SPCount/Count),3))

CombinedTotals

# new proportion

NumIn12345 <- AllTotals %>%
  filter(Sex == "Total", AgeGroup == "Total", PartnershipStatus == "Total", HoursWorked == "Total",
         UsualResidents %in% c("One Usual Resident", "Two Usual Residents", "Three Usual Residents", 
                               "Four Usual Residents", "Five Usual Residents"))

PopDataNum <- sum(NumIn12345$Count)

round(nrow(File2AllPeople)/PopDataNum,3)

round(nrow(File2AllPeople)/nrow(File2ReducedPopulation),3)



