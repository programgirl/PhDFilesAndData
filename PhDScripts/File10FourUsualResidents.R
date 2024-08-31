#clear workspace
#  rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(conflicted)

conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")




#######################################################################################
#######################################################################################
# generate potential sole parents 
#######################################################################################
#######################################################################################

File4CorrectHrs <- readRDS("PhDRData/File4CorrectHrs.rds")
ChildPropsAtHome3 <- readRDS("PhDRData/ChildPropsAtHome3.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# extract out the four usual residents

FourUsualResidents <- File4CorrectHrs %>%
  filter(UsualResidents == "Four Usual Residents")





####################################################################################################
# graph of ages, relationship status, working hours, all by sex.
####################################################################################################

# ages
AgesProp <- FourUsualResidents %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))

AllAges4P<- ggplot(AgesProp, aes(x=Age, y=CumAgePercent, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100), expand = c(0,1.6)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(AllAges4P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/AllAges4P.pdf")


# ages of partnered people only

AgesPropPart <- FourUsualResidents %>%
  filter(PartnershipStatus == "Partnered") %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))


PartneredAges4P<- ggplot(AgesPropPart, aes(x=Age, y=CumAgePercent, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),
                     expand = c(0, 1.6)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(PartneredAges4P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/PartneredAges4P.pdf")


# working hours

AgesPropHours <- FourUsualResidents %>%
  filter(Age > 14) %>%
  group_by(Sex, HoursWorked) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(HoursPercent=Count/sum(Count),
                HoursWorked = ifelse(!HoursWorked %in% c("No Hours", "50 Hours or More Worked"),
                                     gsub('.{13}$', '', HoursWorked),
                                     ifelse(HoursWorked == "No Hours", "None", "50+")),
                HoursWorkedOrd = factor(HoursWorked, levels = c("None", "1-9", "10-19", "20-29", "30-39",
                                                                "40-49", "50+")))



WorkingHours4P<- ggplot(AgesPropHours, aes(x=HoursWorkedOrd, y=HoursPercent, fill=Sex)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Hours worked per week") + ylab("Proportion") +
  scale_y_continuous(limits = c(0,0.5), breaks = seq(0, 0.5, by = 0.1), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(WorkingHours4P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/WorkingHours4P.pdf")

# prop working
PropWorking <-AgesPropHours %>%
  filter(HoursWorked == "None") %>%
  mutate(Propworking = round(1-HoursPercent, 3))

PropWorking

# prop working 40+ Hours

LongerProps <- AgesPropHours %>%
  filter(HoursWorkedOrd %in% c("40-49", "50+"))%>%
  group_by(Sex) %>%
  summarise(LongestHrsProp = round(100*(sum(HoursPercent)),1))

LongerProps


rm(AgesProp, AgesPropHours, AgesPropPart, AllAges4P, LongerProps, PartneredAges4P, WorkingHours4P,
   File4SchoolIndAdded,PropWorking)












####################################################################################################
####################################################################################################
# create the couples
####################################################################################################
####################################################################################################

# need same-sex first
# need to create couples now as want to remove couples from potential sole parents

PartneredMen <- FourUsualResidents %>%
  filter(Sex == "Male", PartnershipStatus == "Partnered")

PartneredWomen <- FourUsualResidents %>%
  filter(Sex == "Female", PartnershipStatus == "Partnered")

# bring in required Stats NZ data
TABLECODE8160 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type with type of couple/TABLECODE8160_Data_c9558eb4-30f9-4edc-9ad5-c091ac76f6d1.csv", stringsAsFactors=FALSE)
TABLECODE8161 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Age group of people in same-sex couples in occupied private dwellings/TABLECODE8161_Data_9ae7be30-bdba-49ab-9bff-50b093df0335.csv", stringsAsFactors=FALSE)

#######################################################################################
# generate same sex couples
#######################################################################################
# upweight prop are from table 8161.
# upweight age is 25-54
# upweight prop is (18+12+18)/72
AgeUpweight = (TABLECODE8161[3,3]+TABLECODE8161[4,3]+TABLECODE8161[5,3])/TABLECODE8161[1,3]

FSameSexProb <- round(((TABLECODE8160[8,3]/TABLECODE8160[6,3])*2),3)
MSameSexProb <- round(((TABLECODE8160[7,3]/TABLECODE8160[6,3])*2),3)



# get starting household ID

ThreePersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File9ThreePerson/ThreePersonHouseholdsComplete.rds")


StartHHID <- max(ThreePersonHouseholdsComplete$HouseholdID) + 1

# females

SameSexF4 <- fastmatch(PartneredWomen, "Age", FSameSexProb, AgeUpweight, 25,54, StartHHID,
                       "HouseholdID", userseed=TheRandomSeeds[89])             #################### seed 89 %%%%%%%%%%%%%%%%


StartHHID <- max(SameSexF4$HouseholdID) + 1

SameSexM4 <- fastmatch(PartneredMen, "Age", MSameSexProb, AgeUpweight, 25,54, StartHHID,
                       "HouseholdID", userseed=TheRandomSeeds[90])         #################### seed 90 %%%%%%%%%%%%%%%%

 
rm(TABLECODE8160, TABLECODE8161, ThreePersonHouseholdsComplete, AgeUpweight, FSameSexProb, MSameSexProb,
   StartHHID, fastmatch)







#######################################################################################
# generate opposite sex couples
#######################################################################################

# remaining men
PartneredMenLeft <- PartneredMen %>%
  filter(!ID %in% SameSexM4$ID)

#remaining women
PartneredWomenLeft <- PartneredWomen %>%
  filter(!ID %in% SameSexF4$ID)

StartID <- max(SameSexM4$HouseholdID) + 1


OppSex4P <- pairnorm(PartneredMenLeft, "ID", "Age", PartneredWomenLeft, "ID", "Age", directxi = 2, directomega = 3,
                     alphaused = 0, StartID, "HouseholdID",
                     userseed = TheRandomSeeds[91])                         #################### seed 91 %%%%%%%%%%%%%%%%

OppositeSexCouples <- OppSex4P$Matched
UnmatchedPartnered <- OppSex4P$Unmatched

mean(PartneredMenLeft$Age)
mean(PartneredWomenLeft$Age)

men <- OppositeSexCouples %>%
  filter(Sex == "Male")
women <- OppositeSexCouples %>%
  filter(Sex == "Female")

mergedsexes <- left_join(men, women, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.x - Age.y)

# plot
OppositeSexAgeDiffs <- OppositeSexCouples %>%
  arrange(HouseholdID, Sex) %>%
  group_by(HouseholdID) %>%
  mutate(AgeDiff = Age - lag(Age)) %>%
  filter(!(is.na(AgeDiff))) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())

OppositeSexAgeDiffsLong <- OppositeSexAgeDiffs  %>%
  tidyr::uncount(weights = Count)
min(OppositeSexAgeDiffsLong$AgeDiff)
median(OppositeSexAgeDiffsLong$AgeDiff)
max(OppositeSexAgeDiffsLong$AgeDiff)

NumWomenOlder <- as.numeric(OppositeSexAgeDiffsLong %>%
                              filter(AgeDiff < 0) %>%
                              summarise(Count = n()) %>%
                              pull(Count))

NumWomenOlder/nrow(OppositeSexAgeDiffsLong)

File10Partnered <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = OppositeSexAgeDiffsLong,
           color = "grey47", fill = "white") +
  stat_function(fun = dnorm, args = list(mean=2, sd=3),
                color = "#1B9E77", linewidth = 1.5) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1, .12, .14), limits = c(0,.14), expand = c(0, .001)) +
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15), limits = c(-15, 15), expand = c(0, 0.2)) +
  labs(x = "Age difference", y = "Proportion of couples") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))


#   ggsave(File10Partnered, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File10Partnered.pdf")


rm(men, mergedsexes, OppositeSexAgeDiffs, OppositeSexAgeDiffsLong, OppSex4P, PartneredMen, PartneredWomen, PartneredMenLeft,
   PartneredWomenLeft, women, StartID, File10Partnered)






















#######################################################################################
#######################################################################################
# create the child households
#######################################################################################
#######################################################################################

####################################################################################################
# get the kids counts
####################################################################################################

PeopleByAge <- FourUsualResidents %>%
  group_by(Age) %>%
  summarise(Num = n())

# apply probs

NumKids4PH <- left_join(PeopleByAge, ChildPropsAtHome3, by = "Age") %>%
  filter(!(is.na(Fits))) %>%
  mutate(NumNeeded = round(Num*Fits)) %>%
  filter(NumNeeded > 0)

# sample to get the kids

# use diff sample

UnPartneredResidents <- FourUsualResidents %>%
  filter(PartnershipStatus == "Not Partnered")

The15161718InSchool <- FourUsualResidents %>%
  filter(EducationStatus == "Y", Age %in% c(15, 16, 17, 18))

NumKidsByAge15161718 <- The15161718InSchool %>%
  group_by(Age) %>%
  summarise(NumInAge = n())

ExtrasNeeded15161718 <- left_join(NumKidsByAge15161718, NumKids4PH, by = "Age") %>%
  mutate(StilltoSample = NumNeeded - NumInAge)

UnpartneredLeft <- UnPartneredResidents %>%
  filter(!ID %in% The15161718InSchool$ID)

UpdatedSampleCounts <- left_join(NumKids4PH, ExtrasNeeded15161718,
                                 by = c("Age", "Num", "Fits", "ExpectedNum", "NumNeeded")) %>%
  select(-NumInAge) %>%
  mutate(SampleSize = ifelse(is.na(StilltoSample), NumNeeded, StilltoSample),
         SampleSize = ifelse(SampleSize > 0, SampleSize, 0)) %>%
  select(Age, SampleSize)


TheKids <- diffsample(UnpartneredLeft, "Age", UpdatedSampleCounts, "Age", "SampleSize",
                      userseed=TheRandomSeeds[92])                         #################### seed 92 %%%%%%%%%%%%%%%%

TheKids <- bind_rows(TheKids, The15161718InSchool)

# test counts
table(UnPartneredResidents$Age)

table(TheKids$Age)

# check counts
isTRUE(sum(NumKids4PH$NumNeeded) == nrow(TheKids))
sum(NumKids4PH$NumNeeded)
nrow(TheKids)


# # the household split is that .070 households with children are sole parents.
# the couples household contain 2 kids
# the sole parent households contain 3
# split is on number of households NOT number of children
# there are 2379 so split must be that sole parents households children count is divisible by 3
# and couples households children count is divisible by 2

# 255 kids to sole parents = 85 households
2379-255


# split is 2124 kids to couples, 255 to sole parents.
# with 2124 kids, and 2 kids per couple, that is 1062 couples required
# there are 1072 couples, so 10 couples have no child but an additional two people in the household, or <1%




####################################################################################################
# sole parent households
####################################################################################################
####################################################################################################
# Dads
####################################################################################################

SoleFathNum <- round(255 * .138, 0) + 1 # is 35, so need to add 1

SoleFathNum <- SoleFathNum + 18

# get potential sole fathers

PotSoleFathers <- FourUsualResidents %>%
  filter(!ID %in% c(SameSexM4$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Male")

# sample the kids
set.seed(TheRandomSeeds[93])                                                    #################### seed 93 %%%%%%%%%%%%%%%%
FathKids <- TheKids %>%
  slice_sample(n=SoleFathNum, replace = FALSE)


# add kids to sole fathers

StartID <- max(OppositeSexCouples$HouseholdID) + 1

SoleFathHouseholds <- pairchild(FathKids, "ID", "Age", numchild = 3, twinprob = 0.029, PotSoleFathers, "ID", "Age",
                                minparage = 18, maxparage = 50, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[94], maxdiff = 7)           #################### seed 94 %%%%%%%%%%%%%%%%


SoleFatherFamilies <- SoleFathHouseholds$Matched
SoleFatherUnmatchedKids <- SoleFathHouseholds$Children # 6 unmatched, down have the right number sole father households
SoleFatherUnmatchedMen <- SoleFathHouseholds$Adults

numFathHH <- as.numeric(nrow(SoleFatherFamilies)/4)

SoleFatherFamilies <- SoleFatherFamilies %>%
  mutate(Type = ifelse(row_number() <= numFathHH*3, "Child", "Sole Father"))

table(SoleFatherFamilies$Type)



####################################################################################################
# Mums
####################################################################################################
mothersample <- round(255 * .862, 0) + 2
mothersample <- mothersample + 18

set.seed(TheRandomSeeds[95])                                                    #################### seed 95 %%%%%%%%%%%%%%%%
TheKidsSubset1 <- TheKids %>%
  filter(!ID %in% SoleFatherFamilies$ID) %>%
  slice_sample(n=mothersample, replace = FALSE)


PotSoleMothers <- FourUsualResidents %>%
  filter(!ID %in% c(SameSexF4$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Female")


# add kids to sole mothers

StartID <- max(SoleFatherFamilies$HouseholdID) + 1

SoleMothHouseholds <- pairchild(TheKidsSubset1, "ID", "Age", numchild = 3, twinprob = 0.029, PotSoleMothers, "ID", "Age",
                                minparage = 16, maxparage = 49, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[96], maxdiff = 6)           #################### seed 96 %%%%%%%%%%%%%%%%


SoleMotherFamilies <- SoleMothHouseholds$Matched
SoleMotherUnmatchedKids <- SoleMothHouseholds$Children
SoleMotherUnmatchedWomen <- SoleMothHouseholds$Adults

numMothHH <- as.numeric(nrow(SoleMotherFamilies)/4)

SoleMotherFamilies <- SoleMotherFamilies %>%
  mutate(Type = ifelse(row_number() <= numMothHH*3, "Child", "Sole Mother"))

table(SoleMotherFamilies$Type)









####################################################################################################
####################################################################################################
# Couples with two children
####################################################################################################
####################################################################################################

TheKidsSubset2 <- TheKids %>%
  filter(!ID %in% c(SoleFatherFamilies$ID, SoleMotherFamilies$ID))


# get mother

MumsFromCouples <- OppositeSexCouples %>%
  filter(Sex == "Female")


MumPlusKidHouseholds <- pairchildNum(TheKidsSubset2, "ID", "Age", numchild = 2, twinprob = 0.029, MumsFromCouples, "ID", "Age",
                                     minparage = 16, maxparage = 49, "HouseholdID",
                                     userseed = TheRandomSeeds[97], maxdiff = 6)           #################### seed 97 %%%%%%%%%%%%%%%%


MuminCpls <- MumPlusKidHouseholds$Matched
KidsNotInHouseholds <- MumPlusKidHouseholds$Children
NotMums <- MumPlusKidHouseholds$Adults

LastChildRow <- nrow(MuminCpls)/3

MuminCpls <- MuminCpls %>%
  mutate(Type = ifelse(row_number() <= LastChildRow*2, "Child", "Opposite sex with child"))


MalesInOppSexCpls <- OppositeSexCouples %>%
  filter(Sex == "Male", HouseholdID %in% MuminCpls$HouseholdID) %>%
  mutate(Type = "Opposite sex with child")

OppSexCouplesWithKids <- bind_rows(MuminCpls, MalesInOppSexCpls)







####################################################################################################
# same-sex couples with kids
####################################################################################################

TheKidsSubset3 <- TheKidsSubset2 %>%
  filter(!ID %in% MuminCpls$ID)

# the two youngest go to the mothers

HouseholdIDMoth <- as.numeric(SameSexMoth %>%
                                select(HouseholdID) %>%
                                distinct() %>%
                                pull(HouseholdID))

YoungestTwoToMums <- TheKidsSubset3 %>%
  slice_head(n=2) %>%
  mutate(HouseholdID = HouseholdIDMoth)

SameSexF4C <- bind_rows(SameSexF4, YoungestTwoToMums) %>%
    mutate(Type = ifelse(Age < 20, "Child", "Single sex with child"))


# next two to dads

TheKidsSubset4 <- TheKidsSubset3 %>%
  filter(!ID %in% YoungestTwoToMums$ID)

HouseholdIDFath <- as.numeric(SameSexFath %>%
                                select(HouseholdID) %>%
                                distinct() %>%
                                pull(HouseholdID))

YoungestTwoToDads <- TheKidsSubset4 %>%
  slice_head(n=2) %>%
  mutate(HouseholdID = HouseholdIDFath)

SameSexM4C <- bind_rows(SameSexM4, YoungestTwoToDads) %>%
  mutate(Type = ifelse(Age < 25, "Child", "Single sex with child"))


TheKidsSubset5 <- TheKidsSubset4 %>%
    filter(!ID %in% SameSexM4C$ID)














####################################################################################################
###################################################################################################
# remaining adults
####################################################################################################
###################################################################################################


RemainingAdults <- FourUsualResidents %>%
  filter(!ID %in% c(SameSexF4C$ID, SameSexM4C$ID, SoleFatherFamilies$ID, SoleMotherFamilies$ID,
                    MuminCpls$ID, OppositeSexCouples$ID))

# KidsToPlace <- TheKidsSubset5 %>%
#   filter(Age < 20)
####################################################################################################
####################################################################################################
# Couples with two people added
####################################################################################################
####################################################################################################

RemainingCouples <- OppositeSexCouples %>%
  filter(!ID %in% OppSexCouplesWithKids$ID)

# randomly select one person from the couples

set.seed(TheRandomSeeds[98])                                                    #################### seed 98 %%%%%%%%%%%%%%%%
RandomlySelectedPerson <- RemainingCouples %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE )


OtherPersonToCpl <- otherNum(RandomlySelectedPerson, "ID", "Age", "HouseholdID", RemainingAdults, "ID", "Age", 
                             numadd = 2, sdused = 4,
                             userseed = TheRandomSeeds[99])                   #################### seed 99 %%%%%%%%%%%%%%%%

FinishedNoKidsCpls <- OtherPersonToCpl$Matched

LastCplRow <- nrow(FinishedNoKidsCpls)/3

FinishedNoKidsCpls <- FinishedNoKidsCpls %>%
  mutate(Type = ifelse(row_number() <= LastCplRow, "Opposite sex without child", "Other with couples"))

table(FinishedNoKidsCpls$Type)

# add in the missing people from the couples, those who were selected for matching

UnselectedPersonInCouple <- RemainingCouples %>%
  filter(!ID %in% c(FinishedNoKidsCpls$ID)) %>%
  mutate(Type = "Opposite sex without child")


CouplesWithoutKids <- bind_rows(FinishedNoKidsCpls, UnselectedPersonInCouple)

table(CouplesWithoutKids$Type)







####################################################################################################
####################################################################################################
# Create last households - unrelated people only
####################################################################################################
####################################################################################################


OtherOnlyPeople <- RemainingAdults %>%
  filter(!ID %in% CouplesWithoutKids$ID)

OthersStartNum <- max(SoleMotherFamilies$HouseholdID) +1


OtherPeopleHouseholdsList <- other(OtherOnlyPeople, "ID", "Age", 4, sdused = 3, OthersStartNum, "HouseholdID",
                                   userseed = TheRandomSeeds[100])                #################### seed 100 %%%%%%%%%%%%%%%%

OtherPeople <- OtherPeopleHouseholdsList$Matched %>%
  mutate(Type = "Other")












####################################################################################################
####################################################################################################
# Everyone into the same household data frame
####################################################################################################
####################################################################################################


FourPersonHouseholdsComplete <- bind_rows(OppSexCouplesWithKids, SameSexF4C, SameSexM4C, SoleFatherFamilies,
                                           SoleMotherFamilies, CouplesWithoutKids, OtherPeople)

duplicates <- FourPersonHouseholdsComplete %>%
  group_by(ID) %>%
  summarise(Count = n()) %>%
  filter(Count > 1)

duplicates <- FourPersonHouseholdsComplete %>%
  group_by(HouseholdID) %>%
  summarise(Count = n()) %>%
  filter(Count != 4)































####################################################################################################
####################################################################################################
# Graphs, everyone loves graphs - parent age differences
####################################################################################################
####################################################################################################

# sole fathers

SoleFathers <- SoleFatherFamilies %>%
  filter(Type == "Sole Father")

SoleFatherKids <- SoleFatherFamilies %>%
  filter(Type == "Child")

SoleFathAgeDiffs <- left_join(SoleFatherKids, SoleFathers, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.y - Age.x) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())


SoleFathAgeDiffsLong <- SoleFathAgeDiffs  %>%
  tidyr::uncount(weights = Count)

File10SoleFathers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleFathAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleFathAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .1), limits = c(0,.1), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10,60)) +
  labs(x = "Age difference (father age - child age)", y = "Proportion of pairs") +
  annotate(x =min(SoleFathAgeDiffsLong$AgeDiff), y = +Inf, label = min(SoleFathAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =median(SoleFathAgeDiffsLong$AgeDiff), y = +Inf, label = median(SoleFathAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =max(SoleFathAgeDiffsLong$AgeDiff), y = +Inf, label = max(SoleFathAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#    ggsave(File10SoleFathers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File10SoleFathers.pdf")




# sole mothers

SoleMothers <- SoleMotherFamilies %>%
  filter(Type == "Sole Mother")

SoleMotherKids <- SoleMotherFamilies %>%
  filter(Type == "Child")


SoleMothAgeDiffs <- left_join(SoleMotherKids, SoleMothers, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.y - Age.x) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())


SoleMothAgeDiffsLong <- SoleMothAgeDiffs  %>%
  tidyr::uncount(weights = Count)

File10SoleMothers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleMothAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleMothAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10,60)) +
  labs(x = "Age difference (mother age - child age)", y = "Proportion of pairs") +
  annotate(x =min(SoleMothAgeDiffsLong$AgeDiff), y = +Inf, label = min(SoleMothAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =median(SoleMothAgeDiffsLong$AgeDiff), y = +Inf, label = median(SoleMothAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =max(SoleMothAgeDiffsLong$AgeDiff), y = +Inf, label = max(SoleMothAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#   ggsave(File10SoleMothers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File10SoleMothers.pdf")


# mothers


CplsMums <- MuminCpls %>%
  filter(Type == "Opposite sex with child")

KidsInCpls <- MuminCpls %>%
  filter(Type == "Child")

CplsMumsAgeDiffs <- left_join(KidsInCpls, CplsMums, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.y - Age.x) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())


CplsMumsAgeDiffsLong <- CplsMumsAgeDiffs  %>%
  tidyr::uncount(weights = Count)

File10CplsMums <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = CplsMumsAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(CplsMumsAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(CplsMumsAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(CplsMumsAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10,60)) +
  labs(x = "Age difference (mother age - child age)", y = "Proportion of pairs") +
  annotate(x =min(CplsMumsAgeDiffsLong$AgeDiff), y = +Inf, label = min(CplsMumsAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =median(CplsMumsAgeDiffsLong$AgeDiff), y = +Inf, label = median(CplsMumsAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  annotate(x =max(CplsMumsAgeDiffsLong$AgeDiff), y = +Inf, label = max(CplsMumsAgeDiffsLong$AgeDiff),
           vjust = 2, geom = "label")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#    ggsave(File10CplsMums, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File10CplsMums.pdf")











####################################################################################################
# Table of household types
####################################################################################################

DataframeOfHouseholdTypes <- data.frame(HouseholdType = c("Female sole parent", "Male sole parent",
                                                          "Single-sex couple with child", "Opposite-sex couple with child",
                                                          "Opposite-sex couple with unrelated person",
                                                          "Household of unrelated people"),
                                        Count = c(nrow(SoleMothers), nrow(SoleFathers), (nrow(SameSexF4C)+nrow(SameSexM4C))/4,
                                                  nrow(CplsMums), nrow(CouplesWithoutKids)/4, nrow(OtherPeople)/4))

DataframeOfHouseholdTypes <- DataframeOfHouseholdTypes %>%
  mutate(Prop = round((Count/sum(Count)),3)) %>%
  arrange(-Count)

# Save the output file

saveRDS(FourPersonHouseholdsComplete, file = "PhDRData/InterimHouseholdSizeWork/File10FourPerson/FourPersonHouseholdsComplete.rds")

