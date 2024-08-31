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

FiveUsualResidents <- File4CorrectHrs %>%
  filter(UsualResidents == "Five Usual Residents")





####################################################################################################
# graph of ages, relationship status, working hours, all by sex.
####################################################################################################

# ages
AgesProp <- FiveUsualResidents %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))

AllAges5P<- ggplot(AgesProp, aes(x=Age, y=CumAgePercent, colour=Sex)) +
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

#   ggsave(AllAges5P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/AllAges5P.pdf")


# ages of partnered people only

AgesPropPart <- FiveUsualResidents %>%
  filter(PartnershipStatus == "Partnered") %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))


PartneredAges5P<- ggplot(AgesPropPart, aes(x=Age, y=CumAgePercent, colour=Sex)) +
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


#   ggsave(PartneredAges5P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/PartneredAges5P.pdf")


# working hours

AgesPropHours <- FiveUsualResidents %>%
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



WorkingHours5P<- ggplot(AgesPropHours, aes(x=HoursWorkedOrd, y=HoursPercent, fill=Sex)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Hours worked per week") + ylab("Proportion") +
  scale_y_continuous(limits = c(0,0.5), breaks = seq(0, 0.5, by = 0.1), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(WorkingHours5P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/WorkingHours5P.pdf")

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


rm(AgesProp, AgesPropHours, AgesPropPart, AllAges5P, LongerProps, PartneredAges5P, WorkingHours5P,
   File4CorrectHrs,PropWorking)












####################################################################################################
####################################################################################################
# create the couples - no same-sex
####################################################################################################
####################################################################################################

# how many partnered by sex?

PartneredMen <- FiveUsualResidents %>%
  filter(Sex == "Male", PartnershipStatus == "Partnered")

PartneredWomen <- FiveUsualResidents %>%
  filter(Sex == "Female", PartnershipStatus == "Partnered")

# get starting household ID

FourPersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File10FourPerson/FourPersonHouseholdsComplete.rds")


StartHHID <- max(FourPersonHouseholdsComplete$HouseholdID) + 1

rm(FourPersonHouseholdsComplete)








#######################################################################################
# generate opposite sex couples
#######################################################################################



OppSex5P <- pairnorm(PartneredMen, "ID", "Age", PartneredWomen, "ID", "Age", directxi = 2, directomega = 3,
                     alphaused = 0, StartHHID, "HouseholdID",
                     userseed = TheRandomSeeds[101])                         #################### seed 101 %%%%%%%%%%%%%%%%

OppositeSexCouples <- OppSex5P$Matched


mean(PartneredMen$Age)
mean(PartneredWomen$Age)

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

round((NumWomenOlder/nrow(OppositeSexAgeDiffsLong)),3)

File11Partnered <- ggplot() +
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


#   ggsave(File11Partnered, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File11Partnered.pdf")


rm(men, mergedsexes, OppositeSexAgeDiffs, OppositeSexAgeDiffsLong, OppSex5P, PartneredMen, PartneredWomen, women, 
   StartHHID, File11Partnered)






















#######################################################################################
#######################################################################################
# create the child households
#######################################################################################
#######################################################################################

####################################################################################################
# get the kids counts
####################################################################################################

PeopleByAge <- FiveUsualResidents %>%
  group_by(Age) %>%
  summarise(Num = n())

# apply probs

NumKids5PH <- left_join(PeopleByAge, ChildPropsAtHome3, by = "Age") %>%
  filter(!(is.na(Fits))) %>%
  mutate(NumNeeded = round(Num*Fits)) %>%
  filter(NumNeeded > 0)

# sample to get the kids

# use diff sample

UnPartneredResidents <- FiveUsualResidents %>%
  filter(PartnershipStatus == "Not Partnered")

The15161718InSchool <- FiveUsualResidents %>%
  filter(EducationStatus == "Y", Age %in% c(15, 16, 17, 18))

NumKidsByAge15161718 <- The15161718InSchool %>%
  group_by(Age) %>%
  summarise(NumInAge = n())

ExtrasNeeded15161718 <- left_join(NumKidsByAge15161718, NumKids5PH, by = "Age") %>%
  mutate(StilltoSample = NumNeeded - NumInAge)

UnpartneredLeft <- UnPartneredResidents %>%
  filter(!ID %in% The15161718InSchool$ID)

UpdatedSampleCounts <- left_join(NumKids5PH, ExtrasNeeded15161718,
                                 by = c("Age", "Num", "Fits", "ExpectedNum", "NumNeeded")) %>%
  select(-NumInAge) %>%
  mutate(SampleSize = ifelse(is.na(StilltoSample), NumNeeded, StilltoSample),
         SampleSize = ifelse(SampleSize > 0, SampleSize, 0)) %>%
  select(Age, SampleSize)


TheKids <- diffsample(UnpartneredLeft, "Age", UpdatedSampleCounts, "Age", "SampleSize",
                      userseed=TheRandomSeeds[102])                         #################### seed 102 %%%%%%%%%%%%%%%%

TheKids <- bind_rows(TheKids, The15161718InSchool)

# test counts
table(UnPartneredResidents$Age)

table(TheKids$Age)

# check counts
isTRUE(sum(NumKids5PH$NumNeeded) == nrow(TheKids))
sum(NumKids5PH$NumNeeded)
nrow(TheKids)


# # the household split is that .050 households with children are sole parents.
# the couples household contain 3 kids
# the sole parent households contain 4
# split is on number of households NOT number of children
# there are 1443 so split must be that sole parents households children count is divisible by 4
# and couples households children count is divisible by 3

# 24 sole parent households, 449 couples families.

# split is 1347 kids to couples, 96 to sole parents.
# with 1347 kids, and 3 kids per couple, that is 449 couples required
# there are 485 couples, so 36 couples have no child but an additional three people in the household, or 7%




####################################################################################################
# sole parent households
####################################################################################################
####################################################################################################
# Dads
####################################################################################################

SoleFathNum <- round(96 * .138, 0)  

SoleFathNum <- SoleFathNum + 9 # constructs the three needed.

# get potential sole fathers

PotSoleFathers <- FiveUsualResidents %>%
  filter(!ID %in% c(OppositeSexCouples$ID, TheKids$ID),
         Sex == "Male")

# sample the kids
set.seed(TheRandomSeeds[103])                                                  #################### seed 103 %%%%%%%%%%%%%%%%
FathKids <- TheKids %>%
  slice_sample(n=SoleFathNum, replace = FALSE)


# add kids to sole fathers

StartID <- max(OppositeSexCouples$HouseholdID) + 1

SoleFathHouseholds <- pairchild(FathKids, "ID", "Age", numchild = 4, twinprob = 0.029, PotSoleFathers, "ID", "Age",
                                minparage = 18, maxparage = 50, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[104], maxdiff = 6)       #################### seed 104 %%%%%%%%%%%%%%%%


SoleFatherFamilies <- SoleFathHouseholds$Matched
SoleFatherUnmatchedKids <- SoleFathHouseholds$Children # 6 unmatched, down have the right number sole father households


numFathHH <- as.numeric(nrow(SoleFatherFamilies)/5)

SoleFatherFamilies <- SoleFatherFamilies %>%
  mutate(Type = ifelse(row_number() <= numFathHH*4, "Child", "Sole Father"))

table(SoleFatherFamilies$Type)



####################################################################################################
# Mums
####################################################################################################
mothersample <- round(96 * .862, 0) 
mothersample <- mothersample + 7

set.seed(TheRandomSeeds[105])                                                  #################### seed 105 %%%%%%%%%%%%%%%%
TheKidsSubset1 <- TheKids %>%
  filter(!ID %in% SoleFatherFamilies$ID) %>%
  slice_sample(n=mothersample, replace = FALSE)


PotSoleMothers <- FiveUsualResidents %>%
  filter(!ID %in% c(OppositeSexCouples$ID, TheKids$ID),
         Sex == "Female")


# add kids to sole mothers

StartID <- max(SoleFatherFamilies$HouseholdID) + 1

SoleMothHouseholds <- pairchild(TheKidsSubset1, "ID", "Age", numchild = 4, twinprob = 0.029, PotSoleMothers, "ID", "Age",
                                minparage = 16, maxparage = 49, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[106], maxdiff = 6)        #################### seed 106 %%%%%%%%%%%%%%%%


SoleMotherFamilies <- SoleMothHouseholds$Matched
SoleMotherUnmatchedKids <- SoleMothHouseholds$Children

numMothHH <- as.numeric(nrow(SoleMotherFamilies)/5)

SoleMotherFamilies <- SoleMotherFamilies %>%
  mutate(Type = ifelse(row_number() <= numMothHH*4, "Child", "Sole Mother"))

table(SoleMotherFamilies$Type)











####################################################################################################
####################################################################################################
# Couples with three children
####################################################################################################
####################################################################################################

TheKidsSubset2 <- TheKids %>%
  filter(!ID %in% c(SoleFatherFamilies$ID, SoleMotherFamilies$ID))


# get mother

MumsFromCouples <- OppositeSexCouples %>%
  filter(Sex == "Female")


MumPlusKidHouseholds <- pairchildNum(TheKidsSubset2, "ID", "Age", numchild = 3, twinprob = 0.029, MumsFromCouples, "ID", "Age",
                                     minparage = 16, maxparage = 50, "HouseholdID",
                                     userseed = TheRandomSeeds[108], maxdiff = 6)      #################### seed 108 %%%%%%%%%%%%%%%%


MuminCpls <- MumPlusKidHouseholds$Matched
KidsNotInHouseholds <- MumPlusKidHouseholds$Children
MumNotInCpls <- MumPlusKidHouseholds$Adults

LastChildRow <- nrow(MuminCpls)/4

MuminCpls <- MuminCpls %>%
  mutate(Type = ifelse(row_number() <= LastChildRow*3, "Child", "Opposite sex with child"))


MalesInOppSexCpls <- OppositeSexCouples %>%
  filter(Sex == "Male", HouseholdID %in% MuminCpls$HouseholdID) %>%
  mutate(Type = "Opposite sex with child")

OppSexCouplesWithKids <- bind_rows(MuminCpls, MalesInOppSexCpls)

table(OppSexCouplesWithKids$Type)



# get final mother
min <- 16+max(KidsNotInHouseholds$Age)+1 # partner was going to be 15, failed the 18 (male) test on sampling
max <- 49+min(KidsNotInHouseholds$Age)

set.seed(TheRandomSeeds[109])                                                       #################### seed 109 %%%%%%%%%%%%%%%%
SelectedMum <- MumNotInCpls %>%
  filter(between(Age, min, max)) %>%
  slice_sample(n=1) %>%
  mutate(Type = "Opposite sex with child")

HouseholdIDMum <- as.numeric(SelectedMum %>%
                            select(HouseholdID) %>%
                            pull(HouseholdID))

KidsNotInHouseholds <- KidsNotInHouseholds %>%
  mutate(HouseholdID = HouseholdIDMum,
         Type = "Child")

MalesInOppSexCpls2 <- OppositeSexCouples %>%
  filter(Sex == "Male", HouseholdID == HouseholdIDMum) %>%
  mutate(Type = "Opposite sex with child")

OppSexCouplesWithKids2 <- bind_rows(SelectedMum, KidsNotInHouseholds, MalesInOppSexCpls2)

table(OppSexCouplesWithKids2$Type)


rm(KidsNotInHouseholds, MalesInOppSexCpls, MalesInOppSexCpls2, MumsFromCouples, LastChildRow, StartID)













####################################################################################################
####################################################################################################
# Couples with three people added
####################################################################################################
####################################################################################################

RemainingCouples <- OppositeSexCouples %>%
  filter(!ID %in% c(OppSexCouplesWithKids$ID, OppSexCouplesWithKids2$ID))

# randomly take one person out
set.seed(TheRandomSeeds[110])                                         #################### seed 110 %%%%%%%%%%%%%%%%
CplMatchToOther <- RemainingCouples %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE)

RemainingPeople <- FiveUsualResidents %>%
  filter(!(ID %in% c(SoleFatherFamilies$ID, SoleMotherFamilies$ID, OppositeSexCouples$ID, TheKids$ID)))


# add an extra person

OtherPersonToCpl <- otherNum(CplMatchToOther, "ID", "Age", "HouseholdID", RemainingPeople, "ID", "Age",numadd = 3,
                             sdused = 4, userseed = TheRandomSeeds[111])         #################### seed 111 %%%%%%%%%%%%%%%%

FinishedNoKidsCpls <- OtherPersonToCpl$Matched

LastCplRow <- nrow(FinishedNoKidsCpls)/4

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


OtherOnlyPeople <- RemainingPeople %>%
  filter(!ID %in% CouplesWithoutKids$ID)

OthersStartNum <- max(SoleMotherFamilies$HouseholdID) +1


OtherPeopleHouseholdsList <- other(OtherOnlyPeople, "ID", "Age", 5, sdused = 3, OthersStartNum, "HouseholdID",
                                   userseed = TheRandomSeeds[112],           #################### seed 112 %%%%%%%%%%%%%%%%
                                   numiters = 10000)       

OtherPeople <- OtherPeopleHouseholdsList$Matched %>%
  mutate(Type = "Other")

















####################################################################################################
####################################################################################################
# Everyone into the same household data frame
####################################################################################################
####################################################################################################


FivePersonHouseholdsComplete <- bind_rows(OppSexCouplesWithKids, OppSexCouplesWithKids2, SoleFatherFamilies,
                                          SoleMotherFamilies, CouplesWithoutKids, OtherPeople)

duplicates <- FivePersonHouseholdsComplete %>%
  group_by(ID) %>%
  summarise(Count = n()) %>%
  filter(Count > 1)

duplicates <- FivePersonHouseholdsComplete %>%
  group_by(HouseholdID) %>%
  summarise(Count = n()) %>%
  filter(Count != 5)




































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

File11SoleFathers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleFathAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleFathAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2), limits = c(0,.2), expand = c(0, 0.001)) +
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

#    ggsave(File11SoleFathers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File11SoleFathers.pdf")




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

File11SoleMothers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleMothAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleMothAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1), limits = c(0,.1), expand = c(0, 0.001)) +
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

#   ggsave(File11SoleMothers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File11SoleMothers.pdf")


# mothers


CplsMums <- MuminCpls %>%
  filter(Type == "Opposite sex with child")
ExtraCplsNums <- SelectedMum %>%
  filter(Type == "Opposite sex with child")
CplsMums <- bind_rows(CplsMums, ExtraCplsNums)


KidsInCpls <- MuminCpls %>%
  filter(Type == "Child")
ExtraKidsInCpls <- OppSexCouplesWithKids2 %>%
  filter(Type == "Child")
KidsInCpls <- bind_rows(KidsInCpls, ExtraKidsInCpls)

CplsMumsAgeDiffs <- left_join(KidsInCpls, CplsMums, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.y - Age.x) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())


CplsMumsAgeDiffsLong <- CplsMumsAgeDiffs  %>%
  tidyr::uncount(weights = Count)

File11CplsMums <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = CplsMumsAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(CplsMumsAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(CplsMumsAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(CplsMumsAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .01, .02, .03, .04, .05), limits = c(0,.05), expand = c(0, 0.001)) +
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

#    ggsave(File11CplsMums, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File11CplsMums.pdf")











####################################################################################################
# Table of household types
####################################################################################################

DataframeOfHouseholdTypes <- data.frame(HouseholdType = c("Female sole parent", "Male sole parent",
                                                          "Opposite-sex couple with children",
                                                          "Opposite-sex couple with unrelated people",
                                                          "Household of unrelated people"),
                                        Count = c(nrow(SoleMothers), nrow(SoleFathers), nrow(CplsMums), 
                                                  nrow(CouplesWithoutKids)/5, nrow(OtherPeople)/5))

DataframeOfHouseholdTypes <- DataframeOfHouseholdTypes %>%
  mutate(Prop = round((Count/sum(Count)),3)) %>%
  arrange(-Count)

# check count
sum(DataframeOfHouseholdTypes$Count)*5

# Save the output file

saveRDS(FivePersonHouseholdsComplete, file = "PhDRData/InterimHouseholdSizeWork/File11FivePerson/FivePersonHouseholdsComplete.rds")

