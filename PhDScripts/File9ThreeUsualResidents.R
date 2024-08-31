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
ChildPropsAtHome2 <- readRDS("PhDRData/ChildPropsAtHome2.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# extract out the three usual residents

ThreeUsualResidents <- File4CorrectHrs %>%
  filter(UsualResidents == "Three Usual Residents")





####################################################################################################
# graph of ages, relationship status, working hours, all by sex.
####################################################################################################

# ages
AgesProp <- ThreeUsualResidents %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))

AllAges3P<- ggplot(AgesProp, aes(x=Age, y=CumAgePercent, colour=Sex)) +
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

#   ggsave(AllAges3P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/AllAges3P.pdf")


# ages of partnered people only

AgesPropPart <- ThreeUsualResidents %>%
  filter(PartnershipStatus == "Partnered") %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))


PartneredAges3P<- ggplot(AgesPropPart, aes(x=Age, y=CumAgePercent, colour=Sex)) +
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


#   ggsave(PartneredAges3P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/PartneredAges3P.pdf")


# working hours

AgesPropHours <- ThreeUsualResidents %>%
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



WorkingHours3P<- ggplot(AgesPropHours, aes(x=HoursWorkedOrd, y=HoursPercent, fill=Sex)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Hours worked per week") + ylab("Proportion") +
  scale_y_continuous(limits = c(0,0.5), breaks = seq(0, 0.5, by = 0.1), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(WorkingHours3P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/WorkingHours3P.pdf")

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


rm(AgesProp, AgesPropHours, AgesPropPart, AllAges3P, LongerProps, PartneredAges3P, WorkingHours3P, 
   File4CorrectHrs,PropWorking)












####################################################################################################
####################################################################################################
# create the couples
####################################################################################################
####################################################################################################

# need same-sex first
# need to create couples now as want to remove couples from potential sole parents

PartneredMen <- ThreeUsualResidents %>%
  filter(Sex == "Male", PartnershipStatus == "Partnered")

PartneredWomen <- ThreeUsualResidents %>%
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

TwoPersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File8TwoPerson/TwoPersonHouseholdsComplete.rds")

StartHHID <- max(TwoPersonHouseholdsComplete$HouseholdID) + 1

# females

SameSexF3 <- fastmatch(PartneredWomen, "Age", FSameSexProb, AgeUpweight, 25,54, StartHHID,
                       "HouseholdID", userseed=TheRandomSeeds[71])             #################### seed 71 %%%%%%%%%%%%%%%%


SameSexF3Type <- SameSexF3 %>%
  select(ID) %>%
  mutate(Type = "Single sex with kids")


StartHHID <- max(SameSexF3$HouseholdID) + 1

SameSexM3 <- fastmatch(PartneredMen, "Age", MSameSexProb, AgeUpweight, 25,54, StartHHID,
                       "HouseholdID", userseed=TheRandomSeeds[72])         #################### seed 72 %%%%%%%%%%%%%%%%


SameSexM3Type <- SameSexM3 %>%
  select(ID) %>%
  mutate(Type = "Single sex with kids")


rm(TABLECODE8160, TABLECODE8161, TwoPersonHouseholdsComplete, AgeUpweight, FSameSexProb, MSameSexProb,
   StartHHID, fastmatch)







#######################################################################################
# generate opposite sex couples
#######################################################################################

# remaining men
PartneredMenLeft <- PartneredMen %>%
  filter(!ID %in% SameSexM3$ID)

#remaining women
PartneredWomenLeft <- PartneredWomen %>%
  filter(!ID %in% SameSexF3$ID)

StartID <- max(SameSexM3$HouseholdID) + 1


OppSex3P <- pairnorm(PartneredMenLeft, "ID", "Age", PartneredWomenLeft, "ID", "Age", directxi = 2, directomega = 3,
                     alphaused = 0, StartID, "HouseholdID", 
                     userseed = TheRandomSeeds[73])                         #################### seed 73 %%%%%%%%%%%%%%%%

OppositeSexCouples <- OppSex3P$Matched
UnmatchedPartnered <- OppSex3P$Unmatched

mean(PartneredMenLeft$Age)
mean(PartneredWomenLeft$Age)

OppositeSexCouplesType <- OppositeSexCouples %>%
  select(ID) %>%
  mutate(Type = "Opposite sex with kids")

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

File9Partnered <- ggplot() +
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


#   ggsave(File9Partnered, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File9Partnered.pdf")


rm(men, mergedsexes, OppositeSexAgeDiffs, OppositeSexAgeDiffsLong, OppSex3P, PartneredMen, PartneredWomen, PartneredMenLeft,
   PartneredWomenLeft, women, StartID, File9Partnered)






















#######################################################################################
#######################################################################################
# create the child households
#######################################################################################
#######################################################################################

####################################################################################################
# get the kids counts
####################################################################################################

PeopleByAge <- ThreeUsualResidents %>%
  group_by(Age) %>%
  summarise(Num = n())

# apply probs

NumKids3PH <- left_join(PeopleByAge, ChildPropsAtHome2, by = "Age") %>%
  filter(!(is.na(Fits))) %>%
  mutate(NumNeeded = round(Num*Fits)) %>%
  filter(NumNeeded > 0)

# sample to get the kids

# use diff sample

UnPartneredResidents <- ThreeUsualResidents %>%
  filter(PartnershipStatus == "Not Partnered")

The15161718InSchool <- ThreeUsualResidents %>%
  filter(EducationStatus == "Y", Age %in% c(15, 16, 17, 18))

NumKidsByAge15161718 <- The15161718InSchool %>%
  group_by(Age) %>%
  summarise(NumInAge = n())

ExtrasNeeded15161718 <- left_join(NumKidsByAge15161718, NumKids3PH, by = "Age") %>%
  mutate(StilltoSample = NumNeeded - NumInAge)

UnpartneredLeft <- UnPartneredResidents %>%
  filter(!ID %in% The15161718InSchool$ID)

UpdatedSampleCounts <- left_join(NumKids3PH, ExtrasNeeded15161718, 
                                 by = c("Age", "Num", "Fits", "ExpectedNum", "NumNeeded")) %>%
  select(-NumInAge) %>%
  mutate(SampleSize = ifelse(is.na(StilltoSample), NumNeeded, StilltoSample),
         SampleSize = ifelse(SampleSize > 0, SampleSize, 0)) %>%
  select(Age, SampleSize)


TheKids <- diffsample(UnpartneredLeft, "Age", UpdatedSampleCounts, "Age", "SampleSize", 
                      userseed=TheRandomSeeds[74])                         #################### seed 74 %%%%%%%%%%%%%%%%

TheKids <- bind_rows(TheKids, The15161718InSchool)

TheKidsType <- TheKids %>%
  select(ID) %>%
  mutate(Type = "Child")

# test counts
table(UnPartneredResidents$Age)

table(TheKids$Age)

# check counts
isTRUE(sum(NumKids3PH$NumNeeded) == nrow(TheKids))
sum(NumKids3PH$NumNeeded)
nrow(TheKids)


# the household split is that .224 households are sole parents.

# split is 974 kids to couples, 550 to sole parents.
# 100 couples, so 10.1% have another person in the household



####################################################################################################
# sole parent households
####################################################################################################
####################################################################################################
# Dads
####################################################################################################

SoleFathNum <- round(550 * .138, 0)

# get potential sole fathers

PotSoleFathers <- ThreeUsualResidents %>%
  filter(!ID %in% c(SameSexM3$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Male")

# sample the kids
set.seed(TheRandomSeeds[75])                                                    #################### seed 75 %%%%%%%%%%%%%%%%
FathKids <- TheKids %>%
  slice_sample(n=SoleFathNum, replace = FALSE)


# add kids to sole fathers

StartID <- max(OppositeSexCouples$HouseholdID) + 1

SoleFathHouseholds <- pairchild(FathKids, "ID", "Age", numchild = 2, twinprob = 0.029, PotSoleFathers, "ID", "Age",
                                minparage = 18, maxparage = 50, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[76], maxdiff = 4)           #################### seed 76 %%%%%%%%%%%%%%%%


SoleFatherFamilies <- SoleFathHouseholds$Matched
SoleFatherUnmatchedKids <- SoleFathHouseholds$Children
SoleFatherUnmatchedMen <- SoleFathHouseholds$Adults

numFathHH <- as.numeric(nrow(SoleFatherFamilies)/3)

SoleFatherFamilies <- SoleFatherFamilies %>%
  mutate(Type = ifelse(row_number() <= numFathHH*2, "Child", "Sole Father"))

table(SoleFatherFamilies$Type)



####################################################################################################
# Mums
####################################################################################################

set.seed(TheRandomSeeds[77])                                                    #################### seed 77 %%%%%%%%%%%%%%%%
TheKidsSubset1 <- TheKids %>%
  filter(!ID %in% SoleFatherFamilies$ID) %>%
  slice_sample(n=round(550 * .862, 0), replace = FALSE)


PotSoleMothers <- ThreeUsualResidents %>%
  filter(!ID %in% c(SameSexF3$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Female")


# add kids to sole fathers

StartID <- max(SoleFatherFamilies$HouseholdID) + 1

SoleMothHouseholds <- pairchild(TheKidsSubset1, "ID", "Age", numchild = 2, twinprob = 0.029, PotSoleMothers, "ID", "Age",
                                minparage = 16, maxparage = 50, StartID, "HouseholdID",
                                userseed = TheRandomSeeds[78], maxdiff = 4)           #################### seed 78 %%%%%%%%%%%%%%%%


SoleMotherFamilies <- SoleMothHouseholds$Matched
SoleMotherUnmatchedKids <- SoleMothHouseholds$Children
SoleMotherUnmatchedWomen <- SoleMothHouseholds$Adults

numMothHH <- as.numeric(nrow(SoleMotherFamilies)/3)

SoleMotherFamilies <- SoleMotherFamilies %>%
  mutate(Type = ifelse(row_number() <= numMothHH*2, "Child", "Sole Mother"))

table(SoleMotherFamilies$Type)












####################################################################################################
####################################################################################################
# Couples with one child
####################################################################################################
####################################################################################################

TheKidsSubset2 <- TheKids %>%
  filter(!ID %in% c(SoleFatherFamilies$ID, SoleMotherFamilies$ID))

####################################################################################################
# same-sex male couple
####################################################################################################


#--------------------------------------------------------------------------------
# DO THE BELOW IN ONE GO SO SEED FROM FIRST DRAW SETS THE SAMPLING
# #
# #
# #
set.seed(TheRandomSeeds[79])                                         #################### seed 79 %%%%%%%%%%%%%%%%
SameSexFath <-  SameSexM3 %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE)
HouseholdIDFath <- as.numeric(SameSexFath %>%
  select(HouseholdID) %>%
  distinct() %>%
  pull(HouseholdID))
# draw from the 4-parameter beta distribution
FathAgeDraw <- round(PearsonDS::rpearsonI(n=1, a=3.72, b=6.07, location=13.93, scale=47.08),0)
SelectedChildDad <- TheKidsSubset2 %>%
  filter(Age == SameSexFath$Age - FathAgeDraw) %>%
  slice_sample(n=1, replace = FALSE) %>%
  mutate(HouseholdID = HouseholdIDFath)

FathKidAge <- SameSexFath$Age - FathAgeDraw

SameSexM3C <- bind_rows(SameSexM3, SelectedChildDad) %>%
  mutate(Type = ifelse(Age == FathKidAge, "Child", "Single sex with child"))

####################################################################################################
# same-sex female couple
####################################################################################################

TheKidsSubset3 <- TheKidsSubset2 %>%
  filter(!ID %in% SameSexM3C$ID)

SameSexMoth <-  SameSexF3 %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE)
HouseholdIDMoth <- as.numeric(SameSexMoth %>%
                                select(HouseholdID) %>%
                                distinct() %>%
                                pull(HouseholdID))
# draw from the 4-parameter beta distribution
MothAgeDraw <- round(PearsonDS::rpearsonI(n=1, a=2.21, b=3.66, location=16.5, scale=33.36),0)
SelectedChildMum <- TheKidsSubset3 %>%
  filter(Age == SameSexMoth$Age - MothAgeDraw) %>%
  slice_sample(n=1, replace = FALSE) %>%
  mutate(HouseholdID = HouseholdIDMoth)

MothKidAge <- SameSexMoth$Age - MothAgeDraw

SameSexF3C <- bind_rows(SameSexF3, SelectedChildMum) %>%
  mutate(Type = ifelse(Age == MothKidAge, "Child", "Single sex with child"))

# #
# # 
# # 
# END IN ONE GO SECTION
#--------------------------------------------------------------------------------











####################################################################################################
# Couples with one child
####################################################################################################

TheKidsSubset4 <- TheKidsSubset3 %>%
  filter(!ID %in% SameSexF3C$ID)


# get mother

MumsFromCouples <- OppositeSexCouples %>%
  filter(Sex == "Female")

# 
# CouplesAndKids <- pair4PbetaNum(TheKidsSubset4, "ID", "Age", MumsFromCouples, "ID", "Age", shapeA=2.21, shapeB=3.66, 
#                              locationP=16.5, scaleP=33.36, "HouseholdID", 
#                              userseed = TheRandomSeeds[80])                    #################### seed 80  %%%%%%%%%%%%%%%%
# 
# # did not produce the required distribution, re-run with the advanced version

# remove one 15 and one 16 year old
set.seed(TheRandomSeeds[81])                                                   #################### seed 81 %%%%%%%%%%%%%%%%
TheKids4SP <- TheKidsSubset4 %>%
  filter(Age %in% c(15,16)) %>%
  group_by(Age) %>%
  slice_sample(n=1, replace = FALSE)

TheKidsSubset5 <- TheKidsSubset4 %>%
  filter(!ID %in% TheKids4SP$ID)

MumPlusKidHouseholds <- pair4PbetaNumAdv(TheKidsSubset4, "ID", "Age", MumsFromCouples, "ID", "Age", shapeA=2.21, shapeB=3.66, 
                                       locationP=16.5, scaleP=33.36, "HouseholdID", 
                                       userseed = TheRandomSeeds[82], attempts = 10)             
                                                                               #################### seed 82 %%%%%%%%%%%%%%%%

MuminCpls <- MumPlusKidHouseholds$Matched
KidsNotInHouseholds <- MumPlusKidHouseholds$Smaller
NotMums <- MumPlusKidHouseholds$Larger

LastChildRow <- nrow(MuminCpls)/2

MuminCpls <- MuminCpls %>%
  mutate(Type = ifelse(row_number() <= LastChildRow, "Child", "Opposite sex with child"))


MalesInOppSexCpls <- OppositeSexCouples %>%
  filter(Sex == "Male", HouseholdID %in% MuminCpls$HouseholdID) %>%
  mutate(Type = "Opposite sex with child")

OppSexCouplesWithKids <- bind_rows(MuminCpls, MalesInOppSexCpls)

# # add the two other kids to a sole mother
# 
# #--------------------------------------------------------------------------------
# # DO THE BELOW IN ONE GO SO SEED FROM FIRST DRAW SETS THE SAMPLING
# set.seed(TheRandomSeeds[83])                                         #################### seed 83 %%%%%%%%%%%%%%%%
# # draw from the 4-parameter beta distribution
# MothAgeDraw <- round(PearsonDS::rpearsonI(n=1, a=2.21, b=3.66, location=16.5, scale=33.36),0)
# #draw the child age
# ChildAgeDraw <- as.numeric(TheKids4SP %>%
#   ungroup() %>%
#   slice_sample(n=1, replace = FALSE) %>%
#   select(Age) %>%
#   pull(Age))
# 
# SelectedSoleMum <- SoleMotherUnmatchedWomen %>%
#   filter(Age >= 15 + ChildAgeDraw) %>%
#   slice_sample(n=1, replace = FALSE) 
# # END IN ONE GO SECTION
# #--------------------------------------------------------------------------------
# 
# FinalSPKidsHHID <- max(SoleMotherFamilies$HouseholdID) + 1
# 
# FinalSPKidHousehold <- bind_rows(TheKids4SP, SelectedSoleMum) %>%
#   mutate(HouseholdID = FinalSPKidsHHID,
#          Type = ifelse(Age < 20, "Child", "Sole Mother"))















####################################################################################################
####################################################################################################
# Couples with one person added
####################################################################################################
####################################################################################################

RemainingCouples <- OppositeSexCouples %>%
  filter(!ID %in% OppSexCouplesWithKids$ID)

# randomly take one person out 
set.seed(TheRandomSeeds[84])                                         #################### seed 84 %%%%%%%%%%%%%%%%
CplMatchToOtherYoung <- RemainingCouples %>%
  filter(Age < 60) %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE)

PeopleForOtherYoung <- ThreeUsualResidents %>%
  filter(!ID %in% c(OppositeSexCouples$ID, TheKids$ID, SameSexF3$ID, SameSexF3C$ID, SameSexM3$ID, SameSexM3C$ID,
                    # SoleFatherFamilies$ID, SoleMotherFamilies$ID, FinalSPKidHousehold$ID),
                    SoleFatherFamilies$ID, SoleMotherFamilies$ID),
         Age < 40)


# add an extra person

OtherPersonToCplYoung <- otherNum(CplMatchToOtherYoung, "ID", "Age", "HouseholdID", PeopleForOtherYoung, "ID", "Age",numadd = 1, 
                             sdused = 4, userseed = TheRandomSeeds[85])         #################### seed 85 %%%%%%%%%%%%%%%%
                             

FinishedNoKidsCplsYoung <- OtherPersonToCplYoung$Matched



# repeat for the older couples
set.seed(TheRandomSeeds[86])                                         #################### seed 86 %%%%%%%%%%%%%%%%
CplMatchToOtherOlder <- RemainingCouples %>%
  filter(!HouseholdID %in% FinishedNoKidsCplsYoung$HouseholdID) %>%
  group_by(HouseholdID) %>%
  slice_sample(n=1, replace = FALSE)


PeopleForOtherOld <- ThreeUsualResidents %>%
  filter(!ID %in% c(OppositeSexCouples$ID, TheKids$ID, SameSexF3$ID, SameSexF3C$ID, SameSexM3$ID, SameSexM3C$ID,
                    # SoleFatherFamilies$ID, SoleMotherFamilies$ID, FinalSPKidHousehold$ID, FinishedNoKidsCplsYoung$ID),
                    SoleFatherFamilies$ID, SoleMotherFamilies$ID, FinishedNoKidsCplsYoung$ID),
         Age > 55)


OtherPersonToCplOlder <- otherNum(CplMatchToOtherOlder, "ID", "Age", "HouseholdID", PeopleForOtherOld, "ID", "Age",numadd = 1, 
                                  sdused = 4, userseed = TheRandomSeeds[87])         #################### seed 87 %%%%%%%%%%%%%%%%


FinishedNoKidsCplsOlder <- OtherPersonToCplOlder$Matched


# put them all together

LastCplYoungerRow <- nrow(FinishedNoKidsCplsYoung)/2

FinishedNoKidsCplsYoung <- FinishedNoKidsCplsYoung %>%
  mutate(Type = ifelse(row_number() <= LastCplYoungerRow, "Opposite sex without child", "Other with couples"))



LastCplOlderRow <- nrow(FinishedNoKidsCplsOlder)/2

FinishedNoKidsCplsOlder <- FinishedNoKidsCplsOlder %>%
  mutate(Type = ifelse(row_number() <= LastCplOlderRow, "Opposite sex without child", "Other with couples"))

# add in the missing people from the couples, those who were selected for matching

UnselectedPersonInCouple <- RemainingCouples %>%
  filter(!ID %in% c(FinishedNoKidsCplsYoung$ID, FinishedNoKidsCplsOlder$ID)) %>%
  mutate(Type = "Opposite sex without child")


CouplesWithoutKids <- bind_rows(FinishedNoKidsCplsYoung, FinishedNoKidsCplsOlder, UnselectedPersonInCouple)





















####################################################################################################
####################################################################################################
# Create last households - unrelated people only
####################################################################################################
####################################################################################################


OtherOnlyPeople <- ThreeUsualResidents %>%
  filter(!ID %in% c(OppositeSexCouples$ID, TheKids$ID, SameSexF3$ID, SameSexF3C$ID, SameSexM3$ID, SameSexM3C$ID,
                    # SoleFatherFamilies$ID, SoleMotherFamilies$ID, FinalSPKidHousehold$ID, CouplesWithoutKids$ID))
                    SoleFatherFamilies$ID, SoleMotherFamilies$ID, CouplesWithoutKids$ID))

# OthersStartNum <- max(FinalSPKidHousehold$HouseholdID) +1
OthersStartNum <- max(SoleMotherFamilies$HouseholdID) +1


OtherPeopleHouseholdsList <- other(OtherOnlyPeople, "ID", "Age", 3, sdused = 3, OthersStartNum, "HouseholdID",
                                   userseed = TheRandomSeeds[88])                   #################### seed 88 %%%%%%%%%%%%%%%%

OtherPeople <- OtherPeopleHouseholdsList$Matched %>%
  mutate(Type = "Other")





















####################################################################################################
####################################################################################################
# Everyone into the same household data frame
####################################################################################################
####################################################################################################


ThreePersonHouseholdsComplete <- bind_rows(OppSexCouplesWithKids, SameSexF3C, SameSexM3C, SoleFatherFamilies, 
                                           # SoleMotherFamilies, FinalSPKidHousehold, CouplesWithoutKids, OtherPeople)
                                           SoleMotherFamilies, CouplesWithoutKids, OtherPeople)

duplicates <- ThreePersonHouseholdsComplete %>%
  group_by(ID) %>%
  summarise(Count = n()) %>%
  filter(Count > 1)

duplicates <- ThreePersonHouseholdsComplete %>%
  group_by(HouseholdID) %>%
  summarise(Count = n()) %>%
  filter(Count != 3)


































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

File9SoleFathers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleFathAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleFathAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleFathAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1, .12, .14), limits = c(0,.14), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 60), limits = c(10,60)) +
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

#    ggsave(File9SoleFathers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File9SoleFathers.pdf")




# sole mothers

SoleMothers <- SoleMotherFamilies %>%
  filter(Type == "Sole Mother")

# ExtraSoleMum <- FinalSPKidHousehold %>%
#   filter(Type == "Sole Mother")
# 
# SoleMothers <- bind_rows(SoleMothers, ExtraSoleMum)

SoleMotherKids <- SoleMotherFamilies %>%
  filter(Type == "Child")

# ExtraSoleKids <- FinalSPKidHousehold %>%
#   filter(Type == "Child")
# 
# SoleMotherKids <- bind_rows(SoleMotherKids, ExtraSoleKids)


SoleMothAgeDiffs <- left_join(SoleMotherKids, SoleMothers, by = "HouseholdID") %>%
  mutate(AgeDiff = Age.y - Age.x) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())


SoleMothAgeDiffsLong <- SoleMothAgeDiffs  %>%
  tidyr::uncount(weights = Count)

File9SoleMothers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = SoleMothAgeDiffsLong,
           color = "white", fill = "grey47") +
  geom_vline(xintercept = min(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = median(SoleMothAgeDiffsLong$AgeDiff)) +
  geom_vline(xintercept = max(SoleMothAgeDiffsLong$AgeDiff)) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 60), limits = c(10,60)) +
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

#   ggsave(File9SoleMothers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File9SoleMothers.pdf")


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

File9CplsMums <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = CplsMumsAgeDiffsLong,
           color = "grey47", fill = "white") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = 2.21, b=3.66, location=16.5, scale=33.36), 
                color = "#1B9E77", linewidth = 1.5) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, 0.001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10,60)) +
  labs(x = "Age difference (mother age - child age)", y = "Proportion of pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#    ggsave(File9CplsMums, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File9CplsMums.pdf")











####################################################################################################
# Table of household types
####################################################################################################

DataframeOfHouseholdTypes <- data.frame(HouseholdType = c("Female sole parent", "Male sole parent", 
                                                          "Single-sex couple with child", "Opposite-sex couple with child", 
                                                          "Opposite-sex couple with unrelated person", 
                                                          "Household of unrelated people"),
                                        Count = c(nrow(SoleMothers), nrow(SoleFathers), (nrow(SameSexF3C)+nrow(SameSexM3C))/3,
                                                  nrow(CplsMums), nrow(CouplesWithoutKids)/3, nrow(OtherPeople)/3))

DataframeOfHouseholdTypes <- DataframeOfHouseholdTypes %>%
  mutate(Prop = round((Count/sum(Count)),3)) %>%
  arrange(-Count)

# Save the output file

saveRDS(ThreePersonHouseholdsComplete, file = "PhDRData/InterimHouseholdSizeWork/File9ThreePerson/ThreePersonHouseholdsComplete.rds")

# correct the expected counts for the older children

KidsCreated <- ThreePersonHouseholdsComplete %>%
  filter(Type == "Child") %>%
  group_by(Age) %>%
  summarise(Numin3 = n())

# deduct kids 62 and older, these are all in 2-person households

ChildPropsAtHome3 <- ChildPropsAtHome2 %>%
  filter(Age < 44)

saveRDS(ChildPropsAtHome3, file = "PhDRData/ChildPropsAtHome3.rds")

# # clean up environment
# rm(list = ls())