#clear workspace
#  rm(list = ls())

library(dplyr)
library(ggplot2)


File4CorrectHrs <- readRDS("PhDRData/File4CorrectHrs.rds")
ChildPropsAtHome <- readRDS("PhDRData/ChildPropsAtHome.rds")
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# extract out the two usual residents

TwoUsualResidents <- File4CorrectHrs %>%
  filter(UsualResidents == "Two Usual Residents")




####################################################################################################
# graph of ages, relationship status, working hours, all by sex.
####################################################################################################

# ages
AgesProp <- TwoUsualResidents %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))

AllAges2P<- ggplot(AgesProp, aes(x=Age, y=CumAgePercent, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 110), breaks = c(20, 40, 60, 80, 100),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(AllAges2P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/AllAges2P.pdf")


# ages of partnered people only

AgesPropPart <- TwoUsualResidents %>%
  filter(PartnershipStatus == "Partnered") %>%
  group_by(Sex, Age) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Count/sum(Count), CumAgePercent=cumsum(AgePercent))


PartneredAges2P<- ggplot(AgesPropPart, aes(x=Age, y=CumAgePercent, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 110), breaks = c(20, 40, 60, 80, 100),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(PartneredAges2P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/PartneredAges2P.pdf")



# # plot both all and partnered on same graph
# 
# AgesPropExtraVar <- AgesProp %>%
#   mutate(Sex = ifelse(Sex == "Female", "All females", "All males"))
# RelsPropExtraVar <- AgesPropPart %>%
#   mutate(Sex = ifelse(Sex == "Female", "Partnered females", "Partnered males"))
# 
# # combine the two into one df
# 
# PrtPlusAll <- bind_rows(AgesPropExtraVar, RelsPropExtraVar)
# 
# 
# AllandRel2P<- ggplot(PrtPlusAll, aes(x=Age, y=CumAgePercent, colour=Sex)) +
#   geom_line(linewidth = 1) +
#   scale_linetype_manual(values = c("solid", "solid","dashed", "dashed")) +
#   scale_color_manual(values=c("mediumorchid2",'blue', "mediumorchid2",'blue'), name = "",
#                      labels = c("Females", "Males","Females", "Males")) +
#   xlab("Age (years)") + ylab("Cumulative proportion") +
#   scale_x_continuous(limits = c(0, 110), breaks = c(20, 40, 60, 80, 100),
#                      expand = c(0, 2)) +
#   scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 18),
#         legend.position = "bottom")


# working hours

AgesPropHours <- TwoUsualResidents %>%
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
  


WorkingHours2P<- ggplot(AgesPropHours, aes(x=HoursWorkedOrd, y=HoursPercent, fill=Sex)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Hours worked per week") + ylab("Proportion") +
  scale_y_continuous(limits = c(0,0.5), breaks = seq(0, 0.5, by = 0.1), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")


#   ggsave(WorkingHours2P, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/WorkingHours2P.pdf")

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


rm(AgesProp, AgesPropHours, AgesPropPart, AllAges2P, ExtraPerson, LongerProps, PartneredAges2P, WorkingHours2P, 
   File4SchoolIndAdded,PropWorking)







####################################################################################################
####################################################################################################
# create the couples
####################################################################################################
####################################################################################################

# need same-sex first
# need to create couples now as want to remove couples from potential sole parents

PartneredMen <- TwoUsualResidents %>%
  filter(Sex == "Male", PartnershipStatus == "Partnered")

PartneredWomen <- TwoUsualResidents %>%
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

FSameSexProb <- round(((TABLECODE8160[4,3]/TABLECODE8160[2,3])*2),3)
MSameSexProb <- round(((TABLECODE8160[3,3]/TABLECODE8160[2,3])*2),3)



# get starting household ID

File7OnePersonHH <- readRDS("PhDRData/InterimHouseholdSizeWork/File7OnePerson/File7OnePersonHH.rds")

StartHHID <- max(File7OnePersonHH$HouseholdID) + 1

# females

SameSexF2 <- fastmatch(PartneredWomen, "Age", FSameSexProb, AgeUpweight, 25,54, StartHHID,
                               "HouseholdID", userseed=TheRandomSeeds[61])  #################### seed 61 %%%%%%%%%%%%%%%%

SameSexF2 <- SameSexF2 %>%
  mutate(Type = "Single sex no kids")


StartHHID <- max(SameSexF2$HouseholdID) + 1

SameSexM2 <- fastmatch(PartneredMen, "Age", MSameSexProb, AgeUpweight, 25,54, StartHHID,
                       "HouseholdID", userseed=TheRandomSeeds[62])         #################### seed 62 %%%%%%%%%%%%%%%%

SameSexM2 <- SameSexM2 %>%
  mutate(Type = "Single sex no kids")


rm(TABLECODE8160, TABLECODE8161, File7OnePersonHH, AgeUpweight, FSameSexProb, MSameSexProb,
   StartHHID, fastmatch)







#######################################################################################
# generate opposite sex couples
#######################################################################################

# remaining men
PartneredMenLeft <- PartneredMen %>%
  filter(!ID %in% SameSexM2$ID)

#remaining women
PartneredWomenLeft <- PartneredWomen %>%
  filter(!ID %in% SameSexF2$ID)

StartID <- max(SameSexM2$HouseholdID) + 1


OppSex2P <- pairnorm(PartneredMenLeft, "ID", "Age", PartneredWomenLeft, "ID", "Age", directxi = 2, directomega = 3,
                     alphaused = 0, StartID, "HouseholdID", 
                     userseed = TheRandomSeeds[63])                         #################### seed 63 %%%%%%%%%%%%%%%%

OppositeSexCouples <- OppSex2P$Matched
UnmatchedPartnered <- OppSex2P$Unmatched

mean(PartneredMenLeft$Age)
mean(PartneredWomenLeft$Age)

OppositeSexCouples <- OppositeSexCouples %>%
  mutate(Type = "Opposite sex no kids")

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

File8Partnered <- ggplot() +
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


#   ggsave(File8Partnered, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/File8Partnered.pdf")


rm(men, mergedsexes, OppositeSexAgeDiffs, OppositeSexAgeDiffsLong, OppSex2P, PartneredMen, PartneredWomen, PartneredMenLeft,
   PartneredWomenLeft, women, StartID, File8Partnered)


























#######################################################################################
#######################################################################################
# create the child households
#######################################################################################
#######################################################################################

####################################################################################################
# get the kids counts
####################################################################################################

PeopleByAge <- TwoUsualResidents %>%
  group_by(Age) %>%
  summarise(Num = n())

# apply probs

NumKids2PH <- left_join(PeopleByAge, ChildPropsAtHome, by = "Age") %>%
  filter(!(is.na(Fits))) %>%
  mutate(NumNeeded = ceiling(Num*Fits))

# sample to get the kids

# use diff sample

UnPartneredResidents <- TwoUsualResidents %>%
  filter(PartnershipStatus == "Not Partnered")

The15161718InSchool <- TwoUsualResidents %>%
  filter(EducationStatus == "Y", Age %in% c(15, 16, 17, 18))

NumKidsByAge15161718 <- The15161718InSchool %>%
  group_by(Age) %>%
  summarise(NumInAge = n())

ExtrasNeeded15161718 <- left_join(NumKidsByAge15161718, NumKids2PH, by = "Age") %>%
  mutate(StilltoSample = NumNeeded - NumInAge)

UnpartneredLeft <- UnPartneredResidents %>%
  filter(!ID %in% The15161718InSchool$ID)

UpdatedSampleCounts <- left_join(NumKids2PH, ExtrasNeeded15161718, 
                                 by = c("Age", "Num", "Fits", "ExpectedNum", "NumNeeded")) %>%
  select(-NumInAge) %>%
  mutate(SampleSize = ifelse(is.na(StilltoSample), NumNeeded, StilltoSample),
         SampleSize = ifelse(SampleSize > 0, SampleSize, 0)) %>%
  select(Age, SampleSize)


TheKids <- diffsample(UnpartneredLeft, "Age", UpdatedSampleCounts, "Age", "SampleSize", 
                      userseed=TheRandomSeeds[64])                         #################### seed 64 %%%%%%%%%%%%%%%%

TheKids <- bind_rows(TheKids, The15161718InSchool)

# test counts
table(UnPartneredResidents$Age)

table(TheKids$Age)

# check counts
isTRUE(sum(NumKids2PH$NumNeeded) == nrow(TheKids))
sum(NumKids2PH$NumNeeded)
nrow(TheKids)








####################################################################################################
# sole parent households
####################################################################################################
# all into sole parent households

####################################################################################################
# Dads

SoleFathNum <- round((nrow(TheKids) * 0.138),0)

# get potential sole fathers

PotSoleFathers <- TwoUsualResidents %>%
  filter(!ID %in% c(SameSexM2$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Male")

# sample the kids
set.seed(TheRandomSeeds[65])                                                    #################### seed 65 %%%%%%%%%%%%%%%%
FathKids <- TheKids %>%
  slice_sample(n=SoleFathNum, replace = FALSE)


# add kids to sole fathers

StartID <- max(OppositeSexCouples$HouseholdID) + 1
# 
# SoleFathHouseholds <- pair4Pbeta(FathKids, "ID", "Age", PotSoleFathers, "ID", "Age", shapeA=3.72, shapeB=6.07, 
#                                  locationP=13.93, scaleP=47.08, StartID, "HouseholdID", 
#                                  userseed = TheRandomSeeds[66])                    #################### seed 66 %%%%%%%%%%%%%%%%

############ JUST USED THE 66TH RANDOM SEED ################

# did not produce the required distribution, re-run with the advanced version

SoleFathHouseholds <- pair4PbetaAdv(FathKids, "ID", "Age", PotSoleFathers, "ID", "Age", shapeA=3.72, shapeB=6.07, 
                                 locationP=13.93, scaleP=47.08, StartID, "HouseholdID", 
                                 userseed = TheRandomSeeds[67])                    #################### seed 67 %%%%%%%%%%%%%%%%

SoleFatherFamilies <- SoleFathHouseholds$Matched

numFathHH <- as.numeric(nrow(SoleFatherFamilies)/2) + 1

SoleFatherFamilies <- SoleFatherFamilies %>%
  mutate(Type = ifelse(row_number() < numFathHH, "Child", "Sole Father"))

table(SoleFatherFamilies$Type)



FatherAgeDiffs <- SoleFatherFamilies %>%
  arrange(HouseholdID, Age) %>%
  group_by(HouseholdID) %>%
  mutate(AgeDiff = Age - lag(Age)) %>%
  filter(!(is.na(AgeDiff))) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())

FatherAgeDiffsLong <- FatherAgeDiffs  %>%
  tidyr::uncount(weights = Count)
min(FatherAgeDiffsLong$AgeDiff)
median(FatherAgeDiffsLong$AgeDiff)
max(FatherAgeDiffsLong$AgeDiff)


File8Fathers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = FatherAgeDiffsLong,
           color = "grey47", fill = "white") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a =3.72, b=6.07, location=13.93, scale=47.08), 
                color = "#1B9E77", linewidth = 1.5) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 50), limits = c(10,50)) +
  labs(x = "Age difference (father age - child age)", y = "Proportion of pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#   ggsave(File8Fathers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File8Fathers.pdf")


# add kids to sole mothers

rm(StartID)

MothKids <- TheKids %>%
  filter(!ID %in% FathKids$ID)

PotSoleMothers <- TwoUsualResidents %>%
  filter(!ID %in% c(SameSexF2$ID, OppositeSexCouples$ID, TheKids$ID),
         Sex == "Female")

StartID <- max(SoleFatherFamilies$HouseholdID) + 1
# 
# SoleMothHouseholds <- pair4Pbeta(MothKids, "ID", "Age", PotSoleMothers, "ID", "Age", shapeA=2.21, shapeB=3.66, 
#                                  locationP=16.5, scaleP=33.36, StartID, "HouseholdID", 
#                                  userseed = TheRandomSeeds[68])                    #################### seed 68 %%%%%%%%%%%%%%%%


# did not produce the required distribution, re-run with the advanced version

SoleMothHouseholds <- pair4PbetaAdv(MothKids, "ID", "Age", PotSoleMothers, "ID", "Age", shapeA=2.21, shapeB=3.66, 
                                 locationP=16.5, scaleP=33.36, StartID, "HouseholdID", 
                                 userseed = TheRandomSeeds[69])                   #################### seed 69 %%%%%%%%%%%%%%%%
                                

# fails to converge using 10, 15, and 20 attempts values

# keep the 10-attempts one, slightly better chi square value

# plot

SoleMotherFamilies <- SoleMothHouseholds$Matched

numMothHH <- as.numeric(nrow(SoleMotherFamilies)/2) + 1

SoleMotherFamilies <- SoleMotherFamilies %>%
  mutate(Type = ifelse(row_number()< numMothHH, "Child", "Sole Mother"))

table(SoleMotherFamilies$Type)


MotherAgeDiffs <- SoleMotherFamilies %>%
  arrange(HouseholdID, Age) %>%
  group_by(HouseholdID) %>%
  mutate(AgeDiff = Age - lag(Age)) %>%
  filter(!(is.na(AgeDiff))) %>%
  group_by(AgeDiff) %>%
  summarise(Count = n())

MotherAgeDiffsLong <- MotherAgeDiffs  %>%
  tidyr::uncount(weights = Count)
min(MotherAgeDiffsLong$AgeDiff)
median(MotherAgeDiffsLong$AgeDiff)
max(MotherAgeDiffsLong$AgeDiff)


File8Mothers <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = MotherAgeDiffsLong,
           color = "grey47", fill = "white") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = 2.21, b=3.66, location=16.5, scale=33.36), 
                color = "#1B9E77", linewidth = 1.5) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10,60)) +
  labs(x = "Age difference (mother age - child age)", y = "Proportion of pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#   ggsave(File8Mothers, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/File8Mothers.pdf")


rm(FatherAgeDiffs, FatherAgeDiffsLong, FathKids, File8Fathers, File8Mothers, MotherAgeDiffs, MotherAgeDiffsLong, MothKids,
   PotSoleFathers, PotSoleMothers, SoleFathHouseholds, SoleMothHouseholds, SoleFathNum, StartID, diffsample, pair4Pbeta,
   pair4PbetaAdv, pairnorm, numFathHH, numMothHH, NumWomenOlder, AchievedKidsCounts, ExtrasNeeded15161718, NumKids2PH,
   NumKidsByAge15161718, PeopleByAge, The15161718InSchool, TheKids, UnmatchedPartnered, UnpartneredLeft,
   UnPartneredResidents, UpdatedSampleCounts)


#######################################################################################
#######################################################################################
# create the other households
#######################################################################################
#######################################################################################

RemainingPeople <- TwoUsualResidents %>%
  filter(!ID %in% c(SameSexF2$ID, SameSexM2$ID, OppositeSexCouples$ID, SoleFatherFamilies$ID, SoleMotherFamilies$ID))


OthersStartNum <- max(SoleMotherFamilies$HouseholdID) + 1

OtherHouseholds <- other(RemainingPeople, "ID", "Age", numppl = 2, sdused = 3, OthersStartNum, "HouseholdID", 
                         userseed = TheRandomSeeds[70])                   #################### seed 70 %%%%%%%%%%%%%%%%

OtherHouseholdsFinished <- OtherHouseholds$Matched %>%
  mutate(Type = "Other")


TwoPersonHouseholdsComplete <- bind_rows(OppositeSexCouples, OtherHouseholdsFinished, SameSexF2, SameSexM2,
                                         SoleFatherFamilies, SoleMotherFamilies)

####################################################################################################
# Table of household types
####################################################################################################

DataframeOfHouseholdTypes <- data.frame(HouseholdType = c("Female sole parent", "Male sole parent", 
                                                          "Opposite-sex couple without children", 
                                                          "Same-sex couple without children",
                                                          "Household of unrelated people"),
                                        Count = c(nrow(SoleMotherFamilies)/2, nrow(SoleFatherFamilies)/2,
                                                  nrow(OppositeSexCouples)/2, (nrow(SameSexF2)+nrow(SameSexM2))/2,
                                                  nrow(OtherHouseholdsFinished)/2))

DataframeOfHouseholdTypes <- DataframeOfHouseholdTypes %>%
  mutate(Prop = round((Count/sum(Count)),3)) %>%
  arrange(-Count)



saveRDS(TwoPersonHouseholdsComplete, file = "PhDRData/InterimHouseholdSizeWork/File8TwoPerson/TwoPersonHouseholdsComplete.rds")

# correct the expected counts for the older children

KidsCreated <- TwoPersonHouseholdsComplete %>%
  filter(Type == "Child") %>%
  group_by(Age) %>%
  summarise(Numin2 = n())

# deduct kids 62 and older, these are all in 2-person households

ChildPropsAtHome2 <- ChildPropsAtHome %>%
  filter(Age < 62)

saveRDS(ChildPropsAtHome2, file = "PhDRData/ChildPropsAtHome2.rds")

# # clean up environment
# rm(list = ls())


#######################################################################################
#######################################################################################
# STATS ON THE NUMBER AND PROP OF HOUSEHOLDS ARE IN THE ENDHOUSEHOLDCOMP FILE
#######################################################################################
#######################################################################################
