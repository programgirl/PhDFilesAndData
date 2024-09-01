#clear workspace
#  rm(list = ls())

library(dplyr)
library(readxl)
library(ggplot2)
library(conflicted)
library(forcats)
library(RColorBrewer)
library(ggridges)

conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")
# library(forcats)
# library(ggplot2)



JOB_11634_CONZUL_ <- read_excel("Stats NZ downloaded files/Population/JOB-11634 CONZUL .xlsx", 
                                sheet = "Table 1", range = "A9:G8434", col_names = TRUE, na = "..C")

# remove blank and text-only note rows
JOB_11634_CONZUL_ <- JOB_11634_CONZUL_ %>%
  filter(!(is.na(`Age Group`)))



# rename variables with spaces in their names and create ordered factors
JOB_11634_CONZUL <- JOB_11634_CONZUL_ %>%
  rename(AgeGroup = `Age Group`, 
         PartnershipStatus = `Partnership Status (total incl not in subject population (1))`, 
         UsualResidents = `Usual Resident in Household`, 
         HoursWorked = `Hours Worked in Employment Per Week (total incl not in subject population (1))`,
         Count = `People count (UA 113 Timaru, 2013 areas)`,
         TotalNZ = `People count (total NZ)`) %>%
  select(-TotalNZ) %>%
  mutate(Sex = fct_relevel(Sex, c("Female", "Male", "Total")),
         AgeGroup = fct_relevel(AgeGroup, c("0-4 Years", "5 -9 Years", "10 - 14 Years", "15 - 17 Years",
                                            "18 - 24 Years", "25 - 34 Years", "35 - 44 Years", 
                                            "45 - 54 Years", "55 - 64 Years", "65 - 74 Years", 
                                            "75 - 84 Years", "85 Years and Over", "Total")),
         AgeGroup = recode(AgeGroup, "0-4 Years" = "0 - 4 Years", "5 -9 Years" = "5 - 9 Years"),
         PartnershipStatus = fct_relevel(PartnershipStatus, c("Non-partnered  and not elsewhere included (incl not in subject population)",
                                                              "Partnered", "Total")),
         UsualResidents = fct_relevel(UsualResidents, c("One Usual Resident", "Two Usual Residents",
                                                        "Three Usual Residents", "Four Usual Residents",
                                                        "Five Usual Residents", "Six Usual Residents",
                                                        "Seven Usual Residents",
                                                        "Eight or More Usual Residents", "Total")),
         HoursWorked = fct_relevel(HoursWorked, c("Not working and not elsewhere included (incl not in subject population)", 
                                                  "1-9 Hours Worked", "10-19 Hours Worked",
                                                  "20-29 Hours Worked", "30-39 Hours Worked",
                                                  "40-49 Hours Worked", "50 Hours or More Worked",
                                                  "Total"))) %>%
  mutate(PartnershipStatus = fct_recode(PartnershipStatus, 
                                        "Not Partnered" = "Non-partnered  and not elsewhere included (incl not in subject population)"),
         HoursWorked = fct_recode(HoursWorked, 
                                  "No Hours" = "Not working and not elsewhere included (incl not in subject population)"))



####################################################################################################
####################################################################################################
# Stats for missing cells, using eight and six person households and the age group 25-34 years
####################################################################################################
####################################################################################################

# 0-4 year old boy in eight person households
temp048B <- JOB_11634_CONZUL %>%
  filter(Sex == "Male", AgeGroup == "0 - 4 Years", UsualResidents == "Eight or More Usual Residents")
# 15-17 boys
temp15178B <- JOB_11634_CONZUL %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years", UsualResidents == "Eight or More Usual Residents")


# 0-4 year old girl in eight person households
temp048G <- JOB_11634_CONZUL %>%
  filter(Sex == "Female", AgeGroup == "0 - 4 Years", UsualResidents == "Eight or More Usual Residents")
# 15-17 girls
temp15178G <- JOB_11634_CONZUL %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years", UsualResidents == "Eight or More Usual Residents")


# 15-17 in six person households
# boys
temp1517B6 <- JOB_11634_CONZUL %>%
  filter(Sex == "Male", AgeGroup == "15 - 17 Years", UsualResidents == "Six Usual Residents")
# girls
temp1517G6 <- JOB_11634_CONZUL %>%
  filter(Sex == "Female", AgeGroup == "15 - 17 Years", UsualResidents == "Six Usual Residents")




# 25-34 year old women in eight person households
temp <- JOB_11634_CONZUL %>%
  filter(Sex == "Female")
temp <- temp %>%
  filter(AgeGroup == "25 - 34 Years")
temp <- temp %>%
  filter(UsualResidents == "Eight or More Usual Residents")

# in six-person households
temp2 <- JOB_11634_CONZUL %>%
  filter(Sex == "Female")
temp2 <- temp2 %>%
  filter(AgeGroup == "25 - 34 Years")
temp2 <- temp2 %>%
  filter(UsualResidents == "Six Usual Residents")


# 25-34 year old men in eight person households
temp3 <- JOB_11634_CONZUL %>%
  filter(Sex == "Male")
temp3 <- temp3 %>%
  filter(AgeGroup == "25 - 34 Years")
temp3 <- temp3 %>%
  filter(UsualResidents == "Eight or More Usual Residents")

# in six-person households
temp4 <- JOB_11634_CONZUL %>%
  filter(Sex == "Male")
temp4 <- temp4 %>%
  filter(AgeGroup == "25 - 34 Years")
temp4 <- temp4 %>%
  filter(UsualResidents == "Six Usual Residents")





####################################################################################################
####################################################################################################
# Stats for missing cells, seven person households and the age group 25-34 years
####################################################################################################
####################################################################################################


# 25-34 year old women in seven person households
temp2534SevenF <- JOB_11634_CONZUL %>%
  filter(Sex == "Female", AgeGroup == "25 - 34 Years",UsualResidents == "Seven Usual Residents")


# 25-34 year old men in seven person households
temp2534SevenM <- JOB_11634_CONZUL %>%
  filter(Sex == "Male", AgeGroup == "25 - 34 Years",UsualResidents == "Seven Usual Residents")


####################################################################################################
####################################################################################################
# Comparison for discussion
####################################################################################################
####################################################################################################


















####################################################################################################
####################################################################################################
# Final household stats for dissertation
####################################################################################################
####################################################################################################

# bring in the population and do stats

File16SyntheticPopulation <- readRDS("PhDRData/File16SyntheticPopulation.rds")

####################################################################################################
# How many households?
####################################################################################################

NumHouseholds <- File16SyntheticPopulation %>%
  group_by(HouseholdID) %>%
  summarise(NumInHousehold = n())

# get the household types

OthersWithCouples <- File16SyntheticPopulation %>%
  select(ID, HouseholdID, Type, UsualResidents) %>%
  filter(Type == "Other with couples")

OthersWithCouplesHHCounts <- OthersWithCouples %>%
  select(HouseholdID, UsualResidents) %>%
  distinct()
  
OnePerson <- File16SyntheticPopulation %>%
  select(ID, HouseholdID, Type, UsualResidents) %>%
  filter(Type == "Alone") 

CouplesOnly <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type %in% c("Opposite sex without child", "Same sex without child"), UsualResidents == "Two Usual Residents") %>%
  distinct()

CouplesWithKids <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type %in% c("Opposite sex with child", "Same sex with child")) %>%
  distinct()

SoleMothers <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type == "Sole Mother") %>%
  distinct()

SoleFathers <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type == "Sole Father") %>%
  distinct()

Unrelated <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type == "Other") %>%
  distinct()

# check num households

NumHouseholds <- nrow(OthersWithCouplesHHCounts) + nrow(OnePerson) + nrow(CouplesOnly) + nrow(CouplesWithKids) + 
  nrow(SoleMothers) +  nrow(SoleFathers) + nrow(Unrelated)

# couple only
round(nrow(CouplesOnly)/NumHouseholds,4)

# one person
round(nrow(OnePerson)/NumHouseholds,4)

# couple with children
round(nrow(CouplesWithKids)/NumHouseholds,4)

# sole parents
round((nrow(SoleMothers) +  nrow(SoleFathers)) / NumHouseholds,4)

# unrelated
round(nrow(Unrelated)/NumHouseholds,4)

# couple and other persons
round(nrow(OthersWithCouplesHHCounts)/NumHouseholds, 4)






####################################################################################################
# How many families?
####################################################################################################

NumFamilies <- nrow(CouplesOnly) + nrow(CouplesWithKids) + nrow(SoleMothers) +  nrow(SoleFathers)

OppSexWithKids <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type =="Opposite sex with child") %>%
  distinct()

SameSexWithKidsF <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents, Sex) %>%
  filter(Type == "Same sex with child", Sex == "Female") %>%
  distinct()

SameSexWithKidsM <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents, Sex) %>%
  filter(Type == "Same sex with child", Sex == "Male") %>%
  distinct()

OppSexCouples <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents) %>%
  filter(Type %in% c("Opposite sex without child"), UsualResidents == "Two Usual Residents") %>%
  distinct()

SameSexCouplesF <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents, Sex) %>%
  filter(Type %in% c("Same sex without child"), UsualResidents == "Two Usual Residents", Sex == "Female") %>%
  distinct()

SameSexCouplesM <- File16SyntheticPopulation %>%
  select(HouseholdID, Type, UsualResidents, Sex) %>%
  filter(Type %in% c("Same sex without child"), UsualResidents == "Two Usual Residents", Sex == "Male") %>%
  distinct()

# opp sex without kids
round(nrow(OppSexCouples)/NumFamilies,4)

# opp sex with kids
round(nrow(OppSexWithKids)/NumFamilies,4)

# one parent families
round((nrow(SoleMothers) +  nrow(SoleFathers)) / NumFamilies,4)

# female couple no kids
round(nrow(SameSexCouplesF)/NumFamilies,4)

# male couple no kids
round(nrow(SameSexCouplesM)/NumFamilies,4)

# female couple with children
round(nrow(SameSexWithKidsF)/NumFamilies,4)

# male couple with children
round(nrow(SameSexWithKidsM)/NumFamilies,4)

# total couple without kids
round((nrow(OppSexCouples)+nrow(SameSexCouplesF)+nrow(SameSexCouplesM))/NumFamilies,4)

# total couple iwth kids
round((nrow(OppSexWithKids)+nrow(SameSexWithKidsF)+nrow(SameSexWithKidsM))/NumFamilies,4)



# prop male versus female sole parents

NumSoleParents <- nrow(SoleMothers) +  nrow(SoleFathers)

# prop female
round(nrow(SoleMothers)/NumSoleParents,3)

# prop male
round(nrow(SoleFathers)/NumSoleParents,3)










####################################################################################################
####################################################################################################
# Opposite sex couple age differences
####################################################################################################
####################################################################################################


AllCouples <- File16SyntheticPopulation %>%
  select(ID, Sex, Age, HouseholdID, Type, UsualResidents) %>%
  filter(Type %in% c("Opposite sex without child","Opposite sex with child"))


table(AllCouples$UsualResidents) # when divided by two, gives number of couples

# calculate the age differences

# plot
OppositeSexAgeDiffs <- AllCouples %>%
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

# test for normality

# Pearsons
PearsonsMLAge <- PearsonDS::pearsonFitML(OppositeSexAgeDiffsLong$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMSCAge <- PearsonDS::pearsonMSC(OppositeSexAgeDiffsLong$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMSCAge

PearsonsMSCAge$Best$AIC  # best model
PearsonsMSCAge$Best$AICc
PearsonsMSCAge$Best$BIC
PearsonsMSCAge$Best$HQC


File21AllCouples <- ggplot() +
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


#   ggsave(File21AllCouples, width=9.32, height=7.78, units="in", file="File21AllCouples.pdf")


# get percentage of difference









####################################################################################################
####################################################################################################
# Children
####################################################################################################
####################################################################################################

Kids <- File16SyntheticPopulation %>%
  filter(Type == "Child")

# what prop?

round(nrow(Kids)/nrow(File16SyntheticPopulation),3)

median(Kids$Age)
max(Kids$Age)


# get prop children by age

AgesInSPProps <- File16SyntheticPopulation %>%
  select(Age) %>%
  group_by(Age) %>%
  summarise(NumAgeSP = n())

KidsPropAgeSP <- Kids %>%
  select(Age) %>%
  group_by(Age) %>%
  summarise(NumKidsSP = n())

SPKidsData <- left_join(KidsPropAgeSP, AgesInSPProps, by = "Age") %>%
  mutate(Fits = NumKidsSP/NumAgeSP,
         Group = "Achieved") %>%
  select(-c(NumKidsSP,NumAgeSP))



TimaruDistrictAgeTotals8001 <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex groups/TABLECODE8001_Data_b339765f-cf78-4e38-aa57-332c2e066b02.csv")

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


# mothers data read in
Mothersraw <- read_excel("~/Sync/PhD/Stats NZ downloaded files/Children/JOB-09626 CONZUL rerun.xlsx",
                         sheet = "Table 1 rerun", range = "a9:q70", na = "..C")

MotherChildTD <- Mothersraw %>%
  filter(Area == "Timaru District", !(`Age of Child` %in% c("Total", "0-4 Years", "5-9 Years"))) %>%
  dplyr::select(`Age of Child`, `Total 2`) %>%
  rename(ChildAge = `Age of Child`, NumberChildren = `Total 2`) %>%
  mutate(ChildAge = ifelse(ChildAge == "15 - 17 Years", "15-17 Years",
                           ifelse(ChildAge == "18 -24 Years", "18-24 Years", ChildAge)))

ChildCountsTD <-  left_join(MotherChildTD, TimaruDistrictAgeTotalsCleaned, by = "ChildAge") %>%
  filter(!(ChildAge %in% c("0-4 Years", "5-9 Years"))) %>%
  mutate(Bottom = c(14, 15, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65),
         Middle = c(14, 16, 21, 27, 32, 37, 42, 47, 52, 57, 62, 74.5),
         Top = c(14, 17, 24, 29, 34, 39, 44, 49, 54, 59, 64, 84),
         PropAtHome =ifelse(ChildAge == "10-14 Years", 1, NumberChildren/Value),
         FakeGroup = c(rep("FakeGroup", nrow(.))))

# get counts for dissertation

CountsForDist <- ChildCountsTD %>%
  select(c(Middle, PropAtHome)) %>%
  mutate(PropAtHome = round(PropAtHome, 3))

CountsForDist

# add in the zero value for age 85 years

TimaruDistrict85Plus <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("85-89 years")) %>%
  dplyr::select(-Age.group) %>%
  mutate(ChildAge = "No children",
         NumberChildren = 0,
         Value = 0,
         Bottom = 85,
         Middle = 85,
         Top = 85,
         PropAtHome = 0,
         FakeGroup = "FakeGroup")

ChildCountsTD <- bind_rows(ChildCountsTD,TimaruDistrict85Plus)

TheGroup <- "FakeGroup"

CalcProps <- interdiff(ChildCountsTD, "Middle", "PropAtHome", "Bottom", "Top", TheGroup)

MissingAges <- data.frame(Age = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                          Fits = c(rep(1,14)),
                          FakeGroup = c(rep("FakeGroup",14)))

EstimatedProps <- bind_rows(MissingAges, CalcProps) %>%
  select(-FakeGroup) %>%
  filter(Age < 73) %>%
  mutate(Group = "Imputed")

CombinedChildProps <- bind_rows(SPKidsData,EstimatedProps)

# plot

ChildPropsExpAch <- ggplot(CombinedChildProps, aes(x = Age, y = Fits, color = Group)) +
  geom_point(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1), limits = c(0,1), expand = c(0, 0.01)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80), limits = c(0, 80)) +
  labs(x = "Age (years)", y = "Proportion who are children living at home") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(ChildPropsExpAch, file="ChildPropsExpAch.pdf", width=10, height=6, units="in")



PercentDiffKids <- left_join(SPKidsData,EstimatedProps, by = "Age") %>%
  mutate(RelativeError = (Fits.y - Fits.x)/Fits.y,
         PercentageError = abs(RelativeError) * 100)


File21PercentChangeChildProps <- ggplot(PercentDiffKids, aes(x = Age, y = PercentageError)) +
  geom_point(size = 1) +
  scale_y_continuous(breaks = c(0, 20, 30, 40, 60, 80), limits = c(0,80), expand = c(.02, .02)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80), limits = c(0, 80)) +
  labs(x = "Age (years)", y = "Percentage error") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(File21PercentChangeChildProps, file="File21PercentChangeChildProps.pdf", width=10, height=6, units="in")



####################################################################################################
# Twins
####################################################################################################

# maximum number of twins expected
0.029* nrow(Kids)

# get the twins
TwinsHouseholds <- Kids %>%
  group_by(HouseholdID, Age, UsualResidents) %>%
  summarise(NumPerAgePerHH = n()) %>%
  filter(NumPerAgePerHH > 1)

# num twins
2*nrow(TwinsHouseholds)

table(TwinsHouseholds$UsualResidents)


rm(list = ls())




































####################################################################################################
####################################################################################################
# Achieved age at childbirth distributions
# only use one-child families
# sole parents one child - two person families, and mother - three person families
# multi-child uses a different approach, not distributional but range-based.
####################################################################################################
####################################################################################################

####################################################################################################
# father-child, only uses two-person households
####################################################################################################

# father data, two person households, only plot to create

TwoPSoleFHH <- File16SyntheticPopulation %>%
  filter(UsualResidents == "Two Usual Residents", Type == "Sole Father") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(DadAge = Age,
         DadSex = Sex,
         DadID = ID)

TwoPSoleFKids <- File16SyntheticPopulation %>%
  filter(HouseholdID %in% TwoPSoleFHH$HouseholdID, Type == "Child") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(ChildAge = Age,
         ChildSex = Sex,
         ChildID = ID)

TwoPFatherDiffs <- left_join(TwoPSoleFHH, TwoPSoleFKids, by = "HouseholdID") %>%
  mutate(AgeDiff = DadAge - ChildAge)

# Pearsons
PearsonsFathMS <- PearsonDS::pearsonFitML(TwoPFatherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsFathMSC <- PearsonDS::pearsonMSC(TwoPFatherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsFathMSC

PearsonsFathMSC$Best$AIC  # best model
round(PearsonsFathMSC$MSCs[2,2],0) # AIC for best model
round(PearsonsFathMSC$MSCs[4,2],0) # BIC for best model


File21SoleFatherDiffs <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = TwoPFatherDiffs,
           color = "grey47", fill = "white") +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, .001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50), limits = c(10,50), expand = c(0, 0.2)) +
  labs(x = "Age difference between father and child", y = "Proportion of fathers") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = 3.72, b=6.07, location=13.93, scale=47.08),
                aes(color = "original (4-parameter beta)"), linewidth = 2) +
                    stat_function(fun = PearsonDS::dpearsonI, args = list(a = PearsonsFathMSC$Best$ML[["a"]],
                                                                        b = PearsonsFathMSC$Best$ML[["b"]],
                                                                        location = PearsonsFathMSC$Best$ML[["location"]],
                                                                        scale =  PearsonsFathMSC$Best$ML[["scale"]]),
                                  aes(color = "achieved (symmetric beta)"), linewidth = 2) +
  scale_colour_manual("Distribution", values = c("original (4-parameter beta)" = "#1B9E77",
                                                 "achieved (symmetric beta)" = "#D95F02")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(File21SoleFatherDiffs, file="File21SoleFatherDiffs.pdf", width=10, height=6, units="in")


rm(list = ls())













####################################################################################################
# sole mothers, two person households
####################################################################################################

# father data, two person households, only plot to create

TwoPSoleMHH <- File16SyntheticPopulation %>%
  filter(UsualResidents == "Two Usual Residents", Type == "Sole Mother") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(MumAge = Age,
         MumSex = Sex,
         MumID = ID)

TwoPSoleMKids <- File16SyntheticPopulation %>%
  filter(HouseholdID %in% TwoPSoleMHH$HouseholdID, Type == "Child") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(ChildAge = Age,
         ChildSex = Sex,
         ChildID = ID)

TwoPMotherDiffs <- left_join(TwoPSoleMHH, TwoPSoleMKids, by = "HouseholdID") %>%
  mutate(AgeDiff = MumAge - ChildAge)

# Pearsons
PearsonsMoth2MS <- PearsonDS::pearsonFitML(TwoPMotherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMoth2MSC <- PearsonDS::pearsonMSC(TwoPMotherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMoth2MSC


File21SoleMotherDiffs <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = TwoPMotherDiffs,
           color = "grey47", fill = "white") +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, .001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50), limits = c(10,50), expand = c(0, 0.2)) +
  labs(x = "Age difference between mother and child", y = "Proportion of mothers") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = 2.21, b=3.66, location=16.5, scale=33.36),
                aes(color = "original (4-parameter beta)"), linewidth = 2) +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = PearsonsMoth2MSC$Best$ML[["a"]],
                                                        b = PearsonsMoth2MSC$Best$ML[["b"]],
                                                        location = PearsonsMoth2MSC$Best$ML[["location"]],
                                                        scale =  PearsonsMoth2MSC$Best$ML[["scale"]]),
                aes(color = "achieved (4-parameter beta)"), linewidth = 2) +
  scale_colour_manual("Distribution", values = c("original (4-parameter beta)" = "#1B9E77",
                                                 "achieved (4-parameter beta)" = "#D95F02")) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(File21SoleMotherDiffs, file="File21SoleMotherDiffs.pdf", width=10, height=6, units="in")


rm(list = ls())






####################################################################################################
# Mothers, couples, three person households
####################################################################################################

# father data, two person households, only plot to create

ThreePMothers <- File16SyntheticPopulation %>%
  filter(UsualResidents == "Three Usual Residents", Type == "Opposite sex with child", Sex == "Female") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(MumAge = Age,
         MumSex = Sex,
         MumID = ID)

ThreePersonKids <- File16SyntheticPopulation %>%
  filter(HouseholdID %in% ThreePMothers$HouseholdID, Type == "Child") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  rename(ChildAge = Age,
         ChildSex = Sex,
         ChildID = ID)

ThreePMotherDiffs <- left_join(ThreePMothers, ThreePersonKids, by = "HouseholdID") %>%
  mutate(AgeDiff = MumAge - ChildAge)

# Pearsons
PearsonsMoth3MS <- PearsonDS::pearsonFitML(ThreePMotherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMoth3MSC <- PearsonDS::pearsonMSC(ThreePMotherDiffs$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMoth3MSC


File21MotherCplsOneKidDiffs <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = ThreePMotherDiffs,
           color = "grey47", fill = "white") +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08), limits = c(0,.08), expand = c(0, .001)) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50), limits = c(10,50), expand = c(0, 0.2)) +
  labs(x = "Age difference between mother and child", y = "Proportion of mothers") +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = 2.21, b=3.66, location=16.5, scale=33.36),
                aes(color = "original (4-parameter beta)"), linewidth = 2) +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = PearsonsMoth3MSC$Best$ML[["a"]],
                                                        b = PearsonsMoth3MSC$Best$ML[["b"]],
                                                        location = PearsonsMoth3MSC$Best$ML[["location"]],
                                                        scale =  PearsonsMoth3MSC$Best$ML[["scale"]]),
                aes(color = "achieved (4-parameter beta)"), linewidth = 2) +
  stat_function(fun = PearsonDS::dpearsonII, args = list(a = PearsonsMoth3MSC$Best$BIC[["a"]],
                                                        location = PearsonsMoth3MSC$Best$BIC[["location"]],
                                                        scale =  PearsonsMoth3MSC$Best$BIC[["scale"]]),
                aes(color = "achieved (symmetric beta)"), linewidth = 2) +
  scale_colour_manual("Distribution", values = c("original (4-parameter beta)" = "#1B9E77",
                                                 "achieved (4-parameter beta)" = "#D95F02",
                                                 "achieved (symmetric beta)"   ="#7570b3"))  +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(File21MotherCplsOneKidDiffs, file="File21MotherCplsOneKidDiffs.pdf", width=10, height=6, units="in")


rm(list = ls())











####################################################################################################
# Demonstrate age structure constraints using two-person households
####################################################################################################

# limit to the case of one child families as the sole parents didn't use a distribution to match
# they used an age range based on the distribution

File16SyntheticPopulation <- readRDS("PhDRData/File16SyntheticPopulation.rds")

ThreePersonMothersCpls <- File16SyntheticPopulation %>%
  filter(UsualResidents == "Three Usual Residents", Type == "Opposite sex with child", Sex == "Female") %>%
  select(ID, HouseholdID, Age, Sex) %>%
  mutate(Type = "Mother")

ThreePersonKids <- File16SyntheticPopulation %>%
  filter(HouseholdID %in% ThreePersonMothersCpls$HouseholdID, Type == "Child") %>%
  select(ID, HouseholdID, Age, Sex, Type) 

ThreePersonKidsPlusMums <- bind_rows(ThreePersonMothersCpls, ThreePersonKids)


# plot kids against mothers

# scale = 1, exactly touching
File21ThreePersonMumsPlusKids <- ggplot(ThreePersonKidsPlusMums, aes(x = Age, y = Type, 
                                                                     fill = factor(stat(quantile)))) + 
  stat_density_ridges(scale = 1, geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE) +
  scale_fill_cyclical(values = c("#1B9E77","#D95F02","#7570b3","#e7298a")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100), expand = c(0, 2)) +
  labs(x = "Age (years)", y = "Type of person")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(File21ThreePersonMumsPlusKids, file="File21ThreePersonMumsPlusKids.pdf", width=10, height=6, units="in")
  
# get the mother age quantiles
quantile(ThreePersonMothersCpls$Age, probs = c(.1, 0.5,0.7, 0.75, .9))

# get the child age quantiles
quantile(ThreePersonKids$Age, probs = c(.1, 0.5, 0.7, 0.75, .9))


Mums18to57 <- ThreePersonMothersCpls %>%
  filter(between(Age, 18,57))

Kids18to57 <- ThreePersonKids %>%
  filter(between(Age, 18,57))

round(nrow(Kids18to57)/nrow(ThreePersonKids), 3)

Mothers34Plus <- ThreePersonMothersCpls %>%
  filter(Age > 33)

# how did the pairs line up, merge


MergedMumsKids <- left_join(ThreePersonMothersCpls, ThreePersonKids, by = "HouseholdID")

MumAgeByKidAge <- MergedMumsKids %>%
  group_by(Age.y)%>% 
  summarize(median = median(Age.x)) %>%
  mutate(MedianDiff = median - Age.y)


