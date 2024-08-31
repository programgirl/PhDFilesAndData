#clear workspace
#  rm(list = ls())

library(dplyr)
library(ggplot2)
library(conflicted)

conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")





####################################################################################################
####################################################################################################
# Final data frame
####################################################################################################
####################################################################################################

File7OnePersonHH <- readRDS("PhDRData/InterimHouseholdSizeWork/File7OnePerson/File7OnePersonHH.rds")
TwoPersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File8TwoPerson/TwoPersonHouseholdsComplete.rds")
ThreePersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File9ThreePerson/ThreePersonHouseholdsComplete.rds")
FourPersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File10FourPerson/FourPersonHouseholdsComplete.rds")
FivePersonHouseholdsComplete <- readRDS("PhDRData/InterimHouseholdSizeWork/File11FivePerson/FivePersonHouseholdsComplete.rds")

File12PopulationHouseholds <- bind_rows(File7OnePersonHH, TwoPersonHouseholdsComplete, ThreePersonHouseholdsComplete,
                                        FourPersonHouseholdsComplete, FivePersonHouseholdsComplete)

duplicates <- File12PopulationHouseholds %>%
  group_by(ID) %>%
  summarise(Count = n()) %>%
  filter(Count !=1)

saveRDS(File12PopulationHouseholds, file = "PhDRData/File12PopulationHouseholds.rds")

# how many households?

NumHouseholds <- File12PopulationHouseholds %>%
  group_by(HouseholdID) %>%
  summarise(Count = n()) 
  
# average household size
mean(NumHouseholds$Count)

# how many households with kids
HouseholdsWithKids <- File12PopulationHouseholds %>%
  filter(Type == "Child") %>%
  group_by(HouseholdID) %>%
  summarise(Count = n())

# prop of households containing kids
round((nrow(HouseholdsWithKids)/nrow(NumHouseholds)),3)*100

# average number of kids per household, for households with kids
mean(HouseholdsWithKids$Count)

  

# household stats
table(File12PopulationHouseholds$Type)

#one person
OnePersonNum <- as.numeric(nrow(File7OnePersonHH))





####################################################################################################
# two people
####################################################################################################
CouplesOnlyNum2P <- as.numeric(nrow(TwoPersonHouseholdsComplete %>%
  filter(Type == "Opposite sex no kids") %>%
  group_by(HouseholdID) %>%
  distinct(HouseholdID)))

SolePM2P <- as.numeric(nrow(TwoPersonHouseholdsComplete %>%
  filter(Type == "Sole Mother")))

SolePF2P <- as.numeric(nrow(TwoPersonHouseholdsComplete %>%
                              filter(Type == "Sole Father")))

Unrelated2P <- as.numeric(nrow(TwoPersonHouseholdsComplete %>%
  filter(Type == "Other") %>%
  group_by(HouseholdID) %>%
  distinct(HouseholdID)))

SameSexNoKids2P <- as.numeric(nrow(TwoPersonHouseholdsComplete %>%
  filter(Type == "Single sex no kids") %>%
  group_by(HouseholdID) %>%
  distinct(HouseholdID)))

####################################################################################################
# missing the two person household stats in the chapter, do here

TwoPersonHouseholdTypes <- data.frame(Type = c("Opposite sex no kids", "Sole Mother", "Sole Father", "Other",
                                               "Single sex no kids"),
                                      Count = c(CouplesOnlyNum2P, SolePM2P, SolePF2P, Unrelated2P, SameSexNoKids2P))

TwoPersonHouseholdTypes <- TwoPersonHouseholdTypes %>%
  mutate(Prop = round((Count/sum(Count)),3)) %>%
  arrange(-Count)

rm(TwoPersonHouseholdTypes)
####################################################################################################








####################################################################################################
# three people
####################################################################################################

CouplesKidsNum3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
                                      filter(Type == "Opposite sex with child") %>%
                                      group_by(HouseholdID) %>%
                                      distinct(HouseholdID)))

SolePM3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
                              filter(Type == "Sole Mother")))

SolePF3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
                              filter(Type == "Sole Father")))

CouplesNoKids3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
                             filter(Type == "Opposite sex without child") %>%
                             group_by(HouseholdID) %>%
                             distinct(HouseholdID)))

SameSexKids3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
                                     filter(Type == "Single sex with child") %>%
                                     group_by(HouseholdID) %>%
                                     distinct(HouseholdID)))

Unrelated3P <- as.numeric(nrow(ThreePersonHouseholdsComplete %>%
  filter(Type == "Other") %>%
  group_by(HouseholdID) %>%
  summarise(Count = n()) %>%
  filter(Count == 3)))





####################################################################################################
# four people
####################################################################################################

CouplesKidsNum4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                                      filter(Type == "Opposite sex with child") %>%
                                      group_by(HouseholdID) %>%
                                      distinct(HouseholdID)))

SolePM4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                              filter(Type == "Sole Mother")))

SolePF4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                              filter(Type == "Sole Father")))

CouplesNoKids4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                                     filter(Type == "Opposite sex without child") %>%
                                     group_by(HouseholdID) %>%
                                     distinct(HouseholdID)))

SameSexKids4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                                   filter(Type == "Single sex with child") %>%
                                   group_by(HouseholdID) %>%
                                   distinct(HouseholdID)))

Unrelated4P <- as.numeric(nrow(FourPersonHouseholdsComplete %>%
                                 filter(Type == "Other") %>%
                                 group_by(HouseholdID) %>%
                                 summarise(Count = n()) %>%
                                 filter(Count == 4)))





####################################################################################################
# five people
####################################################################################################

CouplesKidsNum5P <- as.numeric(nrow(FivePersonHouseholdsComplete %>%
                                      filter(Type == "Opposite sex with child") %>%
                                      group_by(HouseholdID) %>%
                                      distinct(HouseholdID)))

SolePM5P <- as.numeric(nrow(FivePersonHouseholdsComplete %>%
                              filter(Type == "Sole Mother")))

SolePF5P <- as.numeric(nrow(FivePersonHouseholdsComplete %>%
                              filter(Type == "Sole Father")))

CouplesNoKids5P <- as.numeric(nrow(FivePersonHouseholdsComplete %>%
                                     filter(Type == "Opposite sex without child") %>%
                                     group_by(HouseholdID) %>%
                                     distinct(HouseholdID)))

Unrelated5P <- as.numeric(nrow(FivePersonHouseholdsComplete %>%
                                 filter(Type == "Other") %>%
                                 group_by(HouseholdID) %>%
                                 summarise(Count = n()) %>%
                                 filter(Count == 5)))





####################################################################################################
# combined across household sizes
####################################################################################################

DataframeOfHouseholds <- data.frame(Type = c("One person household", "Opposite-sex couple only", 
                                             "Opposite-sex couple with children", "Same-sex couple only",
                                             "Same-sex with children", "Female sole parent", "Male sole parent",
                                             "Opposite-sex couple with unrelated", "Household of unrelated"),
                                    Count = c(OnePersonNum, CouplesOnlyNum2P, 
                                               sum(CouplesKidsNum3P, CouplesKidsNum4P,CouplesKidsNum5P),
                                               SameSexNoKids2P, sum(SameSexKids3P, SameSexKids4P),
                                               sum(SolePM2P, SolePM3P, SolePM4P, SolePM5P),
                                               sum(SolePF2P, SolePF3P, SolePF4P, SolePF5P),
                                               sum(CouplesNoKids3P, CouplesNoKids4P, CouplesNoKids5P),
                                               sum(Unrelated2P, Unrelated3P, Unrelated4P, Unrelated5P)))

DataframeOfHouseholds <- DataframeOfHouseholds %>%
  mutate(Prop = round((Count/sum(Count)),4)) %>%
  arrange(-Count)
