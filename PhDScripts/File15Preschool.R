
#  rm(list = ls())

library(dplyr)

# Get the random seed file
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# get the population data

File14EmployersAdded <- readRDS("PhDRData/File14EmployersAdded.rds")

# what are the numbers of children each age and therefore the prop in preschool/ECE?

KidsUnderFive <- File14EmployersAdded %>%
  filter(Age < 5) %>%
  group_by(Age) %>%
  summarise(NumByAge = n()) %>%
  mutate(ProbOfInECE = ifelse(Age == 0, 0.123, 
                              ifelse(Age == 1, 0.359, 
                                     ifelse(Age == 2, 0.565, 
                                            ifelse(Age == 3, 0.875, 0.934)))),
         CountinECE = round(NumByAge*ProbOfInECE, 0))

sum(KidsUnderFive$NumByAge)

# how many in ECE?
sum(KidsUnderFive$CountinECE)

KidsInECEInclHomeBased <- File14EmployersAdded %>%
  filter(Age < 5) %>%
  group_by(Age) %>%
  summarise(NumByAge = n()) %>%
  mutate(ProbOfInECEAll = ifelse(Age == 0, 0.153, 
                              ifelse(Age == 1, 0.43, 
                                     ifelse(Age == 2, 0.638, 
                                            ifelse(Age == 3, 0.942, 0.984)))),
         CountinECEAll = round(NumByAge*ProbOfInECEAll, 0))

sum(KidsInECEInclHomeBased$CountinECEAll)


# how many different households

KidsUnderFiveHouseholds <- File14EmployersAdded %>%
  filter(Age < 5) %>%
  group_by(HouseholdID) %>%
  summarise(NumKidsInHousehold = n())

# households in synthetic pop
SyntheticPopHouseholds <- File14EmployersAdded %>%
  group_by(HouseholdID) %>%
  summarise(HouseholdSize = n())

HouseholdsWithKidsUnderFive <- left_join(KidsUnderFiveHouseholds, SyntheticPopHouseholds, by = "HouseholdID")

# median household size with kids aged between 0 and 4
median(HouseholdsWithKidsUnderFive$HouseholdSize)


########################################################################
# make sure that kids of same-sex couples are included, if they have under-5s
#######################################################################
# include households with same-sex couples

SameSexCouples <- File14EmployersAdded %>%
  filter(Type == "Single sex with child")

# get the under-5s of same-sex couples
KidsOfSameSexCouples <- File14EmployersAdded %>%
  filter(HouseholdID %in% SameSexCouples$HouseholdID, Age < 5)

# there are two same-sex couples with pre-school children

rm(KidsUnderFive, KidsUnderFiveHouseholds, SyntheticPopHouseholds, HouseholdsWithKidsUnderFive,
   SameSexCouples, KidsOfSameSexCouples)






#######################################################################
#######################################################################
# Extract the households that have the different ages of the preschool children
#######################################################################
#######################################################################

Years4 <- File14EmployersAdded %>%
  filter(Age == 4)

HouseholdsWith4YearOlds <- File14EmployersAdded %>%
  filter(HouseholdID %in% Years4$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)

Years3 <- File14EmployersAdded %>%
  filter(Age == 3)

HouseholdsWith3YearOlds <- File14EmployersAdded %>%
  filter(HouseholdID %in% Years3$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)

Years2 <- File14EmployersAdded %>%
  filter(Age == 2)

HouseholdsWith2YearOlds <- File14EmployersAdded %>%
  filter(HouseholdID %in% Years2$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)

Years1 <- File14EmployersAdded %>%
  filter(Age == 1)

HouseholdsWith1YearOlds <- File14EmployersAdded %>%
  filter(HouseholdID %in% Years1$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)

Years0 <- File14EmployersAdded %>%
  filter(Age == 0)

HouseholdsWith0YearOlds <- File14EmployersAdded %>%
  filter(HouseholdID %in% Years0$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)





#######################################################################
# zero and one child families - how many in the data
#######################################################################

HouseholdsWithOnly0s <- HouseholdsWith0YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsWith1YearOlds$HouseholdID, HouseholdsWith2YearOlds$HouseholdID,
                             HouseholdsWith3YearOlds$HouseholdID, HouseholdsWith4YearOlds$HouseholdID))

# 104 households with only 0s
#prop with only 0-year-old
nrow(HouseholdsWithOnly0s)/nrow(HouseholdsWith0YearOlds) # 38.4%

nrow(HouseholdsWith0YearOlds) - nrow(HouseholdsWithOnly0s)

# almost 40% of households with under ones only have that one child
# currently have 167 households of under-ones with older siblings


# check proportion of ones living just with parent 

HouseholdsWithOnly1s <- HouseholdsWith1YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsWith0YearOlds$HouseholdID, HouseholdsWith2YearOlds$HouseholdID,
                             HouseholdsWith3YearOlds$HouseholdID, HouseholdsWith4YearOlds$HouseholdID))

#prop with only 1-year-old
nrow(HouseholdsWithOnly1s)/nrow(HouseholdsWith1YearOlds) # 46.5%


# check proportion of twos living just with parent 

HouseholdsWithOnly2s <- HouseholdsWith2YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsWith0YearOlds$HouseholdID, HouseholdsWith1YearOlds$HouseholdID,
                             HouseholdsWith3YearOlds$HouseholdID, HouseholdsWith4YearOlds$HouseholdID))

#prop with only 2-year-old
nrow(HouseholdsWithOnly2s)/nrow(HouseholdsWith2YearOlds) # 49.6%


# check proportion of threes living just with parent 

HouseholdsWithOnly3s <- HouseholdsWith3YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsWith0YearOlds$HouseholdID, HouseholdsWith1YearOlds$HouseholdID,
                             HouseholdsWith2YearOlds$HouseholdID, HouseholdsWith4YearOlds$HouseholdID))

#prop with only 2-year-old
nrow(HouseholdsWithOnly3s)/nrow(HouseholdsWith3YearOlds) # 58.1%








#######################################################################
#######################################################################
# Select the households - constraints are expected count per age
# start with the four-year-olds and work backwards
#######################################################################
#######################################################################

#######################################################################
# Four year olds - need 254
#######################################################################

# limit households containing children with those aged under 1 year, otherwise 39 out of 41 will be selected, 
# and max required is 34

HouseholdsWith0And4 <- bind_rows(HouseholdsWith4YearOlds, HouseholdsWith0YearOlds) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

# note: removal of 16 only can be done (270-254)

set.seed(TheRandomSeeds[116])                                              #################### seed 116 
HouseholdsWith0NotSelected <- HouseholdsWith0And4 %>%
  slice_sample(n=16, replace = FALSE)

HouseholdsECE4 <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsWith4YearOlds$HouseholdID) %>%
  filter(!HouseholdID %in% HouseholdsWith0NotSelected$HouseholdID) %>%
  select(HouseholdID) %>%
  distinct(HouseholdID)

ChildrenInECE4 <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsECE4$HouseholdID, Age <5) 

table(ChildrenInECE4$Age)

ChildrenInECE4Vector <- as.vector(table(ChildrenInECE4$Age))



#######################################################################
# Three year olds - need 257
#######################################################################



HouseholdsAlreadySelected3 <- HouseholdsWith3YearOlds %>%
  filter(HouseholdID %in% HouseholdsECE4$HouseholdID)

HouseholdsNeeded3 <- 257-ChildrenInECE4Vector[4] # number households still required

SingleChild3 <- round(257* (nrow(HouseholdsWithOnly3s)/nrow(HouseholdsWith3YearOlds)),0)

SingleChild3Parents <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsWithOnly3s$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         Weight = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb))


set.seed(TheRandomSeeds[117])                                              #################### seed 117 
HouseholdsECE3Only <- SingleChild3Parents %>%
  slice_sample(n=SingleChild3, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

# probability of selection
nrow(HouseholdsECE3Only)/nrow(HouseholdsWithOnly3s)

OtherChildren3 <- 257-(ChildrenInECE4Vector[4] + SingleChild3)


HouseholdsAvailable3 <- HouseholdsWith3YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsAlreadySelected3$HouseholdID, HouseholdsWith0NotSelected$HouseholdID,
                             HouseholdsECE3Only$HouseholdID))

HouseholdsWith3And0 <- bind_rows(HouseholdsAvailable3, HouseholdsWith0YearOlds) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

HouseholdsWith3And1 <- bind_rows(HouseholdsAvailable3, HouseholdsWith1YearOlds) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

HouseholdsWith3And1And0 <- bind_rows(HouseholdsWith3And0, HouseholdsWith3And1) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

DownWeight30 <- (as.numeric((nrow(HouseholdsWith3And0)-nrow(HouseholdsWith3And1And0))/nrow(HouseholdsWith3YearOlds)))/10
DownWeight31 <- (as.numeric((nrow(HouseholdsWith3And1)-nrow(HouseholdsWith3And1And0))/nrow(HouseholdsWith3YearOlds)))/10
DownWeight310 <- (as.numeric(nrow(HouseholdsWith3And1And0)/nrow(HouseholdsWith3YearOlds)))/10


Households0Test <- (as.numeric(nrow(HouseholdsWith3And0)/nrow(HouseholdsWith3YearOlds)))/10


HouseholdsWith3and0Only <- HouseholdsWith3And0 %>%
  filter(!HouseholdID %in% HouseholdsWith3And1And0$HouseholdID)

HouseholdsWith3And1Only <- HouseholdsWith3And1 %>%
  filter(!HouseholdID %in% HouseholdsWith3And1And0$HouseholdID)

# households with 3 and 0 etc 
nrow(HouseholdsWith3and0Only) + nrow(HouseholdsWith3And1Only) + nrow(HouseholdsWith3And1And0)

Parents3 <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsAvailable3$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         ProbWithParentStatus = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb),
         Weight = ifelse(HouseholdID %in% HouseholdsWith3and0Only$HouseholdID, ProbWithParentStatus*DownWeight30,
                         ifelse(HouseholdID %in% HouseholdsWith3And1Only$HouseholdID, ProbWithParentStatus*DownWeight31,
                                ifelse(HouseholdID %in% HouseholdsWith3And1And0$HouseholdID, ProbWithParentStatus*DownWeight310,
                                       ProbWithParentStatus))))

set.seed(TheRandomSeeds[118])                                              #################### seed 118 
HouseholdsECE3Multiple <- Parents3 %>%
  slice_sample(n=OtherChildren3, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

(nrow(HouseholdsWith3and0Only) + nrow(HouseholdsWith3And1Only) + nrow(HouseholdsWith3And1And0))/nrow(Parents3)

ChildrenInECE3 <- File14EmployersAdded %>%
  filter(HouseholdID %in% c(HouseholdsECE3Only$HouseholdID, HouseholdsECE3Multiple$HouseholdID), Age <5) 

table(ChildrenInECE3$Age)

ChildrenInECE <- bind_rows(ChildrenInECE3, ChildrenInECE4) %>%
  distinct()

table(ChildrenInECE$Age)


HouseholdsRejected3 <- HouseholdsAvailable3 %>%
  filter(!HouseholdID %in% ChildrenInECE3$HouseholdID)

# # get 23 0 year olds and 24 one year olds, 55 two year olds try again without splitting
# 
# Test3s <- File14EmployersAdded %>%
#   filter(HouseholdID %in% HouseholdsWith3YearOlds$HouseholdID) %>%
#   filter(!HouseholdID %in% HouseholdsAlreadySelected3$HouseholdID) %>%
#   filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
#   mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
#                               ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
#                                                         "30-39 Hours Worked"), 0.6, 1.2)),
#          ProbWithParentStatus = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb),
#          Weight = ifelse(HouseholdID %in% HouseholdsWith3and0Only$HouseholdID, ProbWithParentStatus*DownWeight30,
#                          ifelse(HouseholdID %in% HouseholdsWith3And1Only$HouseholdID, ProbWithParentStatus*DownWeight31,
#                                 ifelse(HouseholdID %in% HouseholdsWith3And1And0$HouseholdID, ProbWithParentStatus*DownWeight310,
#                                        ProbWithParentStatus))))
# 
# set.seed(TheRandomSeeds[118])                                              #################### seed 118 
# HouseholdsTest3s <- Test3s %>%
#   slice_sample(n=HouseholdsNeeded3, weight_by = Weight, replace = FALSE) %>%
#   select(HouseholdID)
# 
# ChildrenTest3 <- File14EmployersAdded %>%
#   filter(HouseholdID %in% HouseholdsTest3s$HouseholdID, Age <5) 
# 
# table(ChildrenTest3$Age)
# 
# # get 27 0 year olds, 20 one year olds, and 22 two year olds
# 
# # try with no weights
# 
# set.seed(TheRandomSeeds[118])                                              #################### seed 118 
# HouseholdsTest3sb <- Test3s %>%
#   slice_sample(n=HouseholdsNeeded3, replace = FALSE) %>%
#   select(HouseholdID)
# 
# ChildrenTest3b <- File14EmployersAdded %>%
#   filter(HouseholdID %in% HouseholdsTest3sb$HouseholdID, Age <5) 
# 
# table(ChildrenTest3b$Age)

# no weights is objectively worse - 42 0 year olds, 36 one year olds, 24 two year olds selected instead

HouseholdsNotSelected3 <- HouseholdsWith3YearOlds %>%
  filter(!HouseholdID %in% HouseholdsECE3Multiple$HouseholdID)


ChildrenInECE3Vector <- as.vector(table(ChildrenInECE$Age))



#######################################################################
# Two year olds - need 147
#######################################################################

HouseholdsAlreadySelected2 <- HouseholdsWith2YearOlds %>%
  filter(HouseholdID %in% c(HouseholdsECE4$HouseholdID, ChildrenInECE3$HouseholdID)) 

HouseholdsAvailable2 <- HouseholdsWith2YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsWith0NotSelected$HouseholdID, HouseholdsAlreadySelected2$HouseholdID,
                             HouseholdsNotSelected3$HouseholdID))

HouseholdsNeeded2 <- 147-ChildrenInECE3Vector[3]  # number households still required

SingleChild2 <- round(147* (nrow(HouseholdsWithOnly2s)/nrow(HouseholdsWith2YearOlds)),0)

SingleChild2Parents <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsWithOnly2s$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         Weight = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb))


set.seed(TheRandomSeeds[119])                                              #################### seed 119 
HouseholdsECE2Only <- SingleChild2Parents %>%
  slice_sample(n=SingleChild2, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

OtherChildren2 <- 147-(ChildrenInECE3Vector[3] + SingleChild2)


HouseholdsAvailable2 <- HouseholdsWith2YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsAlreadySelected2$HouseholdID, HouseholdsNotSelected3$HouseholdID,
                             HouseholdsECE2Only$HouseholdID, HouseholdsWith0NotSelected$HouseholdID))


HouseholdsWith0And2 <- bind_rows(HouseholdsAvailable2, HouseholdsWith0YearOlds) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

HouseholdsWith1And2 <- bind_rows(HouseholdsAvailable2, HouseholdsWith1YearOlds) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)

HouseholdsWith0And1And2 <- bind_rows(HouseholdsWith0And2, HouseholdsWith1And2) %>%
  group_by(HouseholdID) %>%
  summarise(Num = n()) %>%
  filter(Num == 2)


DownWeight20 <- (as.numeric((nrow(HouseholdsWith0And2)-nrow(HouseholdsWith0And1And2))/nrow(HouseholdsAvailable2)))
DownWeight21 <- (as.numeric((nrow(HouseholdsWith1And2)-nrow(HouseholdsWith0And1And2))/nrow(HouseholdsAvailable2)))
DownWeight210 <- (as.numeric(nrow(HouseholdsWith0And1And2)/nrow(HouseholdsAvailable2)))


HouseholdsWith0and2Only <- HouseholdsWith0And2 %>%
  filter(!HouseholdID %in% HouseholdsWith0And1And2$HouseholdID)

HouseholdsWith1And2Only <- HouseholdsWith1And2 %>%
  filter(!HouseholdID %in% HouseholdsWith0And1And2$HouseholdID)


Parents2 <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsAvailable2$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         ProbWithParentStatus = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb),
         Weight = ifelse(HouseholdID %in% HouseholdsWith0and2Only$HouseholdID, ProbWithParentStatus*DownWeight20,
                         ifelse(HouseholdID %in% HouseholdsWith1And2Only$HouseholdID, ProbWithParentStatus*DownWeight21,
                                ifelse(HouseholdID %in% HouseholdsWith0And1And2$HouseholdID, ProbWithParentStatus*DownWeight210,
                                       ProbWithParentStatus))))


set.seed(TheRandomSeeds[120])                                              #################### seed 120 
HouseholdsECE2Multiple <- Parents2 %>%
  slice_sample(n=OtherChildren2, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

ChildrenInECE2 <- File14EmployersAdded %>%
  filter(HouseholdID %in% c(HouseholdsECE2Only$HouseholdID, HouseholdsECE2Multiple$HouseholdID), Age <5) 

table(ChildrenInECE2$Age)

ChildrenInECE <- bind_rows(ChildrenInECE3, ChildrenInECE4, ChildrenInECE2) %>%
  distinct()

table(ChildrenInECE$Age)

HouseholdsNotSelected2 <- HouseholdsAvailable2 %>%
  filter(!HouseholdID %in% ChildrenInECE2$HouseholdID)

HouseholdsNotSelectedRolling <- bind_rows(HouseholdsNotSelected3, HouseholdsNotSelected2)

ChildrenInECE2Vector <- as.vector(table(ChildrenInECE$Age))


#######################################################################
# One year olds - need 95
#######################################################################

HouseholdsAlreadySelected1 <- HouseholdsWith1YearOlds %>%
  filter(HouseholdID %in% c(HouseholdsECE4$HouseholdID, ChildrenInECE3$HouseholdID, ChildrenInECE2$HouseholdID)) 

HouseholdsAvailable1 <- HouseholdsWith1YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsNotSelectedRolling$HouseholdID, HouseholdsWith0NotSelected$HouseholdID))

HouseholdsNeeded1 <- 95-ChildrenInECE2Vector[2] # number households still required

SingleChild1 <- round(95* (nrow(HouseholdsWithOnly1s)/nrow(HouseholdsWith1YearOlds)),0)


SingleChild1Parents <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsWithOnly1s$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         Weight = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb))


set.seed(TheRandomSeeds[121])                                              #################### seed 121 
HouseholdsECE1Only <- SingleChild1Parents %>%
  slice_sample(n=SingleChild1, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

OtherChildren1 <- 95-(ChildrenInECE2Vector[2] + SingleChild1)

HouseholdsAvailable1 <- HouseholdsWith1YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsAlreadySelected1$HouseholdID, HouseholdsNotSelectedRolling$HouseholdID,
                             HouseholdsECE1Only$HouseholdID, HouseholdsWith0NotSelected$HouseholdID))

# other children needed were
# 80/141 = 57% of the number of one-child households for three year olds = single child count/otherchildren count
# 28/64 = 44% of the number of one-child households for two year olds


Parents1 <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsAvailable1$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         Weight = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb))


set.seed(TheRandomSeeds[122])                                              #################### seed 122
HouseholdsECE1Multiple <- Parents1 %>%
  slice_sample(n=OtherChildren1, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

ChildrenInECE1 <- File14EmployersAdded %>%
  filter(HouseholdID %in% c(HouseholdsECE1Only$HouseholdID, HouseholdsECE1Multiple$HouseholdID), Age <5)

table(ChildrenInECE1$Age)

ChildrenInECE <- bind_rows(ChildrenInECE3, ChildrenInECE4, ChildrenInECE2, ChildrenInECE1) %>%
  distinct()

table(ChildrenInECE$Age)

HouseholdsNotSelected1 <- HouseholdsAvailable1 %>%
  filter(!HouseholdID %in% ChildrenInECE1$HouseholdID)

HouseholdsNotSelectedRolling <- bind_rows(HouseholdsNotSelectedRolling, HouseholdsNotSelected1)






#######################################################################
# Zero year olds - need 34 but have exceeded
#######################################################################

HouseholdsAlreadySelected0 <- HouseholdsWith0YearOlds %>%
  filter(HouseholdID %in% c(HouseholdsECE4$HouseholdID, ChildrenInECE3$HouseholdID, ChildrenInECE2$HouseholdID,
                            ChildrenInECE1$HouseholdID)) 

HouseholdsAvailable0 <- HouseholdsWith0YearOlds %>%
  filter(!HouseholdID %in% c(HouseholdsNotSelectedRolling$HouseholdID))

# take the same ratio of 0s:1s as there is 1s:3s - around 64%
# 64% of 96 is 61
# already have 52
# missing 9


SingleChild0Parents <- File14EmployersAdded %>%
  filter(HouseholdID %in% HouseholdsWithOnly0s$HouseholdID) %>%
  filter((Sex == "Female" & Type == "Opposite sex with child") | Type == "Sole Mother" | Type == "Sole Father") %>%
  mutate(InitialProb = ifelse(HoursWorked == "No Hours", 0.2,
                              ifelse(HoursWorked %in% c("1-9 Hours Worked", "10-19 Hours Worked", "20-29 Hours Worked",
                                                        "30-39 Hours Worked"), 0.6, 1.2)),
         Weight = ifelse(Type == "Sole Mother", InitialProb*2, InitialProb))


set.seed(TheRandomSeeds[121])                                              #################### seed 121 
HouseholdsECE0 <- SingleChild0Parents %>%
  slice_sample(n=9, weight_by = Weight, replace = FALSE) %>%
  select(HouseholdID)

ChildrenInECE0 <- File14EmployersAdded %>%
  filter(HouseholdID %in% c(HouseholdsECE0$HouseholdID), Age <5)

table(ChildrenInECE0$Age)

ChildrenInECE <- bind_rows(ChildrenInECE3, ChildrenInECE4, ChildrenInECE2, ChildrenInECE1, ChildrenInECE0) %>%
  distinct()

table(ChildrenInECE$Age)









#######################################################################
# Add ECE indicator - also gives count of children in ECE
#######################################################################

ChildrenInECE <- ChildrenInECE %>%
  mutate(ECEIndicator = "Child")



#######################################################################
# Match children to ECE employers and ECE staff
#######################################################################

ECEStaff <- File14EmployersAdded %>%
  filter(IndCode == "P801000") %>%
  mutate(ECEIndicator = "Staff")

# number employees is the number of rows in that data frame

# number of ECE centres
ECEEmployers <- ECEStaff %>%
  group_by(Company) %>%
  summarise(NumStaff = n())

# get median number of staff
median(ECEEmployers$NumStaff)

# calculate average kids per ECE centre
nrow(ChildrenInECE)/nrow(ECEEmployers)

# assign each child to an ECE centre
# by household, assume each child goes to same provider

# ECE households, n=number kids

ECEHouseholds <- ChildrenInECE %>%
  group_by(HouseholdID) %>%
  summarise(NumKids = n())

# get the ECE for each household by random sampling the ECE providers, weighted by number of staff
# need 614, one for each household
# thus, sampling WITH replacement as there are only 26 providers

set.seed(TheRandomSeeds[122])                                              #################### seed 122 
ECESelected <- ECEEmployers %>%
  slice_sample(n=nrow(ECEHouseholds), weight_by = NumStaff, replace = TRUE) %>%
  select(Company)

# join to the housholds by join columns

ECEHouseholdsWithProviders <- bind_cols(ECEHouseholds, ECESelected) %>%
  rename(ECEProvider = Company)

ECEStaff <- ECEStaff %>%
  filter(IndCode == "P801000") %>%
  mutate(ECEProvider = Company)

# add the ECE indicator to the kids

ChildrenInECEWithProvider <- left_join(ChildrenInECE, ECEHouseholdsWithProviders, by = "HouseholdID") %>%
  select(-NumKids)

# how many ECE providers are used?

NumECEProvidersUsed <- ChildrenInECEWithProvider %>%
  select(ECEProvider) %>%
  distinct(ECEProvider)
  

# put the staff and kids back together

KidsAndStaff <- bind_rows(ECEStaff, ChildrenInECEWithProvider)


# get the remaining people and give them the extra variables
NotKidsOrStaff <- File14EmployersAdded %>%
  filter(!ID %in% KidsAndStaff$ID) %>%
  mutate(ECEIndicator = "None",
         ECEProvider = "None")


# put the synthetic population back together

File15ECEAdded <- bind_rows(KidsAndStaff, NotKidsOrStaff)


# check for duplicates
duplicates <- File15ECEAdded %>%
  group_by(ID) %>%
  summarise(Number = n()) %>%
  filter(Number > 1)

# no duplicates, save the file

saveRDS(File15ECEAdded, file = "PhDRData/File15ECEAdded.rds")












################################################################
################################################################
# Rewrite the schools names with the real IDs
################################################################
################################################################

File15ECEAdded <- readRDS("PhDRData/File15ECEAdded.rds")

PrimarySchoolEmployees <- File15ECEAdded %>%
  filter(IndCode == "P802100",
         EducationStatus == "N")

SecondarySchoolEmployees <- File15ECEAdded %>%
  filter(IndCode == "P802200",
         EducationStatus == "N")

PrimarySchools <- PrimarySchoolEmployees %>%
  group_by(Company) %>%
  summarise(NumStaffSec = n())

SecondarySchools <- SecondarySchoolEmployees %>%
  group_by(Company) %>%
  summarise(NumStaffSec = n())

File13SchoolsDataFrame <- readRDS("PhDRData/File13SchoolsDataFrame.rds")

TheSchools <- File13SchoolsDataFrame %>%
  group_by(SchoolName, SchoolID, SchoolType) %>%
  summarise(NumPupils = sum(RollCount)) %>%
  ungroup()


# Draw the five schools randomly, will be col bound to SecondarySchools
# extract the secondary schools

SecWithCounts <- TheSchools %>%
  filter(SchoolName %in% c("Craighead Diocesan School", "Mountainview High School", "Roncalli College", 
                           "Timaru Boys' High School", "Timaru Girls' High School"))

set.seed(TheRandomSeeds[123])                                              #################### seed 123 
SecSchoolsSampled <- SecWithCounts %>%
  slice_sample(n=nrow(SecondarySchools), replace = FALSE)

SecSchoolsNamed <- bind_cols(SecondarySchools, SecSchoolsSampled) %>%
  select(c(Company, SchoolID))




# Note: 26 primary school companies, but only 11 in the data
# rework the primary school employees into the 11 primary schools in the school roll data


PrimWithCounts <-  TheSchools %>%
  filter(!SchoolName %in% c("Craighead Diocesan School", "Mountainview High School", "Roncalli College", 
                            "Timaru Boys' High School", "Timaru Girls' High School"))

# create a dataset of schools, one for each person employed in a primary school

set.seed(TheRandomSeeds[124])                                              #################### seed 124 
PrimTeachersWithSchools <- PrimWithCounts %>%
  slice_sample(n=nrow(PrimarySchoolEmployees), weight_by = NumPupils, replace = TRUE) %>%
  select(SchoolID) %>%
  bind_cols(PrimarySchoolEmployees) %>%
  mutate(Company = SchoolID) %>%
  select(-SchoolID)


# replace the company names for the secondary school teachers

SecTeachersWithSchools <- inner_join(SecondarySchoolEmployees, SecSchoolsNamed, by = "Company") %>%
  mutate(Company = SchoolID) %>%
  select(-SchoolID)


SchoolKidsWorkInSecSchools <- File15ECEAdded %>%
  filter(EducationStatus == "Y", Company %in% SecondarySchools$Company) %>%
  left_join(SecSchoolsNamed, by = "Company") %>%
  mutate(Company = SchoolID) %>%
  select(-SchoolID)

SchoolKidsWorkInPrimSchools <- File15ECEAdded %>%
  filter(EducationStatus == "Y", Company %in% PrimarySchools$Company)

set.seed(TheRandomSeeds[125])                                              #################### seed 125 
SchoolKidsWorkInPrimSchoolsF <- PrimWithCounts %>%
  slice_sample(n=nrow(SchoolKidsWorkInPrimSchools), weight_by = NumPupils, replace = TRUE) %>%
  select(SchoolID) %>%
  bind_cols(SchoolKidsWorkInPrimSchools) %>%
  mutate(Company = SchoolID) %>%
  select(-SchoolID)


EveryoneElse <- File15ECEAdded %>%
  filter(!ID %in% c(SecTeachersWithSchools$ID, PrimTeachersWithSchools$ID,SchoolKidsWorkInSecSchools$ID,
                    SchoolKidsWorkInPrimSchoolsF$ID)) 

# finalise the data

File15AllEducation <- bind_rows(SecTeachersWithSchools, PrimTeachersWithSchools, SchoolKidsWorkInSecSchools,
                                SchoolKidsWorkInPrimSchoolsF, EveryoneElse) 


dups <- File15AllEducation %>%
  group_by(ID) %>%
  summarise(Dup = n()) %>%
  filter(Dup > 1)

# no duplicates, save the file

saveRDS(File15AllEducation, file = "PhDRData/File15AllEducation.rds")

# how many adolescents were working?

NumAdolescentWorking <- File15AllEducation %>%
  filter(EducationStatus == "Y", !Company =="Not employed")

