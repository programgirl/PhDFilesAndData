#clear workspace
#  rm(list = ls())



library(dplyr)
library(ggplot2)

# get colour theme data
# library(RColorBrewer)
# brewer.pal(n = 7, name = "Dark2")
# plotrix::color.id("#1B9E77")
# plotrix::color.id("#D95F02")
# plotrix::color.id("#7570B3")
# plotrix::color.id("#E7298A")
# plotrix::color.id("#66A61E")
# plotrix::color.id("#E6AB02")
# plotrix::color.id("#A6761D")
# detach("package:RColorBrewer", unload = TRUE)

####################################################################################################
####################################################################################################
# Fix relationship status
####################################################################################################
####################################################################################################

File2AgeStructure <- readRDS("~/Sync/PhD/PhDMay2023/PhDRData/File2AgeStructure.rds")








####################################################################################################
# Set up custom breaks for graphs
####################################################################################################
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

custom_breaks <- seq(0, 110, 10)











####################################################################################################
# Create expected probabilities using interdiff function from PopulateR
####################################################################################################

File2AgeStructure <- readRDS("~/Sync/PhD/PhDMay2023/PhDRData/File2AgeStructure.rds")

# reduce down hours worked text

File2AgeStructure <- File2AgeStructure %>%
  mutate(HoursWorked = ifelse(HoursWorked == "No Hours", "0",
                              ifelse(HoursWorked == "1-9 Hours Worked", "1-9",
                                     ifelse(HoursWorked == "10-19 Hours Worked", "10-19",
                                            ifelse(HoursWorked == "20-29 Hours Worked", "20-29",
                                                   ifelse(HoursWorked == "30-39 Hours Worked", "30-39",
                                                          ifelse(HoursWorked == "40-49 Hours Worked", "40-49", "50+")))))))


# create data frame containing the sex x age group proportions

PartneredProps <- File2AgeStructure %>%
  select(Sex, AgeGroup, UsualResidents, PartnershipStatus, HoursWorked) %>%
  group_by(Sex, AgeGroup, UsualResidents, PartnershipStatus, HoursWorked) %>%
  summarise(NumGroup = n()) %>%
  mutate(RelProps = NumGroup/sum(NumGroup),
         MidAge = ifelse(AgeGroup == "18 - 24 Years", 21, 
                ifelse(AgeGroup == "25 - 34 Years", 29.5,
                       ifelse(AgeGroup == "35 - 44 Years", 39.5,
                              ifelse(AgeGroup == "45 - 54 Years", 49.5,
                                     ifelse(AgeGroup == "55 - 64 Years", 59.5,
                                            ifelse(AgeGroup == "65 - 74 Years", 69.5,
                                                   ifelse(AgeGroup == "75 - 84 Years", 79.5,
                                                          ifelse(AgeGroup == "85 Years and Over" & Sex == "Male",
                                                                 92.5, 94.5)))))))),
         Bottom = ifelse(AgeGroup == "18 - 24 Years", 18, 
                         ifelse(AgeGroup == "25 - 34 Years", 25,
                                ifelse(AgeGroup == "35 - 44 Years", 35,
                                       ifelse(AgeGroup == "45 - 54 Years", 45,
                                              ifelse(AgeGroup == "55 - 64 Years", 55,
                                                     ifelse(AgeGroup == "65 - 74 Years", 65,
                                                            ifelse(AgeGroup == "75 - 84 Years", 75, 85))))))),
         Top = ifelse(AgeGroup == "18 - 24 Years", 24, 
                      ifelse(AgeGroup == "25 - 34 Years", 34,
                             ifelse(AgeGroup == "35 - 44 Years", 44,
                                    ifelse(AgeGroup == "45 - 54 Years", 54,
                                           ifelse(AgeGroup == "55 - 64 Years", 64,
                                                  ifelse(AgeGroup == "65 - 74 Years", 74,
                                                         ifelse(AgeGroup == "75 - 84 Years", 84,
                                                                ifelse(AgeGroup == "85 Years and Over" & Sex == "Male",
                                                                       100, 104))))))))) %>%
  filter(PartnershipStatus == "Partnered") %>%
  select(!NumGroup)

# split out by household size

PartneredProps2P <- PartneredProps %>%
  filter(UsualResidents == "Two Usual Residents")

AdditionalData2HH <- data.frame(Sex = c("Female", "Male", "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male",
                                        "Female", "Female","Male", "Male", "Female", "Female", "Male", "Male", "Female", "Female",
                                        "Male", "Male", "Female", "Female", "Male", "Male"),
                                
                             AgeGroup = c("15 - 17 Years", "15 - 17 Years", "35 - 44 Years", "75 - 84 Years", "45 - 54 Years",
                                          "85 Years and Over", "25 - 34 Years", "75 - 84 Years", "35 - 44 Years", "75 - 84 Years",
                                          "15 - 17 Years", "75 - 84 Years", "35 - 44 Years", "75 - 84 Years", "15 - 17 Years",
                                          "75 - 84 Years", "15 - 17 Years", "75 - 84 Years", "15 - 17 Years", "75 - 84 Years",
                                          "15 - 17 Years", "75 - 84 Years", "15 - 17 Years", "75 - 84 Years", "15 - 17 Years",
                                          "75 - 84 Years"),
                             
                             UsualResidents = c(rep("Two Usual Residents",26)),
                             HoursWorked = c("0", "0", "1-9", "1-9", "1-9", "1-9", "10-19", "10-19", "10-19", "10-19", "20-29",
                                             "20-29", "20-29", "20-29", "30-39", "30-39", "30-39", "30-39", "40-49", "40-49",
                                             "40-49", "40-49", "50+", "50+", "50+", "50+"),
                             
                             RelProps = c(rep(0,26)),
                             MidAge = c(17, 17, 44, 75, 54, 85, 34, 75, 44, 75, 17, 75, 44, 75, 17, 75, 17, 75, 17, 75, 17, 75,
                                        17, 75, 17, 75),
                             Bottom = c(17, 17, 44, 75, 54, 85, 34, 75, 44, 75, 17, 75, 44, 75, 17, 75, 17, 75, 17, 75, 17, 75,
                                        17, 75, 17, 75),
                             Top    = c(17, 17, 44, 75, 54, 85, 34, 75, 44, 75, 17, 75, 44, 75, 17, 75, 17, 75, 17, 75, 17, 75,
                                        17, 75, 17, 75),
                             PartnershipStatus = c(rep("Partnered",26)))


PartneredProps3P <- PartneredProps %>%
  filter(UsualResidents == "Three Usual Residents")

AdditionalData3HH <- data.frame(Sex = c("Female", "Female", "Male", "Female", "Female", "Female", "Female", "Female", "Female",
                                        "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female",
                                        "Female", "Male", "Male"),
                                
                                AgeGroup = c("15 - 17 Years", "85 Years and Over", "15 - 17 Years", "18 - 24 Years",
                                             "55 - 64 Years", "15 - 17 Years", "65 - 74 Years", "15 - 17 Years",
                                             "65 - 74 Years", "15 - 17 Years", "65 - 74 Years", "18 - 24 Years", "75 - 84 Years",
                                             "15 - 17 Years", "65 - 74 Years", "15 - 17 Years", "75 - 84 Years", "18 - 24 Years",
                                             "65 - 74 Years", "15 - 17 Years", "65 - 74 Years"),
                                
                                UsualResidents = c(rep("Three Usual Residents", 21)),
                                HoursWorked = c("0", "0", "0", "1-9", "1-9", "10-19", "10-19", "20-29", "20-29", "30-39",
                                                "30-39", "30-39", "30-39", "40-49", "40-49", "40-49", "40-49", "50+", "50+", "50+",
                                                "50+"),
                                
                                RelProps = c(rep(0, 21)),
                                MidAge = c(17, 85, 17, 24, 55, 17, 65, 17, 65, 17, 65, 24, 75, 17, 65, 17, 75, 24, 65, 17, 65),
                                Bottom = c(17, 85, 17, 24, 55, 17, 65, 17, 65, 17, 65, 24, 75, 17, 65, 17, 75, 24, 65, 17, 65),
                                Top    = c(17, 85, 17, 24, 55, 17, 65, 17, 65, 17, 65, 24, 75, 17, 65, 17, 75, 24, 65, 17, 65),
                                PartnershipStatus = c(rep("Partnered", 21)))
                                
  
PartneredProps4P <- PartneredProps %>%
  filter(UsualResidents == "Four Usual Residents")

AdditionalData4HH <- data.frame(Sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Female", "Female", "Female",
                                        "Female", "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male",
                                        "Female", "Female", "Male", "Male"),
                                
                                AgeGroup = c("15 - 17 Years", "75 - 84 Years", "15 - 17 Years", "75 - 84 Years", "18 - 24 Years",
                                             "55 - 64 Years", "18 - 24 Years", "55 - 64 Years", "18 - 24 Years", "55 - 64 Years",
                                             "15 - 17 Years", "55 - 64 Years", "18 - 24 Years", "65 - 74 Years", "15 - 17 Years",
                                             "65 - 74 Years", "15 - 17 Years", "65 - 74 Years", "15 - 17 Years", "55 - 64 Years",
                                             "15 - 17 Years", "65 - 74 Years"),
                                
                                UsualResidents = c(rep("Four Usual Residents", 22)),
                                HoursWorked = c("0", "0", "0", "0", "1-9", "1-9", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39",
                                                "30-39", "30-39", "40-49", "40-49", "40-49", "40-49", "50+", "50+", "50+", "50+"),
                                
                                RelProps = c(rep(0, 22)),
                                MidAge = c(17, 75, 17, 75, 24, 55, 24, 55, 24, 55, 17, 55, 24, 65, 17, 65, 17, 65, 17, 55, 17, 65),
                                Bottom = c(17, 75, 17, 75, 24, 55, 24, 55, 24, 55, 17, 55, 24, 65, 17, 65, 17, 65, 17, 55, 17, 65),
                                Top    = c(17, 75, 17, 75, 24, 55, 24, 55, 24, 55, 17, 55, 24, 65, 17, 65, 17, 65, 17, 55, 17, 65),
                                PartnershipStatus = c(rep("Partnered", 22)))                              
                                
                                
PartneredProps5P <- PartneredProps %>%
  filter(UsualResidents == "Five Usual Residents")

AdditionalData5HH <- data.frame(Sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Female", "Female", "Female", 
                                        "Female", "Female", "Female", "Male", "Male", "Male", "Male"),
                                
                                AgeGroup = c("15 - 17 Years", "55 - 64 Years", "18 - 24 Years", "55 - 64 Years", "18 - 24 Years",
                                             "55 - 64 Years", "18 - 24 Years", "55 - 64 Years", "18 - 24 Years", "55 - 64 Years",
                                             "15 - 17 Years", "55 - 64 Years", "15 - 17 Years", "65 - 74 Years", "18 - 24 Years",
                                             "65 - 74 Years"),
                                
                                UsualResidents = c(rep("Five Usual Residents", 16)),
                                HoursWorked = c("0", "0", "0", "0", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39", "40-49",
                                                "40-49", "40-49", "40-49", "50+", "50+"),
                                
                                RelProps = c(rep(0, 16)),
                                MidAge = c(17, 55, 24, 55, 24, 55, 24, 55, 24, 55, 17, 55, 17, 65, 24, 65),
                                Bottom = c(17, 55, 24, 55, 24, 55, 24, 55, 24, 55, 17, 55, 17, 65, 24, 65),
                                Top    = c(17, 55, 24, 55, 24, 55, 24, 55, 24, 55, 17, 55, 17, 65, 24, 65),
                                PartnershipStatus = c(rep("Partnered", 16)))       


PartneredProps6P <- PartneredProps %>%
  filter(UsualResidents == "Six Usual Residents")

AdditionalData6HH <- data.frame(Sex = c("Female", "Female", "Male", "Male", "Male", "Male"),
                                
                                AgeGroup = c("15 - 17 Years", "55 - 64 Years", "18 - 24 Years", "55 - 64 Years", "18 - 24 Years",
                                             "55 - 64 Years"),
                                
                                UsualResidents = c(rep("Six Usual Residents", )),
                                HoursWorked = c("0", "0", "40-49", "40-49", "50+", "50+"),
                                
                                RelProps = c(rep(0, )),
                                MidAge = c(17, 55, 24, 55, 24, 55),
                                Bottom = c(17, 55, 24, 55, 24, 55),
                                Top    = c(17, 55, 24, 55, 24, 55),
                                PartnershipStatus = c(rep("Partnered", )))    



InputInterpolate <- rbind(PartneredProps, AdditionalData2HH, AdditionalData3HH, AdditionalData4HH, AdditionalData5HH,
                          AdditionalData6HH)

TheGroups <- c("Sex", "UsualResidents", "HoursWorked")

CalcProps <- interdiff(InputInterpolate, "MidAge", "RelProps", "Bottom", "Top", TheGroups)


# add the age groups back in

CalcPropsGroups <- CalcProps %>%
  filter(Fits > 0) %>%
  mutate(AgeGroup = ifelse(between(Age,18, 24), "18 - 24 Years",
                           ifelse(between(Age, 25, 34), "25 - 34 Years",
                                  ifelse(between(Age, 35, 44), "35 - 44 Years",
                                         ifelse(between(Age, 45, 54), "45 - 54 Years",
                                                ifelse(between(Age, 55, 64), "55 - 64 Years",
                                                       ifelse(between(Age, 65, 74), "65 - 74 Years",
                                                              ifelse(between(Age, 75, 84), "75 - 84 Years",
                                                                     "85 Years and Over"))))))),
         PartnershipStatus = "Partnered")
















####################################################################################################
# apply the expected probabilities using the relstat function from PopulateR
####################################################################################################


RelDef <- c("Sex", "AgeGroup", "UsualResidents", "HoursWorked")
PropDataDef <- c("Sex", "Age", "UsualResidents", "HoursWorked")

File3RelStatusFixed<- relfix(File2AgeStructure, "ID", "Age", "PartnershipStatus", "Partnered", CalcPropsGroups, 
                                  "Fits", RelDef, PropDataDef, userseed = 2023)





####################################################################################################
####################################################################################################
# before and after graphs
####################################################################################################
####################################################################################################

# women in two-person households, working 0 hours per week

Women2pHHBefore <- File2AgeStructure %>%
  filter(Age > 17, Sex == "Female", UsualResidents == "Two Usual Residents", HoursWorked=="No Hours") %>%
  select(Sex, Age, PartnershipStatus, HoursWorked) %>%
  group_by(Sex, Age, PartnershipStatus) %>%
  summarise(NumGroup = n()) %>%
  mutate(RelProps = round(NumGroup/sum(NumGroup),1)) %>%
  filter(PartnershipStatus == "Partnered") 

Women2pHHAfter <- File3RelStatusFixed %>%
  filter(Age > 17, Sex == "Female", UsualResidents == "Two Usual Residents", HoursWorked=="0") %>%
  select(Sex, Age, PartnershipStatus, HoursWorked) %>%
  group_by(Sex, Age, PartnershipStatus) %>%
  summarise(NumGroup = n()) %>%
  mutate(RelProps = round(NumGroup/sum(NumGroup),1)) %>%
  filter(PartnershipStatus == "Partnered") 

# create data frame of all ages for the women

AllWomanAges <- data.frame(Age = c(seq(18,98, 1)))

WBefore <- left_join(AllWomanAges, Women2pHHBefore) %>%
  mutate(RelProps = ifelse(is.na(RelProps), 0, RelProps),
         NumGroup = ifelse(is.na(NumGroup), 0, NumGroup),
         Sex = "Female",
         PartnershipStatus = "Partnered",
         Timing = "Before")

WAfter <- left_join(AllWomanAges, Women2pHHAfter) %>%
  mutate(RelProps = ifelse(is.na(RelProps), 0, RelProps),
         NumGroup = ifelse(is.na(NumGroup), 0, NumGroup),
         Sex = "Female",
         PartnershipStatus = "Partnered",
         Timing = "After")


BothWomen <- bind_rows(WBefore, WAfter)


ggplot(BothWomen, aes(x=Age, y = RelProps, col = Timing)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     expand = c(0, 2)) 
  

ggplot(WBefore, aes(x=Age, y = RelProps)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     expand = c(0, 2)) 


ggplot(WAfter, aes(x=Age, y = RelProps)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     expand = c(0, 2)) 


ggplot(WBefore, aes(x=Age, y = NumGroup)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
                     expand = c(0, 2))


ggplot(WBAfter, aes(x=Age, y = NumGroup)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
                     expand = c(0, 2))


# note little improvement, retain the Age structure and ignore the relationship structure.