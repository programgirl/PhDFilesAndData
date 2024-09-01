#clear workspace
#  rm(list = ls())

library(dplyr)
library(readxl)
library(forcats)
library(ggplot2)

File4CorrectHrs <- readRDS("PhDRData/File4CorrectHrs.rds")

##############################################################################################
##############################################################################################
# Separate into households and give one-person households their household numbers
##############################################################################################
#############################################################################################
File7OnePersonHH <- File4CorrectHrs %>%
  filter(UsualResidents =="One Usual Resident") %>%
  mutate(HouseholdID = row_number(),
         Type="Alone")

# nothing else to do so save file
# save the synthetic population file
saveRDS(File7OnePersonHH, file = "PhDRData/InterimHouseholdSizeWork/File7OnePerson/File7OnePersonHH.rds")





##############################################################################################
# Get some basic stats
##############################################################################################
table(File7OnePersonHH$Sex)
round(prop.table(table(File7OnePersonHH$Sex)),2)





##############################################################################################
# split into male and female datasets for basic stats by sex
OnePersonF <- File7OnePersonHH %>%
  filter(Sex == "Female")

OnePersonM <- File7OnePersonHH %>%
  filter(Sex == "Male")

# min, max, and median ages
# min
min(OnePersonF$Age)
min(OnePersonM$Age)

# median
median(OnePersonF$Age)
median(OnePersonM$Age)

# max
max(OnePersonF$Age)
max(OnePersonM$Age)



# employment
round(prop.table(table(OnePersonF$HoursWorked)),2)
round(prop.table(table(OnePersonM$HoursWorked)),2)


# employment by age group
round(prop.table(table(OnePersonF$HoursWorked, OnePersonF$AgeGroup)),2)
round(prop.table(table(OnePersonM$HoursWorked, OnePersonM$AgeGroup)),2)

# Age group structure
round(prop.table(table(OnePersonF$AgeGroup)),4)
round(prop.table(table(OnePersonM$AgeGroup)),4)







# plot the age distribution

FullAgeDF <- data.frame(Age = c(seq(18, 104, by = 1)))

# count at each age
OnePersonFCounts <- OnePersonF %>%
  group_by(Age) %>%
  summarise(Count = n())
#
OnePersonMCounts<- OnePersonM %>%
  group_by(Age) %>%
  summarise(Count = n())

FemalesOnePerson <- full_join(FullAgeDF, OnePersonFCounts) %>%
  replace(is.na(.), 0) %>%
  mutate(CumProp = cumsum(Count)/sum(Count),
         Sex = "Female")

MalesOnePerson <- full_join(FullAgeDF, OnePersonMCounts) %>%
  replace(is.na(.), 0) %>%
  mutate(CumProp = cumsum(Count)/sum(Count),
         Sex = "Male")


OnePersonAllAges<- bind_rows(FemalesOnePerson, MalesOnePerson)


# plot the counts
# use cumulative density curves as histogram bars will be overplotted
OnePersonAllPlot <- ggplot(OnePersonAllAges, aes(x=Age, y=CumProp, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(10, 110), breaks = seq(10, 110, by =10),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#  ggsave(OnePersonAllPlot, width=9.32, height=7.78, units="in", file="OnePersonAllPlot.pdf")

# prop sex by hours worked

AgesPropHours <- File7OnePersonHH %>%
  group_by(Sex, HoursWorked) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(HoursPercent=Count/sum(Count),
                HoursWorked = ifelse(!HoursWorked %in% c("No Hours", "50 Hours or More Worked"), 
                                     gsub('.{13}$', '', HoursWorked), 
                                     ifelse(HoursWorked == "No Hours", "None", "50+")),
                HoursWorkedOrd = factor(HoursWorked, levels = c("None", "1-9", "10-19", "20-29", "30-39",
                                                                "40-49", "50+")))



WorkingHours1P<- ggplot(AgesPropHours, aes(x=HoursWorkedOrd, y=HoursPercent, fill=Sex)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Hours worked per week") + ylab("Proportion") +
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0, 0.7, by = 0.1), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#  ggsave(WorkingHours1P, width=9.32, height=7.78, units="in", file="WorkingHours1P.pdf")


# clean up
rm(list = ls())

