
#   rm(list = ls())

library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

####################################################################################################
####################################################################################################
# Create an age structure based on sex and age group
####################################################################################################
####################################################################################################

# set up x-axis tick mark function

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





####################################################################################################
####################################################################################################
# Bring in the Timaru age/sex structure counts from Census 2013
####################################################################################################
####################################################################################################

TimaruDistrictSexAge <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex/TABLECODE8001_Data_1746e3c7-2263-4fd5-9dc1-6aef89a0874d.csv")
str(TimaruDistrictSexAge)

# get counts by age for Timaru District, for 85+ years and plot to show decrease in numbers
# data extraction
TimaruDistrictElderlyAges <- TimaruDistrictSexAge %>%
  select(-"Flags") %>%
  filter(Age.group %in% c("85 years", "86 years", "87 years", "88 years", "89 years", "90 years",
                          "91 years", "92 years", "93 years", "94 years", "95 years", "96 years",
                          "97 years", "98 years", "99 years", "100 years", "101 years", "102 years",
                          "103 years", "104 years")) %>%
  mutate(Age = as.numeric(sub("([0-9]+).*$", "\\1", Age.group)))

TDElderlyAgesProp <- TimaruDistrictElderlyAges %>%
  group_by(Sex, Age) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=Value/sum(Value), CumAgePercent=cumsum(AgePercent))


custom_breaksTD <- seq(85, 106, 1) 

# plot
ElderlyAgesPlot <- ggplot(TDElderlyAgesProp, aes(x=Age, y=CumAgePercent, colour=Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(85, 106), breaks = custom_breaksTD,
                     labels = every_nth(custom_breaksTD, 5, inverse = TRUE),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#  ggsave(ElderlyAgesPlot, width=9.32, height=7.78, units="in", file="ElderlyAgesPlot.pdf")
# 9.32 x 7.78 image, as per default

rm(ElderlyAgesPlot, TDElderlyAgesProp, TimaruDistrictElderlyAges, custom_breaksTD)













####################################################################################################
####################################################################################################
# Clean Timaru age/sex structure counts from Census 2013
####################################################################################################
####################################################################################################
# remove extraneous columns - we already know it's Timaru District data for 2013 - and extraneous rows
# remove ages 105 and up as there are zero people that age
# remove all totals
patterns <- c("-", "Total", "Median", "105", "106", "107", "108", "109", "110", "111", "112",
              "113", "114", "115", "116", "117", "118", "119", "120", "over")
TimaruDistrictSexAgeNoTotals <- TimaruDistrictSexAge %>%
  select(Age.group, Sex, Value) %>%
  filter(!grepl(paste(patterns, collapse="|"), Age.group))
# check that we have the right number of ages, remember that <0 is included, so count should be 104+1, i.e. 104 per sex
nrow(TimaruDistrictSexAgeNoTotals)/2

#need to fix age so it is integer, not character
#remove " years" from those rows
TimaruDistrictSexAgeNoTotals$IntegerAge <- as.numeric(gsub("[^[:digit:]]", "", TimaruDistrictSexAgeNoTotals$Age))

# fix the years that are not digits, but are words
TimaruDistrictSexAgeNoTotals$IntegerAge <- ifelse(TimaruDistrictSexAgeNoTotals$Age=="Less than one year", 0,
                                                  ifelse(TimaruDistrictSexAgeNoTotals$Age=="One year",1,
                                                         ifelse(TimaruDistrictSexAgeNoTotals$Age=="Two years",2,
                                                                ifelse(TimaruDistrictSexAgeNoTotals$Age=="Three years",3,
                                                                       ifelse(TimaruDistrictSexAgeNoTotals$Age=="Four years",4,
                                                                              ifelse(TimaruDistrictSexAgeNoTotals$Age=="Five years",5,
                                                                                     ifelse(TimaruDistrictSexAgeNoTotals$Age=="Six years",6,
                                                                                            ifelse(TimaruDistrictSexAgeNoTotals$Age=="Seven years",7,
                                                                                                   ifelse(TimaruDistrictSexAgeNoTotals$Age=="Eight years",8,
                                                                                                          ifelse(TimaruDistrictSexAgeNoTotals$Age=="Nine years",9, TimaruDistrictSexAgeNoTotals$IntegerAge))))))))))

#delete factor age
TimaruDistrictSexAgeNoTotals$Age.group <- NULL

rm(patterns, TimaruDistrictSexAge)


# save the pyramid data
saveRDS(TimaruDistrictSexAgeNoTotals, file="PhDRData/TimaruDistrictSexAgeNoTotals.rds")










####################################################################################################
####################################################################################################
# Give the proportions for males aged 15-15 years, as per illustrative example in the thesis
####################################################################################################
####################################################################################################

ExampleData <- TimaruDistrictSexAgeNoTotals %>%
  filter(IntegerAge %in% c(15, 16, 17),
         Sex == "Male") %>%
  arrange(IntegerAge) %>%
  mutate(PropOfTotal = round(Value/sum(Value),3))

# what is the total?
sum(ExampleData$Value)
















####################################################################################################
####################################################################################################
# Prep synthetic population for sex/age structure to use agedis
####################################################################################################
####################################################################################################
# first number is minimum age
# second number is maximum age
# the final age group has a maximum age of 104

File2AllPeople <- readRDS("PhDRData/File2AllPeople.rds")

File3AgePrep <- File2AllPeople %>%
  mutate(MinAge = as.numeric(stringr::str_extract(AgeGroup, "[0-9]+")),
         MaxAge = as.numeric(ifelse(!(AgeGroup == "85 Years and Over"), 
                        stringr::str_extract(AgeGroup, stringr::regex("(\\d+)(?!.*\\d)")), 
                         104)))
# see https://stackoverflow.com/a/70665472 for extraction of last number

TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

File3PopulationAge <- agedis(File3AgePrep, "Sex", "MinAge", "MaxAge", TimaruDistrictSexAgeNoTotals, "Sex", "IntegerAge", 
                           "Value", "Age", userseed = TheRandomSeeds[58])            #################### seed 58 %%%%%%%%%%%%%%%%

File3PopulationAge <- File3PopulationAge %>%
  select(-c(MinAge, MaxAge))

# save the File3Population
saveRDS(File3PopulationAge, file = "PhDRData/File3PopulationAge.rds")













####################################################################################################
####################################################################################################
# Compare the synthetic population age/sex pyramid to the Timaru District one
####################################################################################################
####################################################################################################

####################################################################################################
# Set up the graph x-axis instructions
####################################################################################################



custom_breaks <- seq(0, 110, 10) 

####################################################################################################
# Compare the females
####################################################################################################

SyntheticWomen <- File3PopulationAge %>%
  filter(Sex == "Female") %>%
  group_by(Age) %>%
  summarise(AgeCount=length(Age)) %>%
  mutate(DataSource = "Synthetic data") %>%
  arrange(Age)

CensoredWomen <- TimaruDistrictSexAgeNoTotals %>%
  filter(Sex == "Female") %>%
  select(IntegerAge, Value) %>%
  rename(AgeCount = Value, Age = IntegerAge) %>%
  mutate(DataSource = "Timaru District") %>%
  arrange(Age)


# get the maximum age from both data frames

femaleMaxAgeSynthetic <- max(SyntheticWomen$Age)
femaleMaxAgePopulation <- max(CensoredWomen$Age)


# insert 0s into any missing ages between 0 and the max age 
# create dataframe of sequence of values from 0 to max age
femaleFullAgeDF <- data.frame(Age = seq(0, max(femaleMaxAgeSynthetic, femaleMaxAgePopulation)))

# add in the missing zeros to both female data frames
# Synthetic population
SyntheticWomenFull <-  SyntheticWomen %>%
  full_join(femaleFullAgeDF, by = "Age") %>%
  mutate(AgeCount = coalesce(AgeCount, 0),
         DataSource = coalesce(DataSource, "Synthetic data")) %>%
  arrange(Age)

# Pyramid data
CensoredWomenFull <-  CensoredWomen %>%
  full_join(femaleFullAgeDF, by = "Age") %>%
  mutate(AgeCount = coalesce(AgeCount, 0),
         DataSource = coalesce(DataSource, "Timaru District")) %>%
  arrange(Age)



# now do the plot comparison of the distribution

WomenAgeComparison <- rbind(SyntheticWomenFull, CensoredWomenFull)

WomenAgeComparison <- WomenAgeComparison %>%
  group_by(DataSource, Age) %>%
  group_by(DataSource) %>%
  dplyr::mutate(AgePercent=AgeCount/sum(AgeCount), CumAgePercent=cumsum(AgePercent))

# and plot
WomenAgesSynPop <- ggplot(WomenAgeComparison, aes(x=Age, y=CumAgePercent, colour=DataSource)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'gray60'), name = "",
                     labels = c("Synthetic data", "Timaru District data")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 110), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE),
                     expand = c(0, 2)
                     ) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

# ggsave(WomenAgesSynPop, width=9.32, height=7.78, units="in",, file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/WomenAgesSynPop.pdf")   
# 9.32 x 7.78 image


# remove the files for the females
rm(CensoredWomen, CensoredWomenFull,femaleFullAgeDF, SyntheticWomen, SyntheticWomenFull, WomenAgeComparison, 
   femaleMaxAgePopulation, femaleMaxAgeSynthetic, WomenAgesSynPop)












####################################################################################################
# Compare the males
####################################################################################################

SyntheticMen <- File3PopulationAge %>%
  filter(Sex == "Male") %>%
  group_by(Age) %>%
  summarise(AgeCount=length(Age)) %>%
  mutate(DataSource = "Synthetic data") %>%
  arrange(Age)

CensoredMen <- TimaruDistrictSexAgeNoTotals %>%
  filter(Sex == "Male") %>%
  select(IntegerAge, Value) %>%
  rename(AgeCount = Value, Age = IntegerAge) %>%
  mutate(DataSource = "Timaru District") %>%
  arrange(Age)


# the last 4 ages in the Timaru District have no counts
# remove them

CensoredMen <- CensoredMen %>%
  filter(Age <=100)

# get the maximum age from both data frames

maleMaxAgeSynthetic <- max(SyntheticMen$Age)
maleMaxAgePopulation <- max(CensoredMen$Age)


# insert 0s into any missing ages between 0 and the max age 
# create dataframe of sequence of values from 0 to max age
maleFullAgeDF <- data.frame(Age = seq(0, max(maleMaxAgeSynthetic, maleMaxAgePopulation)))

# add in the missing zeros to both female data frames
# Synthetic population
SyntheticMenFull <-  SyntheticMen %>%
  full_join(maleFullAgeDF, by = "Age") %>%
  mutate(AgeCount = coalesce(AgeCount, 0),
         DataSource = coalesce(DataSource, "Synthetic data")) %>%
  arrange(Age)

# Pyramid data
CensoredMenFull <-  CensoredMen %>%
  full_join(maleFullAgeDF, by = "Age") %>%
  mutate(AgeCount = coalesce(AgeCount, 0),
         DataSource = coalesce(DataSource, "Timaru District")) %>%
  arrange(Age)



# now do the plot comparison of the distribution

MenAgeComparison <- rbind(SyntheticMenFull, CensoredMenFull)

MenAgeComparison <- MenAgeComparison %>%
  group_by(DataSource, Age) %>%
  group_by(DataSource) %>%
  dplyr::mutate(AgePercent=AgeCount/sum(AgeCount), CumAgePercent=cumsum(AgePercent))

# and plot
MenAgesSynPop <- ggplot(MenAgeComparison, aes(x=Age, y=CumAgePercent, colour=DataSource)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue",'gray60'), name = "",
                     labels = c("Synthetic data", "Timaru District data")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(0, 110), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#  ggsave(MenAgesSynPop, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/MenAgesSynPop.pdf")   
# 9.32 x 7.78 image


# remove the files for the males
rm(MenAgesSynPop, CensoredMen, CensoredMenFull, maleFullAgeDF, MenAgeComparison, SyntheticMen, SyntheticMenFull, 
   custom_breaks, maleMaxAgePopulation, maleMaxAgeSynthetic, every_nth)


# get the maximum ages for males and females
maxMaleAge <- File3PopulationAge %>%
  filter(Sex == "Male") %>%
  mutate(MaxAge = max(Age)) %>%
  select(MaxAge) %>%
  distinct() %>%
  pull(MaxAge)
cat("Maximum male age is", maxMaleAge, "\n")

maxFemaleAge <- File3PopulationAge %>%
  filter(Sex == "Female") %>%
  mutate(MaxAge = max(Age)) %>%
  select(MaxAge) %>%
  distinct() %>%
  pull(MaxAge)
cat("Maximum female age is", maxFemaleAge, "\n")

rm(maxMaleAge, maxFemaleAge)

rm(list = ls())








####################################################################################################
# last random seed vector used was  #################### seed 58 %%%%%%%%%%%%%%%%
####################################################################################################