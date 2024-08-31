
#  rm(list = ls())

library(dplyr)
library(ggplot2)

# Get the random seed file
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# get the population data

File15AllEducation <- readRDS("~/Sync/PhD/PhDMay2023/PhDRData/File15AllEducation.rds")

# how many rest homes?
# industry code is Q860100 "Aged Care Residential Services"

AgedCareHomes <- File15AllEducation %>%
  filter(IndCode == "Q860100") %>%
  group_by(Company) %>%
  summarise(NumAgedCare = n())

#number people in synthetic population employed across all
sum(AgedCareHomes$NumAgedCare)






##################################################################
##################################################################
# How many in rest homes? 2013 census
##################################################################
##################################################################

TABLECODE8084 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied dwelling type by age group of usual residents, for usual residents in occupied dwellings, 2013 Census (RC, TA)/TABLECODE8084_Data_24eaea71-6f0d-4a53-a539-cbf2aa438214.csv")

# remove all younger person rows
# these people aren't in rest homes
TABLECODE8084 <- TABLECODE8084 %>%
  filter(!Age.group %in% c("0-4 Years", "5-9 Years", "10-14 Years", "15-19 Years", "20-24 Years", "25-29 Years",
                           "30-34 Years", "35-39 Years")) %>%
  select(-Flags)


# # note: 0 in "private hospital" so remove all those rows
# 
# TABLECODE8084NoPrivateHosp <- TABLECODE8084 %>%
#   filter(!Occupied.dwelling.type == "Private hospital")
# 
# TABLECODE8084ResidentialAndCommunityCare <- TABLECODE8084 %>%
#   filter(Occupied.dwelling.type == "Residential and community care facilities")

TABLECODE8084ResidentialCareOnly <- TABLECODE8084 %>%
  filter(Occupied.dwelling.type == "Residential care for older people")


# what proportion are those aged 85+ ?
round((TABLECODE8084ResidentialCareOnly[11,4]/TABLECODE8084ResidentialCareOnly[1,4]),3)


# median number of residents per facility, assuming 600 max.

600/nrow(AgedCareHomes)












# fit a distribution given the percentiles in the table?
# from https://stackoverflow.com/a/14548887/1030648

cum.p <- c(10, 25, 50, 75, 90)/100
prob <- c( cum.p[1], diff(cum.p), .01)
x <- c(20, 33, 50, 73, 99)

freq <- 100000 # final output size that we want

# Extreme values beyond x (to sample)
init <- 0 
fin  <- abs(max(x))

ival <- c(init, x, fin) # generate the sequence to take pairs from
len <- 100 # sequence of each pair

s <- sapply(2:length(ival), function(i) {
  seq(ival[i-1], ival[i], length.out=len)
})
# sample from s, total of 10000 values with probabilities calculated above
set.seed(TheRandomSeeds[126])                                              #################### seed 126 
BedsVector <- sample(s, freq, prob=rep(prob, each=len), replace = T)
Beds10000 <- as.data.frame(BedsVector)
 
set.seed(TheRandomSeeds[127])                                              #################### seed 127 
BedsSample <- Beds10000 %>%
  mutate(Beds = round(BedsVector, 0)) %>%
  slice_sample(n=nrow(AgedCareHomes), replace = FALSE)

sum(BedsSample$Beds)

RestHomesWithBedCounts <- bind_cols(AgedCareHomes, BedsSample) %>%
  select(-BedsVector)

# below created too many residents, testing with different random seeds shows excessive variation

# # try again with rough numbers from figure 3.4 in the aged care report, 2013 numbers, this is NOT percentiles
# 
# FigureThreePointFour <- data.frame(Beds = c(4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 94.5, 109.5, 129.5, 149.5,
#                                             169.5),
#                                    Percentage = c(4.5, 5, 10.5, 12, 17.5, 12, 9.5, 7, 7, 4.5, 4, 2.5, 1.5, 1))
# 
# # create dataset of 10,000 points
# 
# FigureThreePointFourSample <- FigureThreePointFour %>%
#   mutate(Count = as.integer(((Percentage/100)*98.5)*10000))
# 
# FigureThreePointFourLong <- FigureThreePointFourSample %>%
#   tidyr::uncount(weights = Count)
# 
# # fit a log normal
# 
# BedsModel <- fitdistrplus::fitdist(FigureThreePointFourLong$Beds, "lnorm", method = "mle")
# summary(BedsModel)
# BedsMeanLog <- BedsModel[["estimate"]][["meanlog"]]
# BedsSDLog <- BedsModel[["estimate"]][["sdlog"]]
# 
# # draw a sample of 11
# set.seed(TheRandomSeeds[128])                                              #################### seed 128 
# AgedCareBedSizes <- round(rlnorm(n=nrow(AgedCareHomes), meanlog = BedsMeanLog, sdlog = BedsSDLog),0)
# 
# sum(AgedCareBedSizes)




##################################################################
##################################################################
# Create the rest home residents
##################################################################
##################################################################

# use weights from TABLECODE8084ResidentialCareOnly 

set.seed(TheRandomSeeds[128])                                              #################### seed 128 
AgeGroup = sample(c("55 - 59 Years", "60 - 64 Years", "65 - 69 Years", "70 - 74 Years", "75 - 79 Years", "80 - 84 Years", 
                    "85 Years and Over"),
                  size = 553, replace = TRUE, prob = c(6, 9, 18, 39, 54, 120, 297))

set.seed(TheRandomSeeds[129])                                                ######################## seed 129
Sex = sample(c("Female", "Male"), size = 553, replace = TRUE, prob=c(0.7,0.3))

# put them into a data frame

RestHomeResidents <- data.frame(AgeGroup, Sex) %>%
  mutate(MinAge = as.numeric(stringr::str_extract(AgeGroup, "[0-9]+")),
         MaxAge = as.numeric(ifelse(!(AgeGroup == "85 Years and Over"), 
                                    stringr::str_extract(AgeGroup, stringr::regex("(\\d+)(?!.*\\d)")), 
                                    104)))

# add age
TimaruDistrictSexAgeNoTotals <- readRDS("PhDRData/TimaruDistrictSexAgeNoTotals.rds")


RestHomeWithAge <- agedis(RestHomeResidents, "Sex", "MinAge", "MaxAge", TimaruDistrictSexAgeNoTotals, "Sex", "IntegerAge", 
                          "Value", "Age", 
                          userseed = TheRandomSeeds[130])            #################### seed 130 %%%%%%%%%%%%%%%%

RestHomePeople <- RestHomeWithAge %>%
  select(-c(MinAge, MaxAge))

# how many males versus females

table(RestHomePeople$Sex)

# another plot!

# cumulative distribution

AgesCumDist <- RestHomePeople %>%
  group_by(Sex, Age) %>%
  summarise(AgeCount = n()) %>%
  group_by(Sex) %>%
  dplyr::mutate(AgePercent=AgeCount/sum(AgeCount), CumAgePercent=cumsum(AgePercent))


RestHomeAgesPlot <- ggplot(AgesCumDist, aes(x=Age, y=CumAgePercent, colour = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("mediumorchid2",'blue'), name = "",
                     labels = c("Females", "Males")) +
  xlab("Age (years)") + ylab("Cumulative proportion") +
  scale_x_continuous(limits = c(50, 110), labels = c(50, 60, 70, 80, 90, 100, 110),
                     expand = c(0, 2)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = .2), expand = c(0, .01)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")
  
#  ggsave(RestHomeAgesPlot, file="~//Sync/PhD/ThesisVersions/Thesis2024/PDFs/RestHomeAgesPlot.pdf", width=9.32, height=7.78, units="in")









# add them into the population, need to create a resthome variable as per the ECE method

AgedCareEmployees <- File15AllEducation %>%
  filter(IndCode == "Q860100") %>%
  mutate(RestHomeIndicator = "Staff",
         RestHome = Company,
         HouseholdID = as.character(HouseholdID))

# expand the rest home beds so one bed per line

ResthomesIndivBeds <- RestHomesWithBedCounts[rep(seq(nrow(RestHomesWithBedCounts)), 
                                               RestHomesWithBedCounts$Beds), 1:3]


set.seed(TheRandomSeeds[131])                                                ######################## seed 131
RestHomesToAdd <- ResthomesIndivBeds %>%
  slice_sample(n=nrow(ResthomesIndivBeds), replace = FALSE)

# get the factor levels for Age group and Usual Residents


AgedCareResidents <- bind_cols(RestHomePeople, RestHomesToAdd) %>%
  rename(RestHome = Company) %>%
  mutate(RestHomeIndicator = "Resident",
         HouseholdID = RestHome,
         ID = 30000+row_number(),
         AgeGroup = ifelse(AgeGroup == "55 - 59 Years", "55 - 64 Years",
                           ifelse(AgeGroup == "60 - 64 Years", "55 - 64 Years", 
                                  ifelse(AgeGroup == "65 - 69 Years", "65 - 74 Years",
                                         ifelse(AgeGroup == "70 - 74 Years", "65 - 74 Years",
                                                ifelse(AgeGroup == "75 - 79 Years", "75 - 84 Years",
                                                       ifelse(AgeGroup == "80 - 84 Years", "75 - 84 Years",
                                                              AgeGroup)))))),
         AgeGroup = factor(AgeGroup, levels = c("0-4 Years", "5 -9 Years", "10 - 14 Years", "15 - 17 Years",
                                                "18 - 24 Years", "25 - 34 Years", "35 - 44 Years", 
                                                "45 - 54 Years", "55 - 64 Years", "65 - 74 Years", 
                                                "75 - 84 Years", "85 Years and Over")),
         PartnershipStatus = "Rest home resident",
         UsualResidents = "Rest home",
         HoursWorked = "No Hours",
         HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")),
         EducationStatus = "N",
         EducationStatus = ordered(EducationStatus, levels = c("N", "Y")),
         Type = "Rest home resident",
         SexMarker = ifelse(Sex == "Male", "M","F"),
         schoolID = "0",
         IndCode = "Not employed",
         IndName = "Not employed",
         Company = "Not employed",
         ECEIndicator = "None",
         ECEProvider = "None") %>%
  select(-c(NumAgedCare, Beds))
         
         
RestHomeCompleted <- bind_rows(AgedCareEmployees, AgedCareResidents) 


str(AgedCareEmployees)

str(RestHomeCompleted)

NoRestHomePeople <- File15AllEducation %>%
  filter(!ID %in% AgedCareEmployees$ID) %>%
  mutate(RestHomeIndicator = "None",
         RestHome = "None",
         HouseholdID = as.character(HouseholdID))

str(NoRestHomePeople)


File16SyntheticPopulation <- bind_rows(NoRestHomePeople, RestHomeCompleted)

str(File16SyntheticPopulation)

table(File16SyntheticPopulation$UsualResidents)

# save the final file
File16SyntheticPopulation <- File16SyntheticPopulation %>%
  mutate(Type = ifelse(Type == "Opposite sex no kids", "Opposite sex without child",
                       ifelse(Type == "Single sex no kids", "Same sex without child",
                              ifelse(Type == "Single sex with child", "Same sex with child", Type))))

saveRDS(File16SyntheticPopulation, file = "PhDRData/File16SyntheticPopulation.rds")

# what was percentage increase?

round(((nrow(File16SyntheticPopulation) - nrow(File15AllEducation))/nrow(File15AllEducation)),3)

# get table of rest home by resident count to check counts.

RestHomePeeps <- File16SyntheticPopulation %>%
  filter(RestHomeIndicator == "Resident") %>%
  group_by(RestHome) %>%
  summarise(NumResidents = n())

SummaryRestHomes <- left_join(RestHomesWithBedCounts, RestHomePeeps, by = c("Company" = "RestHome"))


