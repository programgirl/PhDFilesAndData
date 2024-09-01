#clear workspace
# rm(list = ls())

library(dplyr)
library(readxl)
library(forcats)
library(ggplot2)
# 
# ####################################################################################################
# ####################################################################################################
# # Create population from the data supplied by Statistics New Zealand
# ####################################################################################################
# ####################################################################################################

JOB_11634_CONZUL_ <- read_excel("/Stats NZ downloaded files/Population/JOB-11634 CONZUL .xlsx", 
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


# construct file without totals for expansion into individuals
NoTotals <- subset(JOB_11634_CONZUL, !(Sex =="Total" | AgeGroup =="Total" | PartnershipStatus == "Total" |
                                         UsualResidents == "Total" | HoursWorked == "Total")) 
saveRDS(NoTotals, file = "PhDRdata/NoTotals.rds")

# put the overall totals in another file
# don't want transcription errors
AllTotals <- subset(JOB_11634_CONZUL,(Sex =="Total" | AgeGroup =="Total" | PartnershipStatus == "Total" |
                                        UsualResidents == "Total" | HoursWorked == "Total"))
saveRDS(AllTotals, file = "PhDRdata/AllTotals.rds")

# check that subsetting worked
isTRUE(nrow(NoTotals) + nrow(AllTotals) == nrow(JOB_11634_CONZUL))


#remove zero and NA rows
KnownNums <- subset(NoTotals, !(Count == 0 | is.na(Count)))
# check counts
sum(KnownNums$Count)
# original dataset had 24,613 people
# this dataset contains 25,017 people

NAsOnly <- subset(NoTotals, is.na(Count))

# DATA CHECKS
OnlyZeros <- subset(NoTotals, Count == 0)
nrow(NoTotals) - (nrow(KnownNums) +nrow(NAsOnly)) #identify the number of combinations that do not exist, i.e. have 0 people

# #create one row per observation
File1LongData <- KnownNums[rep(seq(nrow(KnownNums)), KnownNums$Count), 1:5]

#forgot R retains factor levels no longer in use after subsetting, stupid thing!
# need to remove the Totals factor levels
File1LongData <- droplevels(File1LongData)

# make the HoursWorked factor ordered

File1LongData <- File1LongData  %>%
  mutate(HoursWorked = ordered(HoursWorked, levels = c("No Hours", "1-9 Hours Worked", "10-19 Hours Worked",
                                                       "20-29 Hours Worked", "30-39 Hours Worked",
                                                       "40-49 Hours Worked", "50 Hours or More Worked")))


# do not need to add in random order due to use of the functions, which randomise because of the random sampling and log-likelihood functions
# add in random sort order column to stop effect of ordered wide-to-long mutation
# set.seed(123)
# File1LongDataRandomised <- LongData[sample(nrow(File1LongData)),]
File1LongData$ID <- as.integer(seq_len(nrow(File1LongData)))

saveRDS(File1LongData, file = "PhDRdata/File1LongData.rds")



####################################################################################################
# get proportion of people who are working in each sex/age group
####################################################################################################
# subset from 15 years, retain total in age/sex and total for no hours for the age/sex group
# gsub removes the "Years" part of the agegroup string, including the leading space - need fewer characters for plotting
NotWorkingCounts <- AllTotals %>%
  subset(!(Sex=="Total") & !(AgeGroup %in% c("0 - 4 Years", "5 - 9 Years", "10 - 14 Years", "Total")) 
         & PartnershipStatus=="Total" & UsualResidents=="Total" & HoursWorked %in% c("No Hours", "Total")) %>%
  select(Sex, AgeGroup, HoursWorked, Count) %>%
  tidyr::pivot_wider(names_from = HoursWorked, values_from = Count) %>%
  mutate(PropWorking = 1-(`No Hours` / Total),
         AgeGroup = gsub(" Years", "", AgeGroup),
         AgeGroup = ifelse(AgeGroup == "15 - 17", "15-17",
                           ifelse(AgeGroup == "18 - 24", "18-24",
                                  ifelse(AgeGroup == "25 - 34", "25-34",
                                         ifelse(AgeGroup == "35 - 44", "35-44",
                                                ifelse(AgeGroup == "45 - 54", "45-54",
                                                       ifelse(AgeGroup == "55 - 64", "55-64",
                                                              ifelse(AgeGroup == "65 - 74", "65-74",
                                                                     ifelse(AgeGroup == "75 - 84", "75-84", "85 +"))))))))
  )


# plot the proportion working by sex and age
# yes, data frame is weirdly worded but it took the not working, NOT the working, to reduce random rounding effects

PropWorkingPlot <- ggplot(NotWorkingCounts, aes(x=AgeGroup, y=PropWorking)) +
  geom_bar(stat = "identity", position=position_dodge(), aes(fill = Sex)) +
  scale_fill_manual(values=c("mediumorchid2",'blue')) +
  xlab("Age group (years)") + ylab("Proportion of people working") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PropWorkingPlot, width=9.32, height=7.78, units="in", file="~/Sync/PhD/Thesis2020/PDFs/PropWorkingPlot.pdf")
# 9.32 x 7.78 image, as per default

rm(AllTotals, JOB_11634_CONZUL, JOB_11634_CONZUL_, KnownNums, NAsOnly, NoTotals, OnlyZeros, File1LongDataOriginal,
   NotWorkingCounts, PropWorkingPlot)