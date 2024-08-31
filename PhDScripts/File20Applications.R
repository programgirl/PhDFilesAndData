#clear workspace
#  rm(list = ls())

library(dplyr)
library(readxl)


# generation rnorm approximation for Gargiulo

GargSD3 <- rnorm(100000, mean = 2, sd = 3)
hist(GargSD3)
quantile(GargSD3, prob= c(0, .003, .025, .34, .50, .68, .95, .997, 1))


GargSD4 <- rnorm(100000, mean = 2, sd = 4)
hist(GargSD4)
quantile(GargSD4, prob= c(0, .003, .025, .34, .50, .68, .95, .997, 1)) # seems to be closest


####################################################################################################
####################################################################################################
# How many schools in the Timaru District?
####################################################################################################
####################################################################################################


# #Bring in 2013 school rolls data from the Ministry of Education
StudentRollsBySchool2013 <- read_excel("~/Sync/PhD/Ministry of Education files/School Rolls/2-Student-rolls-by-School_2010-2023.xlsx", 
                                       sheet = "2013", 
                                       col_types = c("text", "text", "numeric", "text", "text", "text", "skip", "skip", "text",
                                                     "text", "text", "text", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
                                                     "skip", "skip", "skip", "skip", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                                     "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip"), 
                                       skip = 3)


#remove all schools outside of Timaru District
TimaruStudentRolls <- StudentRollsBySchool2013 %>%
  rename(SchoolName = `School Name`) %>%
  filter(`TA with Auckland Local Board` == "Timaru District") %>%
  mutate(Total = as.numeric(Total),
         Female = as.numeric(Female),
         Male = as.numeric(Male),
         InternationalFeePaying = as.numeric(`International Fee Paying`),
         Age5 = as.numeric(`Age 5`),
         Age6 = as.numeric(`Age 6`),
         Age7 = as.numeric(`Age 7`),
         Age8 = as.numeric(`Age 8`),
         Age9 = as.numeric(`Age 9`),
         Age10 = as.numeric(`Age 10`),
         Age11 = as.numeric(`Age 11`),
         Age12 = as.numeric(`Age 12`),
         Age13 = as.numeric(`Age 13`),
         Age14 = as.numeric(`Age 14`),
         Age15 = as.numeric(`Age 15`),
         Age16 = as.numeric(`Age 16`),
         Age17 = as.numeric(`Age 17`),
         Age18 = as.numeric(`Age 18`),
         Age19 = as.numeric(`Age 19`),
         Age20 = as.numeric(`Age 20`),
         Age21 = as.numeric(`Age 21`),
         Age22Plus = as.numeric(`Age 22+`)) %>%
  select(-c(`Age 5`, `Age 6`, `Age 7`, `Age 8`, `Age 9`, `Age 10`, `Age 11`, `Age 12`, `Age 13`, `Age 14`, `Age 15`,
            `Age 16`, `Age 17`, `Age 18`, `Age 19`, `Age 20`, `Age 21`, `Age 21`, `Age 22+`)) %>%
  replace(is.na(.), 0)




