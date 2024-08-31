#clear workspace
#  rm(list = ls())

library(dplyr)
library(readxl)
library(readr)
library(ggplot2)


#############################################################
#############################################################
#############################################################
# What older children in which households?
#############################################################
#############################################################
#############################################################

# ----------------------------------------------------------------------------------------------
# Expected counts by age
# older ones to go into the two-person households (sole parents)
# and three person households (couples)
ChildPropsAtHome <- readRDS("PhDRData/ChildPropsAtHome.rds")
File4FinalPopulation <- readRDS("PhDRData/File4CorrectHrs.rds")


# split the counts for 60-72 year old kids between 2 and 3 person households

# do this by prop.

TwoPersonOldParents <- File4FinalPopulation %>%
  filter(Age > 74, UsualResidents == "Two Usual Residents")

ThreePersonOldParents <- File4FinalPopulation %>%
  filter(Age > 74, UsualResidents == "Three Usual Residents")

# ratio

1/(nrow(ThreePersonOldParents)/nrow(TwoPersonOldParents))

# for every 27 older children in a two person household there are 1 in a three person household

# how many older children?

OldestKids <- ChildPropsAtHome %>%
  filter(Age > 59)

sum(OldestKids$ExpectedNum)

# there are 22. 

table(TwoPersonOldParents$Age)

table(ThreePersonOldParents$Age)

FivePersonOldParents <- File4FinalPopulation %>%
  filter(Age >= 73, UsualResidents == "Five Usual Residents")

# we only have one 73-year-old and one 74-year-old, so 58, 59 limited to 2-person and 3-person households

TwoPersonOldishParents <- File4FinalPopulation %>%
  filter(between(Age, 50, 100), UsualResidents == "Two Usual Residents")

ThreePersonOldishParents <- File4FinalPopulation %>%
  filter(between(Age, 50, 100), UsualResidents == "Three Usual Residents")

FourPersonOldishParents <- File4FinalPopulation %>%
  filter(between(Age, 50, 100), UsualResidents == "Four Usual Residents")

FivePersonOldishParents <- File4FinalPopulation %>%
  filter(between(Age, 50, 100), UsualResidents == "Five Usual Residents")

table(TwoPersonOldishParents$Age)
table(ThreePersonOldishParents$Age)
table(FourPersonOldishParents$Age)
table(FivePersonOldishParents$Age)

# oldest age for parents, expected
# 2-person, all
# 3-person - 76
# 4-person - 62
# 5-person - 55

# no restriction for children aged 40 years and down.

# breakdown against childprops counts, expected counts in household sizes
# 62 and older only in 2-person households (parent 77 - male, 80 - female)
# 44-61, 1 to 3-person, rest to 2-person
# 41-43, 1 to 3-person, 1 to 4-person, rest to 2-person

# 40 and down, no restriction.

# see what happens when probs applied to counts.