library("dplyr")
library("stringi")
library("ggplot2")
library(readr)
library(forcats)
library("readxl")

# bring in employer data
JOB09000 <- read_csv("~/Sync/PhD/Stats NZ downloaded files/Business/MB_Ind6_GUEC_Timaru Dist.csv")

# remove everything not 2013
JOB09000 <- JOB09000 %>%
  filter(Year == 2013)


# what meshblocks?
Meshblocks <- JOB09000 %>%
  group_by(Meshblock) %>%
  summarise(NumInMB = n())

# get the non-totals to see how many 3s there are
NumInMeshblocks <-JOB09000 %>%
  filter(!(is.na(Meshblock)), !(ANZSIC06_Industry=="Total"), !(Geo_Unit == 0))

prop.table(table(NumInMeshblocks$Geo_Unit))

# how many hospitals and rest homes?
