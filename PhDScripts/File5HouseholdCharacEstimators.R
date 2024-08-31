#clear workspace
#  rm(list = ls())

library(dplyr)
library(readxl)
library(readr)
library(ggplot2)

#############################################################
#############################################################
#############################################################
# Households work estimates
#############################################################
#############################################################
#############################################################

#############################################################
# Households
#############################################################


HouseholdType <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Household composition for households/TABLECODE8165_Data_HouseholdCompositionforHouseholds.csv", col_types = cols(Flags = col_skip()))

HouseholdTotals <- HouseholdType %>%
  filter(grepl('Total', `Household composition`)) 

NumHouseholds <- as.numeric(HouseholdTotals[1,4])

HouseholdNoTotals <- HouseholdType %>%
  filter(!(grepl('Total', `Household composition`)) & !(grepl('Average', `Household composition`))) %>%
  mutate(Proportion = Value/NumHouseholds) %>%
  arrange(desc(Proportion))

HouseholdTotals <- HouseholdTotals %>%
  mutate(Proportion = Value/NumHouseholds)

# what makes up the one-family households?
FamilyHouseholds <- as.numeric(HouseholdNoTotals[1,5] + HouseholdNoTotals[3,5] + HouseholdNoTotals[4,5])

# adding in the one-person households
FamilyAndOneHouseholds <- FamilyHouseholds + as.numeric(HouseholdNoTotals[2,5])

ParentsWithKids <- as.numeric(HouseholdNoTotals[3,4] + HouseholdNoTotals[4,4])

AllHouseholdsWithKids <- as.numeric(HouseholdNoTotals[3,4] + HouseholdNoTotals[4,4] + HouseholdNoTotals[8,4] +
                                      HouseholdNoTotals[9,4] + HouseholdNoTotals[12,4] + HouseholdNoTotals[13,4] +
                                      HouseholdNoTotals[14,4])

HouseholdWithKidsButNotOneFamily <- as.numeric(HouseholdNoTotals[8,4] + HouseholdNoTotals[9,4] + 
                                                 HouseholdNoTotals[12,4] + HouseholdNoTotals[13,4] +
                                                 HouseholdNoTotals[14,4])

PropHouseholdWithKidsButNotOneFamily <- HouseholdWithKidsButNotOneFamily/NumHouseholds



rm(HouseholdNoTotals, FamilyHouseholds, FamilyAndOneHouseholds, ParentsWithKids,
   AllHouseholdsWithKids, HouseholdWithKidsButNotOneFamily, PropHouseholdWithKidsButNotOneFamily)




















#############################################################
# Families
#############################################################

FamilyTypes <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type with type of couple/TABLECODE8160_Data_c9558eb4-30f9-4edc-9ad5-c091ac76f6d1.csv", col_types = cols(Flags = col_skip()))

TotalFamilyHouseholds <- as.numeric(HouseholdTotals[2,4] + HouseholdTotals[3,4])

DiffBetweenTotalsProp <- (as.numeric(FamilyTypes[1,3]) - TotalFamilyHouseholds) / TotalFamilyHouseholds
DiffBetweenTotals <- (as.numeric(FamilyTypes[1,3]) - TotalFamilyHouseholds)

NumFamilies <- as.numeric(FamilyTypes[1,3])

FamilyTotals <- FamilyTypes %>%
  filter(grepl('Total', `Family type with type of couple`)) %>%
  mutate(Proportion = Value/NumFamilies)



FamiliesNoTotals <- FamilyTypes %>%
  filter(!(grepl('Total', `Family type with type of couple`))) %>%
  mutate(Proportion = Value/NumFamilies) %>%
  arrange(desc(Proportion))

CouplesFamilies <- as.numeric(FamilyTotals[2,3] + FamilyTotals[3,3])

PropCouplesFamilies <- CouplesFamilies / NumFamilies

FamiliesWithKids <- as.numeric(FamiliesNoTotals[2,3] + FamiliesNoTotals[3,3] + FamiliesNoTotals[6,3] +
                                 FamiliesNoTotals[7,3])

PropFamiliesWithKids <- FamiliesWithKids/NumFamilies

PropSoleParentFamilies <- as.numeric(FamiliesNoTotals[3,3]) / NumFamilies




rm(FamiliesNoTotals, FamilyTotals, FamilyTypes, HouseholdTotals, HouseholdType, CouplesFamilies,
   DiffBetweenTotals, DiffBetweenTotalsProp, FamiliesWithKids, NumFamilies, NumHouseholds,
   PropCouplesFamilies, PropFamiliesWithKids, PropSoleParentFamilies, TotalFamilyHouseholds)












#############################################################
# Families by child dependency status
#############################################################

ChildDepend <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type by child dependency status/TABLECODE8141_Data_da61e1a1-c19c-41a8-ac22-c025640ce237.csv", col_types = cols(Flags = col_skip()))

ChildDependTotals <- ChildDepend %>%
  filter(grepl('Total', `Family type by child dependency status`)) 

FamilyCount <- as.numeric(ChildDependTotals[1,4])

ChildDependNoTotals <- ChildDepend %>%
  filter(!(grepl('Total', `Family type by child dependency status`))) %>%
  mutate(Proportion = Value/FamilyCount) 

CountMixedAndUnknown <- as.numeric(ChildDependNoTotals[4,4] + ChildDependNoTotals[5,4] + ChildDependNoTotals[6,4]
                                   + ChildDependNoTotals[7,4] + ChildDependNoTotals[8,4] +
                                     ChildDependNoTotals[11,4] + ChildDependNoTotals[12,4] +
                                     ChildDependNoTotals[13,4] + ChildDependNoTotals[14,4] +
                                     ChildDependNoTotals[15,4])

PropMixedAndUnknown <- CountMixedAndUnknown / FamilyCount


rm(ChildDepend, ChildDependNoTotals, ChildDependTotals, CountMixedAndUnknown, FamilyCount,
   PropMixedAndUnknown)
























#############################################################
# Sole parents
#############################################################

SoleParents <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Sex of sole parent with dependent children families in occupied private dwellings/TABLECODE8153_Data_eb5ff063-cb1d-483e-9d7e-32ae56967124.csv", 
                        col_types = cols(Flags = col_skip()))

SoleParentsTotal <- SoleParents %>%
  filter(grepl('Total', `Sex of sole parent`))

# NumSoleParents <- as.numeric(SoleParentsTotal[1,4]) # use of this doesn't let the proportions sum to 1

NumSoleParents <- sum(SoleParents[2,4], SoleParents[3,4])

SoleParentsNoTotals <- SoleParents %>%
  filter(!(grepl('Total', `Sex of sole parent`))) %>%
  mutate(Proportion = Value/NumSoleParents) %>%
  arrange(desc(Proportion))


rm(SoleParents, SoleParentsTotal, NumSoleParents, SoleParentsNoTotals)











#############################################################
# Grandparents
#############################################################

Grandparents <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Family type for grandparents in a parental role in families 2013 census/TABLECODE8158_Data_5d605f25-7a1f-4cbd-b850-189ddedf8f2e.csv", 
                         col_types = cols(Flags = col_skip()))


rm(Grandparents)



























































# #############################################################
# # Couples draws
# #############################################################
# #############################################################
# # note: commented out in thesis but can be added in if needed
# #############################################################
# 
# # use skew normal, use a mean equivalent of 2.5
# 
# ggplot() +
#   stat_function(fun = sn::dsn, args = list(xi=2.5, omega=7, alpha=2.5), 
#                 aes(color = "\U003C9 = 7"), linewidth = 1.5) +
#   stat_function(fun = sn::dsn, args = list(xi=2.5, omega=8, alpha=2.5), 
#                 aes(color = "\U003C9 = 8"), linewidth = 1.5) +
#   stat_function(fun = sn::dsn, args = list(xi=2.5, omega=9, alpha=2.5),
#                 aes(color = "\U003C9 = 9"), linewidth = 1.5) +
#   labs(x = "Age difference, male - female", y = "Proportion of couples") +
#   scale_colour_manual("Skew normal, \U003BE = 2.5, \U003B1 = 2.5",
#                       values = c("\U003C9 = 7" = "#1B9E77", "\U003C9 = 8" = "#D95F02", 
#                                  "\U003C9 = 9" = "#7570B3")) +
#   scale_x_continuous(breaks = c(-20, -10, 0, 10, 20, 30),
#                      limits = c(-20, 30)) +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text=element_text(size=18) ,
#         text = element_text(size = 20),
#         legend.position = "bottom") +
#   guides(colour = guide_legend(override.aes = list(size=2)))
# 
# # note: save as pdf via the console. Use (Device Size) as the size, tick LANDSCAPE, MUST USE CAIRO_PDF
# # OTHERWISE THE GREEK LETTERS WON'T PRINT AND THERE WILL BE ... INSTEAD.
# 
# # save this as "~/Sync/PhD/Thesis2020/PDFs/CplAgeDffOriginal.pdf" and 10.9 x 7.78 image
# 
# 
# 
# # decrease xi so that the median values are in the 0-5 range
# ggplot() +
#   stat_function(fun = sn::dsn, args = list(xi=-2, omega=7, alpha=2.5), 
#                 aes(color = "\U003C9 = 7"), linewidth = 1.5) +
#   stat_function(fun = sn::dsn, args = list(xi=-2, omega=8, alpha=2.5), 
#                 aes(color = "\U003C9 = 8"), linewidth = 1.5) +
#   stat_function(fun = sn::dsn, args = list(xi=-2, omega=9, alpha=2.5),
#                 aes(color = "\U003C9 = 9"), linewidth = 1.5) +
#   labs(x = "Age difference, male - female", y = "Proportion of couples") +
#   scale_colour_manual("Skew normal, \U003BE = -2, \U003B1 = 2.5",
#                       values = c("\U003C9 = 7" = "#1B9E77", "\U003C9 = 8" = "#D95F02", 
#                                  "\U003C9 = 9" = "#7570B3")) +
#   scale_x_continuous(breaks = c(-20, -10, 0, 10, 20, 30),
#                      limits = c(-20, 30)) +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text=element_text(size=18) ,
#         text = element_text(size = 20),
#         legend.position = "bottom") +
#   guides(colour = guide_legend(override.aes = list(size=2)))
# 
# # save this as "~/Sync/PhD/Thesis2020/PDFs/CplAgeDiffs0.pdf" and 10.9 x 7.78 image
# 
# # get probabilities of extreme values, i.e. greater than 15
# 1-sn::psn(15, xi=-2, omega=7, alpha=2.5)
# 1-sn::psn(15, xi=-2, omega=8, alpha=2.5)
# 1-sn::psn(15, xi=-2, omega=9, alpha=2.5)
# 
# # and the lower values, i.e. less than -5
# sn::psn(-5, xi=-2, omega=7, alpha=2.5)
# sn::psn(-5, xi=-2, omega=8, alpha=2.5)
# sn::psn(-5, xi=-2, omega=9, alpha=2.5)
































#############################################################
#############################################################
# Parents
#############################################################
#############################################################





#############################################################
# Mothers
#############################################################

Mothersraw <- read_excel("~/Sync/PhD/Stats NZ downloaded files/Children/JOB-09626 CONZUL rerun.xlsx",
                                     sheet = "Table 1 rerun", range = "a9:q70", na = "..C")

# number of NAs by geographic unit
Mothersraw %>%
  dplyr::select(-c(`Total 1`, `No Matches`, `Multiple Matches`, `Total 2`)) %>%
  filter(!(is.na(`Age of Child`)), `Age of Child` != "Total") %>%
  group_by(Area) %>%
  summarise_all(~sum(is.na(.))) %>%
  transmute(Area, sumNA = rowSums(.[-1]))

# true zeros:
13+12+11+10+9+8+7+6+5+4+3+2+1

# data loss estimates for Timaru Urban Area
TotalLessThan18 <- as.numeric(Mothersraw[16,3])
Total65AndOver <- as.numeric(Mothersraw[16,13])
TotalKids <- as.numeric(Mothersraw[16,14])

# Prop deleted from estimations
(TotalLessThan18+Total65AndOver) / TotalKids



LongChildGuardianFileTUA <- Mothersraw %>%
  tidyr::gather(MotherAgeGroup, RRCount, `Less than 18 Years`:`65 Years and Over`, factor_key=TRUE) %>%
  filter(Area == "Timaru Urban Area", MotherAgeGroup != "Less than 18 Years", MotherAgeGroup != "65 Years and Over",
         `Age of Child` != "Total", RRCount !=0) %>%
  dplyr::select(-c("Total 1", "No Matches", "Multiple Matches", "Total 2")) %>%
  mutate(RRCount = tidyr::replace_na(RRCount, 0),
         YoungestMotherAge = as.numeric(sub(".*?(\\d+).*", "\\1", MotherAgeGroup)),
         OldestMotherAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", MotherAgeGroup)),
         OldestMotherAge = ifelse(YoungestMotherAge == 65, 69, OldestMotherAge),
         YoungestChildAge = as.numeric(sub(".*?(\\d+).*", "\\1", `Age of Child`)),
         OldestChildAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", `Age of Child`)),
         CountMotherAges = OldestMotherAge - YoungestMotherAge + 1 ,
         CountChildAges = OldestChildAge - YoungestChildAge + 1,
         ChildCountDistribute = RRCount / (CountMotherAges * CountChildAges)) 


#############################################################
# create the counts by mother age

for(i in 1:nrow(LongChildGuardianFileTUA)) {
  
  MinChildAge <- LongChildGuardianFileTUA$YoungestChildAge[i]
  MaxChildAge <- LongChildGuardianFileTUA$OldestChildAge[i]
  MinMotherAge <- LongChildGuardianFileTUA$YoungestMotherAge[i]
  MaxMotherAge <- LongChildGuardianFileTUA$OldestMotherAge[i]
  
  cat("Child min is", MinChildAge, "max is", MaxChildAge, "mother min is", MinMotherAge, "mother max is", MaxMotherAge, "\n")
  
  GridOfAges <- tidyr::expand_grid(childages = MinChildAge:MaxChildAge,
                                   motherages = MinMotherAge:MaxMotherAge) %>%
    mutate(AgeDiff = motherages - childages) %>%
    filter(between(AgeDiff, 18, 64))
  
  # cat("minimum childbirth age is", min(GridOfAges$AgeDiff), "\n")
  
  GridOfAges <- as.data.frame(GridOfAges %>%
                                mutate(CountPerCell = LongChildGuardianFileTUA$RRCount[i]/nrow(GridOfAges)))
  
  # return(GridOfAges)
  
  if(exists("LongCountsTUA")) {
    LongCountsTUA <- bind_rows(LongCountsTUA, GridOfAges)
  } else {
    LongCountsTUA <- GridOfAges
  }
  
}

FinalLongCounts <- LongCountsTUA
rm(GridOfAges, LongCountsTUA)


FinalMotherCountsTUA <- FinalLongCounts %>%
  group_by(AgeDiff) %>%
  summarise(NumMums = round(sum(CountPerCell),0))

MotherAgesLongTUA <- FinalMotherCountsTUA %>%        # gives the number of synthetic ages at childbirth
  tidyr::uncount(weights = NumMums)

table(MotherAgesLongTUA$AgeDiff)

# plot the age distribution
# use one bin for each age, therefore 32 bins
MotherAgeDistributionTUA <- ggplot(MotherAgesLongTUA, aes(x=AgeDiff)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), color="white", fill="mediumorchid2", bins=32) +
  labs(x="Age of mother at childbirth (years)", y = "Proportion of mothers") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(MotherAgeDistributionTUA, file="~/Sync/PhD/Thesis2023/PDFs/MotherAgeDistributionTUA.pdf", width=10, height=6, units="in")

# get median age
median(MotherAgesLongTUA$AgeDiff)





#############################################################
# fit distributions
#############################################################

NumMothersTUA <- as.numeric(nrow(MotherAgesLongTUA))
LogNTUA <- log(nrow(MotherAgesLongTUA))




#############################################################
# skew normal
MotherAgeModelSNTUA <- sn::selm(AgeDiff ~ 1, data=MotherAgesLongTUA)
MotherParametersSNTUA <- sn::extractSECdistr(MotherAgeModelSNTUA)
MotherXiTUA <- MotherParametersSNTUA@dp[1]
MotherOmegaTUA <- MotherParametersSNTUA@dp[2]
MotherAlphaTUA <- MotherParametersSNTUA@dp[3]

# compare AIC, note this information is in the skew normal model summary
round(AIC(MotherAgeModelSNTUA),0)

# do AIC manually
KValueSNTUA <- as.numeric(3)
round(AICSNTUA <- as.numeric((-2* MotherAgeModelSNTUA@logL)+2*3))

# do BIC manually as not present in model
round(BICSNTUA <-as.numeric((KValueSNTUA*LogNTUA)-(2*MotherAgeModelSNTUA@logL)),0)








#############################################################
# log normal
MotherAgeModelLNTUA <- fitdistrplus::fitdist(MotherAgesLongTUA$AgeDiff, "lnorm", method = "mle")
summary(MotherAgeModelLNTUA)
MotherMeanLogTUA <- MotherAgeModelLNTUA[["estimate"]][["meanlog"]]
MotherSDLogTUA <- MotherAgeModelLNTUA[["estimate"]][["sdlog"]]

# do AIC manually
KValueLNTUA <- as.numeric(2)
LogLikeLNTUA <- as.numeric(MotherAgeModelLNTUA[6])
round(AICLNTUA <- as.numeric((-2*LogLikeLNTUA)+2*KValueLNTUA),0)

# do BIC manually
round(BICLNTUA <- as.numeric((KValueLNTUA*LogNTUA)-(2*LogLikeLNTUA)),0)






#############################################################
# Pearsons
PearsonsMSCTUA <- PearsonDS::pearsonFitML(MotherAgesLongTUA$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))


PearsonsMSCTUA <- PearsonDS::pearsonMSC(MotherAgesLongTUA$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMSCTUA

PearsonsMSCTUA$Best$AIC  # best model
round(PearsonsMSCTUA$MSCs[2,2],0) # AIC for best model
round(PearsonsMSCTUA$MSCs[4,2],0) # BIC for best model









#############################################################
# overplot the 3 distributions

MAACDistributionsTUA <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = MotherAgesLongTUA,
           color = "grey47", fill = "white") +
  stat_function(fun = dlnorm, args = list(meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA), 
                aes(color = "Log-normal"), linewidth = 2) +
  stat_function(fun = sn::dsn, args = list(xi=MotherXiTUA, omega=MotherOmegaTUA, alpha=MotherAlphaTUA), 
                aes(color = "Skew-normal"), linewidth = 2) +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = PearsonsMSCTUA$Best$ML[["a"]], 
                                                        b = PearsonsMSCTUA$Best$ML[["b"]],
                                                        location = PearsonsMSCTUA$Best$ML[["location"]], 
                                                        scale =PearsonsMSCTUA$Best$ML[["scale"]]),
                aes(color = "4-parameter beta"), linewidth = 2) +
  labs(x = "Mother age at childbirth", y = "Proportion of mothers") +
  scale_colour_manual("Distribution",
                      values = c("Log-normal" = "#1B9E77", "Skew-normal" = "#D95F02", 
                                 "4-parameter beta" = "#7570B3")) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70),
                     limits = c(0, 70)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(MAACDistributionsTUA, file="~/Sync/PhD/Thesis2023/PDFs/MAACDistributionsTUA.pdf", width=10, height=6, units="in")





# get the extreme values

# skew normal
sn::qsn(1/100000, xi = MotherXiTUA, omega = MotherOmegaTUA, alpha = MotherAlphaTUA)
100*(sn::psn(17, xi = MotherXiTUA, omega = MotherOmegaTUA, alpha = MotherAlphaTUA))
median(sn::rsn(100000, xi = MotherXiTUA, omega = MotherOmegaTUA, alpha = MotherAlphaTUA))
sn::qsn(1-(1/100000), xi = MotherXiTUA, omega = MotherOmegaTUA, alpha = MotherAlphaTUA)
100*(1-(sn::psn(50, xi = MotherXiTUA, omega = MotherOmegaTUA, alpha = MotherAlphaTUA)))


# log normal
qlnorm(1/100000, meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA)
100*(plnorm(17, meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA))
median(rlnorm(100000, meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA))
qlnorm(1-(1/100000), meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA)
100*(1-(plnorm(50, meanlog = MotherMeanLogTUA, sdlog = MotherSDLogTUA)))

# 4 parameter beta
PearsonDS::qpearsonI(1/100000, a = PearsonsMSCTUA$Best$ML[["a"]], b = PearsonsMSCTUA$Best$ML[["b"]],
                     location = PearsonsMSCTUA$Best$ML[["location"]], scale = PearsonsMSCTUA$Best$ML[["scale"]])
100*(PearsonDS::ppearsonI(17, a = PearsonsMSCTUA$Best$ML[["a"]], b = PearsonsMSCTUA$Best$ML[["b"]],
                          location = PearsonsMSCTUA$Best$ML[["location"]], 
                          scale = PearsonsMSCTUA$Best$ML[["scale"]]))
median(PearsonDS::rpearsonI(100000, a = PearsonsMSCTUA$Best$ML[["a"]], b = PearsonsMSCTUA$Best$ML[["b"]],
                            location = PearsonsMSCTUA$Best$ML[["location"]], scale = PearsonsMSCTUA$Best$ML[["scale"]]))
PearsonDS::qpearsonI(1-(1/100000), a = PearsonsMSCTUA$Best$ML[["a"]], b = PearsonsMSCTUA$Best$ML[["b"]],
                     location = PearsonsMSCTUA$Best$ML[["location"]], scale = PearsonsMSCTUA$Best$ML[["scale"]])
100*(1-(PearsonDS::ppearsonI(50, a = PearsonsMSCTUA$Best$ML[["a"]], b = PearsonsMSCTUA$Best$ML[["b"]],
                             location = PearsonsMSCTUA$Best$ML[["location"]], 
                             scale = PearsonsMSCTUA$Best$ML[["scale"]])))



rm(FinalLongCounts, FinalMotherCountsTUA, LongChildGuardianFileTUA, MAACDistributionsTUA, 
   MotherAgeDistributionTUA, MotherAgeModelLNTUA, MotherAgeModelSNTUA, MotherAgesLongTUA,
   MotherParametersSNTUA, Mothersraw, PearsonsMSCTUA, AICLNTUA, AICSNTUA, BICLNTUA, BICSNTUA, i,
   KValueLNTUA, KValueSNTUA, LogLikeLNTUA, LogNTUA, MaxChildAge, MaxMotherAge, MinChildAge, MinMotherAge,
   MotherAlphaTUA, MotherMeanLogTUA, MotherOmegaTUA, MotherSDLogTUA, MotherXiTUA, NumMothersTUA,
   Total65AndOver, TotalKids, TotalLessThan18)


























































































































#############################################################
# Fathers
#############################################################
Fathersraw <- read_excel("~/Sync/PhD/Stats NZ downloaded files/Children/JOB-11611 CONZUL .xlsx", 
                         sheet = "Table 2B sole father",  range = "a9:q70", na = "..C")

# number of NAs by geographic unit
Fathersraw %>%
  dplyr::select(-c(`Total 1`, `No Matches`, `Multiple Matches`, `Total 2`)) %>%
  filter(!(is.na(`Age of Child`)), `Age of Child` != "Total") %>%
  group_by(Area) %>%
  summarise_all(~sum(is.na(.))) %>%
  transmute(Area, sumNA = rowSums(.[-1]))

# there is a lot of data loss due to suppression
# TUA has too little data
# compare TD with CR distributions



#############################################################
# TD distribution estimate

LongChildFatherTD <- Fathersraw %>%
  tidyr::gather(FatherAgeGroup, RRCount, `Less than 18 Years`:`65 Years and Over`, factor_key=TRUE) %>%
  filter(Area == "Timaru District", FatherAgeGroup != "Less than 18 Years", FatherAgeGroup != "65 Years and Over",
         `Age of Child` != "Total", RRCount !=0) %>%
  dplyr::select(-c("Total 1", "No Matches", "Multiple Matches", "Total 2")) %>%
  mutate(RRCount = tidyr::replace_na(RRCount, 0),
         YoungestFatherAge = as.numeric(sub(".*?(\\d+).*", "\\1", FatherAgeGroup)),
         OldestFatherAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", FatherAgeGroup)),
         OldestFatherAge = ifelse(YoungestFatherAge == 65, 69, OldestFatherAge),
         YoungestChildAge = as.numeric(sub(".*?(\\d+).*", "\\1", `Age of Child`)),
         OldestChildAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", `Age of Child`)),
         CountFatherAges = OldestFatherAge - YoungestFatherAge + 1 ,
         CountChildAges = OldestChildAge - YoungestChildAge + 1,
         ChildCountDistribute = RRCount / (CountFatherAges * CountChildAges)) 


#############################################################
# create the counts by father age

for(i in 1:nrow(LongChildFatherTD)) {
  
  MinChildAge <- LongChildFatherTD$YoungestChildAge[i]
  MaxChildAge <- LongChildFatherTD$OldestChildAge[i]
  MinFatherAge <- LongChildFatherTD$YoungestFatherAge[i]
  MaxFatherAge <- LongChildFatherTD$OldestFatherAge[i]
  
  # cat("Child min is", MinChildAge, "max is", MaxChildAge, "mother min is", MinMotherAge, "mother max is", MaxMotherAge, "\n")
  
  GridOfAgesTD <- tidyr::expand_grid(childages = MinChildAge:MaxChildAge,
                                   fatherages = MinFatherAge:MaxFatherAge) %>%
    mutate(AgeDiff = fatherages - childages) %>%
    filter(between(AgeDiff, 18, 64))
  
  # cat("minimum childbirth age is", min(GridOfAges$AgeDiff), "\n")
  
  GridOfAgesTD <- as.data.frame(GridOfAgesTD %>%
                                mutate(CountPerCell = LongChildFatherTD$RRCount[i]/nrow(GridOfAgesTD)))
  
  # return(GridOfAges)
  
  if(exists("LongCountsTD")) {
    LongCountsTD <- bind_rows(LongCountsTD, GridOfAgesTD)
  } else {
    LongCountsTD <- GridOfAgesTD
  }
  
}

####### RUN THE CODE BELOW BEFORE DOING THE LOOP ABOVE A SUBSEQUENT TIME

FinalTDLongCounts <- LongCountsTD
rm(GridOfAgesTD, LongCountsTD)


FinalFatherCountsTD <- FinalTDLongCounts %>%
  group_by(AgeDiff) %>%
  summarise(NumFaths = round(sum(CountPerCell),0))

FatherAgesLongTD <- FinalFatherCountsTD %>%
  tidyr::uncount(weights = NumFaths)

table(FatherAgesLongTD$AgeDiff)

# plot the age distribution
# use one bin for each age, therefore 27 bins
FatherAgeDistributionTD <- ggplot(FatherAgesLongTD, aes(x=AgeDiff)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), color="white", fill="blue", bins=27) +
  labs(x="Age of father at childbirth (years)", y = "Proportion of fathers") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(FatherAgeDistributionTD, file="~/Sync/PhD/Thesis2023/PDFs/FatherAgeDistributionTD.pdf", width=10, height=6, units="in")


rm(FatherAgeDistributionTD, FatherAgesLongTD, FinalFatherCountsTD, FinalTDLongCounts, LongChildFatherTD, i,
   MaxChildAge, MaxFatherAge, MinChildAge, MinFatherAge)







#############################################################
# CR distribution estimate

LongChildFatherCR <- Fathersraw %>%
  tidyr::gather(FatherAgeGroup, RRCount, `Less than 18 Years`:`65 Years and Over`, factor_key=TRUE) %>%
  filter(Area == "Canterbury Region", FatherAgeGroup != "Less than 18 Years", FatherAgeGroup != "65 Years and Over",
         `Age of Child` != "Total", RRCount !=0) %>%
  dplyr::select(-c("Total 1", "No Matches", "Multiple Matches", "Total 2")) %>%
  mutate(RRCount = tidyr::replace_na(RRCount, 0),
         YoungestFatherAge = as.numeric(sub(".*?(\\d+).*", "\\1", FatherAgeGroup)),
         OldestFatherAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", FatherAgeGroup)),
         OldestFatherAge = ifelse(YoungestFatherAge == 65, 69, OldestFatherAge),
         YoungestChildAge = as.numeric(sub(".*?(\\d+).*", "\\1", `Age of Child`)),
         OldestChildAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", `Age of Child`)),
         CountFatherAges = OldestFatherAge - YoungestFatherAge + 1 ,
         CountChildAges = OldestChildAge - YoungestChildAge + 1,
         ChildCountDistribute = RRCount / (CountFatherAges * CountChildAges)) 


#############################################################
# create the counts by father age

for(i in 1:nrow(LongChildFatherCR)) {
  
  MinChildAge <- LongChildFatherCR$YoungestChildAge[i]
  MaxChildAge <- LongChildFatherCR$OldestChildAge[i]
  MinFatherAge <- LongChildFatherCR$YoungestFatherAge[i]
  MaxFatherAge <- LongChildFatherCR$OldestFatherAge[i]
  
  # cat("Child min is", MinChildAge, "max is", MaxChildAge, "mother min is", MinMotherAge, "mother max is", MaxMotherAge, "\n")
  
  GridOfAgesCR <- tidyr::expand_grid(childages = MinChildAge:MaxChildAge,
                                     fatherages = MinFatherAge:MaxFatherAge) %>%
    mutate(AgeDiff = fatherages - childages) %>%
    filter(between(AgeDiff, 18, 64))
  
  # cat("minimum childbirth age is", min(GridOfAges$AgeDiff), "\n")
  
  GridOfAgesCR <- as.data.frame(GridOfAgesCR %>%
                                  mutate(CountPerCell = LongChildFatherCR$RRCount[i]/nrow(GridOfAgesCR)))
  
  # return(GridOfAges)
  
  if(exists("LongCountsCR")) {
    LongCountsCR <- bind_rows(LongCountsCR, GridOfAgesCR)
  } else {
    LongCountsCR <- GridOfAgesCR
  }
  
}

####### RUN THE CODE BELOW BEFORE DOING THE LOOP ABOVE A SUBSEQUENT TIME

FinalCRLongCounts <- LongCountsCR
rm(GridOfAgesCR, LongCountsCR)


FinalFatherCountsCR <- FinalCRLongCounts %>%
  group_by(AgeDiff) %>%
  summarise(NumFaths = round(sum(CountPerCell),0))

FatherAgesLongCR <- FinalFatherCountsCR %>%
  tidyr::uncount(weights = NumFaths)

table(FatherAgesLongCR$AgeDiff)

# plot the age distribution
# use one bin for each age, therefore 40 bins
FatherAgeDistributionCR <- ggplot(FatherAgesLongCR, aes(x=AgeDiff)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), color="white", fill="blue", bins=40) +
  labs(x="Age of father at childbirth (years)", y = "Proportion of fathers") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(FatherAgeDistributionCR, file="~/Sync/PhD/Thesis2023/PDFs/FatherAgeDistributionCR.pdf", width=10, height=6, units="in")



# get median age
median(FatherAgesLongCR$AgeDiff)







# use the Canterbury Region data
# ----------------------------------------------------------------------------------------------
# fit distributions
NumFathersCR <- as.numeric(nrow(FatherAgesLongCR))
LogNCR <- log(nrow(FatherAgesLongCR))
KValueSNCR <- as.numeric(3)




# ----------------------------------------------------------------------------------------------
# skew normal
FatherAgeModelSNCR <- sn::selm(AgeDiff ~ 1, data=FatherAgesLongCR)
FatherParametersSNCR <- sn::extractSECdistr(FatherAgeModelSNCR)
FatherXiCR <- FatherParametersSNCR@dp[1]
FatherOmegaCR <- FatherParametersSNCR@dp[2]
FatherAlphaCR <- FatherParametersSNCR@dp[3]

# cannot locate AIC in the father model, unlike the mother model
# object class seems different, getting an error
# re-ran the mother data, no error
# do AIC manually
round(AICSNCR <- as.numeric((-2* FatherAgeModelSNCR@logL)+2*3))

# do BIC manually as not present in model
round(BICSNTUA <-as.numeric((KValueSNCR*LogNCR)-(2*FatherAgeModelSNCR@logL)),0)




# ----------------------------------------------------------------------------------------------
# log normal
FatherAgeModelLNCR <- fitdistrplus::fitdist(FatherAgesLongCR$AgeDiff, "lnorm", method = "mle")
summary(FatherAgeModelLNCR)
FatherMeanLogCR<- FatherAgeModelLNCR[["estimate"]][["meanlog"]]
FatherSDLogCR <- FatherAgeModelLNCR[["estimate"]][["sdlog"]]

# get AIC and BIC from the model output in the console







# ----------------------------------------------------------------------------------------------
# Pearsons
PearsonsMSCR <- PearsonDS::pearsonFitML(FatherAgesLongCR$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMSCCR <- PearsonDS::pearsonMSC(FatherAgesLongCR$AgeDiff, control=list(iter.max=1e5,eval.max=1e5))
PearsonsMSCCR

PearsonsMSCCR$Best$AIC  # best model
round(PearsonsMSCCR$MSCs[2,2],0) # AIC for best model
round(PearsonsMSCCR$MSCs[4,2],0) # BIC for best model






# ----------------------------------------------------------------------------------------------
# overplot the 3 distributions

FAACDistributionsCR <- ggplot() +
  geom_bar(aes(AgeDiff,  y = after_stat(count / sum(count))), data = FatherAgesLongCR,
           color = "grey47", fill = "white") +
  stat_function(fun = dlnorm, args = list(meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR), 
                aes(color = "Log-normal"), linewidth = 2) +
  stat_function(fun = sn::dsn, args = list(xi=FatherXiCR, omega=FatherOmegaCR, alpha=FatherAlphaCR), 
                aes(color = "Skew-normal"), linewidth = 2) +
  stat_function(fun = PearsonDS::dpearsonI, args = list(a = PearsonsMSCCR$Best$ML[["a"]], 
                                                        b = PearsonsMSCCR$Best$ML[["b"]],
                                                        location = PearsonsMSCCR$Best$ML[["location"]], 
                                                        scale =PearsonsMSCCR$Best$ML[["scale"]]),
                aes(color = "4-parameter beta"), linewidth = 2) +
  labs(x = "Father age at childbirth ", y = "Proportion of fathers") +
  scale_colour_manual("Distribution",
                      values = c("Log-normal" = "#1B9E77", "Skew-normal" = "#D95F02", 
                                 "4-parameter beta" = "#7570B3")) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70),
                     limits = c(0, 70)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))
  
#  ggsave(FAACDistributionsCR, file="~/Sync/PhD/Thesis2023/PDFs/FAACDistributionsCR.pdf", width=10, height=6, units="in")




# ----------------------------------------------------------------------------------------------
# get the extreme values

# skew-normal
sn::qsn(1/100000, xi = FatherXiCR, omega = FatherOmegaCR, alpha = FatherAlphaCR)
100*(sn::psn(17, xi = FatherXiCR, omega = FatherOmegaCR, alpha = FatherAlphaCR))
median(sn::rsn(100000, xi = FatherXiCR, omega = FatherOmegaCR, alpha = FatherAlphaCR))
sn::qsn(1-(1/100000), xi = FatherXiCR, omega = FatherOmegaCR, alpha = FatherAlphaCR)
100*(1-(sn::psn(58, xi = FatherXiCR, omega = FatherOmegaCR, alpha = FatherAlphaCR)))



# log-normal
qlnorm(1/100000, meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR)
100*(plnorm(17, meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR))
median(rlnorm(100000, meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR))
qlnorm(1-(1/100000), meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR)
100*(1-(plnorm(58, meanlog = FatherMeanLogCR, sdlog = FatherSDLogCR)))



# 4-parameter beta
PearsonDS::qpearsonI(1/100000, a = PearsonsMSCCR$Best$ML[["a"]], b = PearsonsMSCCR$Best$ML[["b"]],
                     location = PearsonsMSCCR$Best$ML[["location"]], scale = PearsonsMSCCR$Best$ML[["scale"]])
100*(PearsonDS::ppearsonI(17, a = PearsonsMSCCR$Best$ML[["a"]], b = PearsonsMSCCR$Best$ML[["b"]],
                          location = PearsonsMSCCR$Best$ML[["location"]], 
                          scale = PearsonsMSCCR$Best$ML[["scale"]]))
median(PearsonDS::rpearsonI(100000, a = PearsonsMSCCR$Best$ML[["a"]], b = PearsonsMSCCR$Best$ML[["b"]],
                            location = PearsonsMSCCR$Best$ML[["location"]], 
                            scale = PearsonsMSCCR$Best$ML[["scale"]]))
PearsonDS::qpearsonI(1-(1/100000), a = PearsonsMSCCR$Best$ML[["a"]], b = PearsonsMSCCR$Best$ML[["b"]],
                     location = PearsonsMSCCR$Best$ML[["location"]], scale = PearsonsMSCCR$Best$ML[["scale"]])
100*(1-(PearsonDS::ppearsonI(58, a = PearsonsMSCCR$Best$ML[["a"]], b = PearsonsMSCCR$Best$ML[["b"]],
                             location = PearsonsMSCCR$Best$ML[["location"]], 
                             scale = PearsonsMSCCR$Best$ML[["scale"]])))


# ----------------------------------------------------------------------------------------------
rm(FAACDistributionsCR, FatherAgeDistributionCR, FatherAgeModelLNCR, FatherAgeModelSNCR, FatherAgesLongCR,
   FatherParametersSNCR, Fathersraw, FinalCRLongCounts, FinalFatherCountsCR, LongChildFatherCR, PearsonsMSCCR,
   PearsonsMSCR, AICSNCR, BICSNTUA, FatherAlphaCR, FatherOmegaCR, FatherSDLogCR, FatherXiCR, i, KValueSNCR,
   LogNCR, MaxChildAge, MaxFatherAge, MinChildAge, MinFatherAge, NumFathersCR, FatherMeanLogCR)



























































































































































#############################################################
#############################################################
# Children
#############################################################
#############################################################





# ----------------------------------------------------------------------------------------------
# Proportion living at home

# bring in Timaru District data for child counts per age group

TimaruDistrictAgeTotals8001 <- read.csv("OriginalDataFiles/Usually Resident/Timaru District/Age by sex groups/TABLECODE8001_Data_b339765f-cf78-4e38-aa57-332c2e066b02.csv")

TimaruDistrictAgeTotalsCleaned <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("10-14 years", "25-29 years", "30-34 years", "35-39 years",
                          "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years")) %>%
  rename(ChildAge = Age.group) %>%
  mutate(ChildAge = gsub(" y", " Y", ChildAge))

TimaruDistrict15to17 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("15 years", "16 years", "17 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "15-17 Years")

TimaruDistrict18to24 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("18 years", "19 years", "20-24 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "18-24 Years")

TimaruDistrict65to84 <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years")) %>%
  summarise(Value = sum(Value)) %>%
  mutate(ChildAge = "65 Years and Over")

TimaruDistrictAgeTotalsCleaned <- bind_rows(TimaruDistrictAgeTotalsCleaned, TimaruDistrict15to17,
                                            TimaruDistrict18to24, TimaruDistrict65to84)


# mothers data read in
Mothersraw <- read_excel("~/Sync/PhD/Stats NZ downloaded files/Children/JOB-09626 CONZUL rerun.xlsx",
                         sheet = "Table 1 rerun", range = "a9:q70", na = "..C")

MotherChildTD <- Mothersraw %>%
  filter(Area == "Timaru District", !(`Age of Child` %in% c("Total", "0-4 Years", "5-9 Years"))) %>%
  dplyr::select(`Age of Child`, `Total 2`) %>%
  rename(ChildAge = `Age of Child`, NumberChildren = `Total 2`) %>%
  mutate(ChildAge = ifelse(ChildAge == "15 - 17 Years", "15-17 Years",
                           ifelse(ChildAge == "18 -24 Years", "18-24 Years", ChildAge)))

ChildCountsTD <-  left_join(MotherChildTD, TimaruDistrictAgeTotalsCleaned, by = "ChildAge") %>%
  filter(!(ChildAge %in% c("0-4 Years", "5-9 Years"))) %>%
  mutate(Bottom = c(14, 15, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65),
         Middle = c(14, 16, 21, 27, 32, 37, 42, 47, 52, 57, 62, 74.5),
         Top = c(14, 17, 24, 29, 34, 39, 44, 49, 54, 59, 64, 84),
         PropAtHome =ifelse(ChildAge == "10-14 Years", 1, NumberChildren/Value),
         FakeGroup = c(rep("FakeGroup", nrow(.))))

# get counts for dissertation

CountsForDist <- ChildCountsTD %>%
  select(c(Middle, PropAtHome)) %>%
  mutate(PropAtHome = round(PropAtHome, 3))

CountsForDist

# add in the zero value for age 85 years

TimaruDistrict85Plus <- TimaruDistrictAgeTotals8001 %>%
  dplyr::select(-c(Year, Area, Sex, Flags)) %>%
  filter(Age.group %in% c("85-89 years")) %>%
  dplyr::select(-Age.group) %>%
  mutate(ChildAge = "No children",
         NumberChildren = 0,
         Value = 0,
         Bottom = 85,
         Middle = 85,
         Top = 85,
         PropAtHome = 0,
         FakeGroup = "FakeGroup")

ChildCountsTD <- bind_rows(ChildCountsTD,TimaruDistrict85Plus)

TheGroup <- "FakeGroup"

CalcProps <- interdiff(ChildCountsTD, "Middle", "PropAtHome", "Bottom", "Top", TheGroup)


# graph this with lines like with the relationship one

# ----------------------------------------------------------------------------------------------
# add the age groups back in

MissingAges <- data.frame(Age = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                          Fits = c(rep(1,14)),
                          FakeGroup = c(rep("FakeGroup",14)))

# add in the age that has a .5 mid point

TheHalfAge <- ChildCountsTD %>%
  filter(Middle == 74.5) %>%
  rename(Age = Middle,
         Fits = PropAtHome) %>%
  select(Age, Fits, FakeGroup)

AllAges <- rbind(MissingAges, CalcProps, TheHalfAge)


# get all ages with points calculated from the data
ChildPointsThatExist <- AllAges %>%
  filter(Age %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 21, 27, 32, 37, 42, 47, 52, 57, 62, 74.5, 85))




# ----------------------------------------------------------------------------------------------
# Set up custom breaks for graphs

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

custom_breaks <- seq(0, 90, 10)


# ----------------------------------------------------------------------------------------------
# plot the result
DependChildPropEst <- ggplot(AllAges, aes(x = Age, y = Fits)) +
  geom_point(color = "#56B4E9") +
  geom_point(data = subset(AllAges, Age %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 21, 27, 32,
                                               37, 42, 47, 52, 57, 62, 74.5, 85, ChildPointsThatExist$Fits))) +

  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1),
                     limits = c(0, 1),
                     expand = c(0,.008)) +
  scale_x_continuous(limits = c(0, 95), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE),
                     expand = c(0, .3)) +
  labs(x = "Age (years)", y = "Proportion who are children living at home") +
  geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_vline(xintercept = 9.5, linetype = "dotted") +
  geom_vline(xintercept = 14.5, linetype = "dotted") +
  geom_vline(xintercept = 17.5, linetype = "dotted") +
  geom_vline(xintercept = 24.5, linetype = "dotted") +
  geom_vline(xintercept = 34.5, linetype = "dotted") +
  geom_vline(xintercept = 44.5, linetype = "dotted") +
  geom_vline(xintercept = 54.5, linetype = "dotted") +
  geom_vline(xintercept = 64.5, linetype = "dotted") +
  geom_vline(xintercept = 74.5, linetype = "dotted") +
  geom_vline(xintercept = 84.5, linetype = "dotted") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))

#  ggsave(DependChildPropEst, file="~/Sync/PhD/ThesisVersions/Thesis2023DecemberTexStudio/PDFs/DependChildPropEst.pdf", width=10, height=6, units="in")



# ----------------------------------------------------------------------------------------------
# remove the negative fits from the data
ChildPropsAtHome <- AllAges %>%
  select(-FakeGroup) %>%
  filter(!Age == 74.5)

# ----------------------------------------------------------------------------------------------
# save the props
# save the synthetic population file
saveRDS(ChildPropsAtHome, file = "PhDRData/ChildPropsAtHome.rds")





rm(AllAges, CalcProps, ChildCountsTD, ChildPointsThatExist, DependChildPropEst, MissingAges, MotherChildTD,
   Mothersraw, TimaruDistrict15to17, TimaruDistrict18to24, TimaruDistrict65to84, TimaruDistrictAgeTotals8001,
   TimaruDistrictAgeTotalsCleaned, custom_breaks, TheGroup, every_nth, interdiff, ChildPropsAtHome, CountsForDist,
   TheHalfAge, TimaruDistrict85Plus)


# get expected counts of children per age

File4SchoolIndAdded <- readRDS("PhDRData/File4SchoolIndAdded.rds")
ChildPropsAtHome <- readRDS("PhDRData/ChildPropsAtHome.rds")

NumPplPerAge <- File4SchoolIndAdded %>%
  group_by(Age) %>%
  summarise(NumPerAge = n())

EstimateCtsAtHome <- left_join(NumPplPerAge, ChildPropsAtHome) %>%
  mutate(Fits = ifelse(is.na(Fits), 0, Fits),
         ExpectedNum = round((NumPerAge * Fits),0)) %>%
  select(Age, ExpectedNum)
  

# get oldest ages per household size

OldestAgesHH <- File4SchoolIndAdded %>%
  group_by(UsualResidents, Sex) %>%
  mutate(MaxAge = max(Age)) %>% 
  select(c(UsualResidents, MaxAge, Sex)) %>%
  distinct(UsualResidents, MaxAge, Sex) %>%
  mutate(MaxChildAge = ifelse(Sex == "Male", MaxAge - 15, MaxAge-18))

# add the estimated counts to the child props

ChildPropsAtHome <- left_join(ChildPropsAtHome, EstimateCtsAtHome, by = "Age") %>%
  filter(Age < 73)

# save the file updated with the expected counts
saveRDS(ChildPropsAtHome, file = "PhDRData/ChildPropsAtHome.rds")




















#############################################################
# Sole parent dependent child proportions
#############################################################
# use table 8151
TABLECODE8151 <- read_csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/Number of dependent children and total number of children one parent families/TABLECODE8151_Data_5dc79e76-3c16-46cc-b089-1629feb346e0.csv", 
                          col_types = cols(Year = col_skip(), Area = col_skip(), Flags = col_skip()))

SPPerHHSize <- TABLECODE8151 %>%
  filter(`Number of dependent children in family` == "Total one parent families", 
         `Number of children in family` %in% c("One child", "Two children", "Three children", "Four or more children"))

# get the ratios for households 3, 4, 5 person

NumInTwo <- as.numeric(SPPerHHSize[1,3])

SoleParentRatios <- SPPerHHSize %>%
  mutate(Ratio = Value / NumInTwo)



saveRDS(SoleParentRatios, file = "PhDRData/SoleParentRatios.rds")








#############################################################
# Comparison with couples
#############################################################

TABLECODE8143 <- read.csv("~/Sync/PhD/Stats NZ csv files/Occupied private dwellings/NumDependentChildrenAndTotalNumberChildrenForCouplesWithChildren/TABLECODE8143_Data_d3e4c3d9-4ade-4c59-894d-34ff717bff31.csv")

CouplesWithKids <- TABLECODE8143 %>%
  select(-c(Area, Year, Flags)) %>%
  filter(Number.of.dependent.children.in.family == "Total couples with children") %>%
  rename(NumCouples = Value) %>%
  mutate(HouseholdSize = ifelse(Number.of.children.in.family == "One child", "Three person", 
                                ifelse(Number.of.children.in.family == "Two children", "Four person",
                                       ifelse(Number.of.children.in.family == "Three children", "Five person", 
                                              ifelse(Number.of.children.in.family == "Four or more children","Six or more people",
                                                     "All households")))))

SPHouseholdSizes <- SPPerHHSize %>%
  rename(NumSoleP = Value) %>%
  mutate(HouseholdSize = ifelse(`Number of children in family` == "One child", "Two person",
                                ifelse(`Number of children in family` == "Two children", "Three person",
                                       ifelse(`Number of children in family` == "Three children", "Four person", 
                                              "Five or more people"))))



HouseholdComparison <- merge(CouplesWithKids, SPHouseholdSizes, by = "HouseholdSize") %>%
  mutate(Prop = NumSoleP / (NumSoleP + NumCouples)) %>%
  select(HouseholdSize, NumCouples, NumSoleP, Prop)

print(HouseholdComparison)

# prop sole p in larger
 # 5 or more ppl           # 5 ppl                 # 6 ppl
SPHouseholdSizes[4,3] / (CouplesWithKids[4,3] + CouplesWithKids[5,3] + SPHouseholdSizes[4,3] )














#############################################################
# Twin and triplet rates
#############################################################

MoHMultiples <- read_excel("~/Sync/PhD/Minstry of Health/Births_multiples_202102.xlsx",
                           sheet = "LIVE+STILL", range = "a4:k28")

MoHMultiples <- MoHMultiples %>%
  mutate(TwinRate = Twins/Total,
         TwinsPer100000 = TwinRate * 100000,
         TripletRate = Triplets/Total,
         TripletsPer100000 = TripletRate * 100000,
         TwoYear = substr(Year, 3, 4)) %>%
  dplyr::filter(!(TwoYear %in% c("14", "15", "16", "17", "18", "19")))

# plot the two
library(cowplot)

TwinsRatePlot <- ggplot(MoHMultiples, aes(x = TwoYear, y = TwinsPer100000)) +
  geom_point() +
  scale_y_continuous(limits = c(1000,1800)) +
  scale_x_discrete(limits = c("96", "97", "98", "99", "00", "01", "02", "03", "04", "05", "06", "07", "08", 
                              "09", "10", "11", "12", "13")) +
  labs(x = "Year", y = "Twin birth rate") +
  theme(axis.title.x = element_text(margin = margin(t = 40, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.text=element_text(size=16),
        text = element_text(size = 18))

TripletsRatePlot <- ggplot(MoHMultiples, aes(x = TwoYear, y = TripletsPer100000)) +
  geom_point() +
  scale_y_continuous(limits = c(0,50)) +
  scale_x_discrete(limits = c("96", "97", "98", "99", "00", "01", "02", "03", "04", "05", "06", "07", "08", 
                              "09", "10", "11", "12", "13")) +
  labs(x = "Year", y = "Triplet birth rate") +
  theme(axis.title.x = element_text(margin = margin(t = 40, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.text=element_text(size=16),
        text = element_text(size = 18))

BothPlots <- plot_grid(TwinsRatePlot, TripletsRatePlot, ncol = 1, nrow = 2)

#  ggsave(BothPlots, file="~/Sync/PhD/Thesis2023/PDFs/TwinsAndTriplets.pdf", width=10, height=6, units="in")

detach("package:cowplot", unload = TRUE)

rm(TwinsRatePlot, TripletsRatePlot, BothPlots)


# get median values for the period
median(MoHMultiples$TwinsPer100000)
median(MoHMultiples$TripletsPer100000)


# look at correlation
# is time series so must do a time series specific correlation
library("testcorr")
TempDF <- MoHMultiples %>%
  dplyr::select(TwinsPer100000, TripletsPer100000)

rcorr.test(TempDF, plot = TRUE, table = TRUE, var.names = NULL, scale.font = 1.5)


# number of expected extra children per 100000 births
# rounding used as cannot have part-children
KidsPer100000 <- round(median(MoHMultiples$TwinsPer100000),0) + 2*(round(median(MoHMultiples$TripletsPer100000),0)) + 100000

NumTwinsPer100000 <- 2*(round(median(MoHMultiples$TwinsPer100000),0))
NumTripletsPer100000 <- 3*(round(median(MoHMultiples$TripletsPer100000),0))

# proportion children who are twins
NumTwinsPer100000 / KidsPer100000

# proportion who are triplets
NumTripletsPer100000 / KidsPer100000



# calculate for synthetic population, 

File4CorrectHrs <- readRDS("PhDRData/File4CorrectHrs.rds")
# expected number twins
nrow(File4CorrectHrs)*(NumTwinsPer100000 / KidsPer100000)

# expected number triplets
nrow(File4CorrectHrs)*(NumTripletsPer100000 / KidsPer100000)




rm(MoHMultiples, KidsPer100000, NumTripletsPer100000, NumTwinsPer100000)
