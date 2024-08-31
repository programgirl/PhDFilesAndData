
#  rm(list = ls())

library(dplyr)
library(ggplot2)

# Get the random seed file
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# get the population data

File16SyntheticPopulation <- readRDS("PhDRData/File16SyntheticPopulation.rds")


################################################################
################################################################
# social networks
################################################################
################################################################

# add the number of contacts for each person


# # look at negative binomial
# NegBinomExamples <- data.frame(n20p5 = c(rnbinom(10000000, 20, mu=5)),
#                                n30p5 = c(rnbinom(10000000, 30, mu=5))
# )
# 
# 
# ggplot(NegBinomExamples, aes(x=n20p5)) + geom_histogram(binwidth=.5)
# 
# max(NegBinomExamples$n20p5)
# max(NegBinomExamples$n30p5)
# 
# median(NegBinomExamples$n20p5)
# median(NegBinomExamples$n30p5)





# use Poisson
PoissonExamples <- data.frame(L3 = c(rpois(10000000, 3)),
                              L4 = c(rpois(10000000, 4)),
                              L5 = c(rpois(10000000, 5)),
                              L6 = c(rpois(10000000, 6)),
                              L7 = c(rpois(10000000, 7)),
                              L8 = c(rpois(10000000, 8)))

ggplot(PoissonExamples, aes(x=L4)) + geom_histogram(binwidth=.3)

round(prop.table(table(PoissonExamples$L3)),3)
round(prop.table(table(PoissonExamples$L4)),3)
round(prop.table(table(PoissonExamples$L5)),3)
round(prop.table(table(PoissonExamples$L6)),3)

# round(100*(prop.table(table(PoissonExamples$L5))),3)
dpois(5, 3)
dpois(5, 4)


# expected zero counts
round(nrow(File16SyntheticPopulation)*dpois(0, 3),0)
# expected zero counts
round(nrow(File16SyntheticPopulation)*dpois(0, 4),0)


# as prop of population
round((nrow(File16SyntheticPopulation)*dpois(0, 3))/nrow(File16SyntheticPopulation),3)

round((nrow(File16SyntheticPopulation)*dpois(0, 4))/nrow(File16SyntheticPopulation),3)

################################################################
# add the number of friends
################################################################

set.seed(TheRandomSeeds[132])                                                ######################## seed 132
File17MatrixOfCounts <- matrix(rpois(nrow(File16SyntheticPopulation), 4))

saveRDS(File17MatrixOfCounts, file = "PhDRData/File17MatrixOfCounts.rds")





# get some stats

sum(File17MatrixOfCounts)
# table of achieved counts
table(File17MatrixOfCounts)
round(prop.table(table(File17MatrixOfCounts)), 3)
median(File17MatrixOfCounts)

# get prop 5 contacts and under

FiveAndUnder <- File17MatrixOfCounts[File17MatrixOfCounts < 6] 

round(length(FiveAndUnder) / length(File17MatrixOfCounts), 100)

TenAndUnder <- File17MatrixOfCounts[File17MatrixOfCounts < 11]

round(length(TenAndUnder) / length(File17MatrixOfCounts), 100)


# get a nice plot to break up the text

ContactsGraph <- ggplot()+
  aes(File17MatrixOfCounts)+
  geom_histogram(aes(y=after_stat(density)), binwidth = 1, color = "white", fill = "grey47") +
  labs(x = "Number of physical social contacts per day", y = "Proportion of people") +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_y_continuous(limits = c(0,.2), breaks = seq(0, .2, by = .05)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))
  

#   ggsave(ContactsGraph, width=9.32, height=7.78, units="in", file="~/Sync/PhD/ThesisVersions/Thesis2024/PDFs/ContactsGraph.pdf")








################################################################
# add the contacts
################################################################

File16SyntheticPopulation <- readRDS("PhDRData/File16SyntheticPopulation.rds")
File17MatrixOfCounts <- readRDS("PhDRData/File17MatrixOfCounts.rds")

SyntheticPopulation <- File16SyntheticPopulation %>%
  rename(OldID = ID) %>%
  mutate(ID = row_number())

# how many contacts?

nrow(SyntheticPopulation)^2


# how many populated in a sparse matrix?

round(100*((sum(File17MatrixOfCounts)+1)/2)/(nrow(SyntheticPopulation)^2),3)

# add counts to the synthetic population

File17ReducedPop <- SyntheticPopulation %>%
  select(ID, Age) 
# 
# File9Finished <- addnetwork(File9ReducedVariables, "ID", "Age", File9MatrixOfCounts, sdused=1, probsame = .5, userseed = 2024,
#                             numiters = 10)
# 
# # still reaching vector memory exhausted, cut the problem in two
# 
# File9ReducedVariablesA <- File9ReducedVariables %>%
#   slice_sample(n=12479, replace=FALSE)
# 
# MatrixA <- File9MatrixOfCounts[1:12479]
# 
# File9A <- addnetwork(File9ReducedVariablesA, "ID", "Age", MatrixA, sdused=1, probsame = .5, userseed = 2024,
#                             numiters = 10)
#   
# File9ReducedVariablesB <- File9ReducedVariables %>%
#   filter(!ID %in% File9ReducedVariablesA$ID)
# 
# MatrixB <- File9MatrixOfCounts[12480:24957]
# 
# File9B <- addnetwork(File9ReducedVariablesB, "ID", "Age", MatrixB, sdused=1, probsame = .5, userseed = 2024,
#                      numiters = 10)

# use the igraph option

File17ContactInfo <- addnetwork(File17ReducedPop, "ID", "Age", File17MatrixOfCounts, sdused = 2, probsame = .5, 
                                userseed = TheRandomSeeds[133],          ######################## seed 133
                                numiters = 10, usematrix = "N")

saveRDS(File17ContactInfo, file = "PhDRData/File17ContactInfo.rds")

File17ContactsDataframe <- igraph::as_data_frame(File17ContactInfo) %>%
  select(-label)

saveRDS(File17ContactsDataframe, file = "PhDRData/File17ContactsDataframe.rds")
saveRDS(SyntheticPopulation, file = "PhDRData/SyntheticPopulation.rds")

# none of these plots produce good output
# plot(File9ContactInfo)
# 
# plot(igraph::simplify(File9ContactInfo), vertex.size= 0.01, edge.arrow.size=0.001,
#      vertex.label.cex = 0.75,vertex.label.color = "black"  ,vertex.frame.color = adjustcolor("white", alpha.f = 0), 
#      vertex.color = adjustcolor("white", alpha.f = 0),edge.color=adjustcolor(1, alpha.f = 0.15),display.isolates=FALSE,
#      vertex.label=ifelse(igraph::page_rank(File9ContactInfo)$vector > 0.1 , "important nodes", NA))






################################################################
# additional versions of the synthetic population
################################################################

# get variable information from the original synthetic population

str(File16SyntheticPopulation)

SyntheticPopulationOrig <- File16SyntheticPopulation %>%
  droplevels() %>%
  rename(SchoolID = schoolID) %>%
  mutate(SchoolID = as.numeric(SchoolID),
         Type = ifelse(Type=="Opposite sex without child", "Opposite sex no kids", Type)) %>%
  select(-SexMarker)
  

str(SyntheticPopulationOrig)

table(SyntheticPopulationOrig$AgeGroup)
table(SyntheticPopulationOrig$UsualResidents)
table(SyntheticPopulationOrig$HoursWorked)

range(SyntheticPopulationOrig$ID)
range(SyntheticPopulationOrig$Age)
range(SyntheticPopulationOrig$HouseholdID)

table(SyntheticPopulationOrig$Type)

range(SyntheticPopulationOrig$SchoolID)
range(SyntheticPopulationOrig$IndCode)
range(SyntheticPopulationOrig$IndName)
range(SyntheticPopulationOrig$Company)
range(SyntheticPopulationOrig$ECEIndicator)
table(SyntheticPopulationOrig$ECEIndicator)

table(SyntheticPopulationOrig$ECEProvider)
range(SyntheticPopulationOrig$ECEProvider)

table(SyntheticPopulationOrig$RestHomeIndicator)
range(SyntheticPopulationOrig$RestHomeIndicator)

table(SyntheticPopulationOrig$RestHome)
range(SyntheticPopulationOrig$RestHome)


# create one for agent-based modelling

# how many work and school
table(SyntheticPopulationOrig$HoursWorked, SyntheticPopulationOrig$EducationStatus)


# create one for Covasim


