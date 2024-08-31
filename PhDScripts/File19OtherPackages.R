#  rm(list = ls())

library(dplyr)

TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")




###########################################################################
# synthpop
###########################################################################
library(synthpop)

# what are the dependencies

deps <- tools::package_dependencies("synthpop", recursive=TRUE)

deps # has 75 dependencies

str(SD2011)

# reduce the variables down to six 

MySD2011 <- SD2011 %>%
  select(sex, age, income, marital, height, bmi)

# default methods of syn()
MySD2011Output1 <- syn(MySD2011, m=5, seed = TheRandomSeeds[134]) ######################## seed 134 ##############
# five dataframes of 5000 people

# put into dataset
Output1DS <- bind_rows(MySD2011Output1$syn[[1]], MySD2011Output1$syn[[2]], MySD2011Output1$syn[[3]], MySD2011Output1$syn[[4]],
                       MySD2011Output1$syn[[5]])

# sort shows four bmis that are 450

# test for NAs
colSums(is.na(Output1DS)) > 0

# will it do part-sizes?

MySD2011Output2 <- syn(MySD2011, m=.5, seed = TheRandomSeeds[135]) ######################## seed 135 ##############
# produces one data frame of 5000 people

# what about part > 1

MySD2011Output3 <- syn(MySD2011, m=2.5, seed = TheRandomSeeds[136]) ######################## seed 136 ##############


# what about when data frame is shrunk from 5000 to 4000 - does it output 4000 or 5000?

set.seed(TheRandomSeeds[137])                                      ######################## seed 137 ##############
MySD2011With4000 <- MySD2011 %>%
  slice_sample(n=4000)

With4000 <- syn(MySD2011With4000, seed = TheRandomSeeds[138])      ######################## seed 138 ##############












###########################################################################
# humanleague
###########################################################################

library(humanleague)

# what are the dependencies

depshl <- tools::package_dependencies("humanleague", recursive=TRUE)

depshl # has 3 dependencies

# 2D pop from the example

gender=c(51,49)
age=c(17,27,35,21)
states=qis(list(1,2),list(gender,age))$result
table=flatten(states,c("Gender","Age"))
print(nrow(table[table$Gender==1,])) # 51
print(nrow(table[table$Age==2,])) # 27

table(table$Gender, table$Age)


# with third variable

marital=c(75, 25)
states3=qis(list(1,2,3),list(gender,age, marital))$result
table3=flatten(states3,c("Gender","Age", "Marital"))

table(table3$Gender, table3$Age, table3$Marital)

prop.table(table(table3$Marital,table3$Gender, table3$Age))

















###########################################################################
# ABM
###########################################################################

library(ABM)


# simulate an agent based SEIR model
# specify an exponential waiting time for recovery
gamma = newExpWaitingTime(0.2)
# specify a tansmission rate
beta = 0.4
# specify a exponentially distributed latent period
sigma =newExpWaitingTime(0.5)
# the population size
N = 10000
# create a simulation with N agents, initialize the first 5 with a state "I" 
# and the remaining with "S".
sim = Simulation$new(N, function(i) if (i <= 5) "I" else "S")
# add event loggers that counts the individuals in each state.
# the first variable is the name of the counter, the second is
# the state for counting. States should be lists. However, for
# simplicity, if the state has a single value, then we 
# can specify the list as the value, e.g., "S", and the state
# is equivalent to list("S")
sim$addLogger(newCounter("S", "S"))
sim$addLogger(newCounter("E", "E"))
sim$addLogger(newCounter("I", "I"))
sim$addLogger(newCounter("R", "R"))
# create a random mixing contact pattern and attach it to sim
m = newRandomMixing()
sim$addContact(m)
# the transition for leaving latent state anbd becoming infectious
sim$addTransition("E"->"I", sigma)
# the transition for recovery
sim$addTransition("I"->"R", gamma)
# the transition for tranmission, which is caused by the contact m
# also note that the waiting time can be a number, which is the same
# as newExpWaitingTime(beta)
sim$addTransition("I" + "S" -> "I" + "E" ~ m, beta)
# run the simulation, and get a data.frame object
result = sim$run(0:100)
print(result)


# creates a simulation with 100 agents
sim2 = Simulation$new(100)
# add a Poisson network with a mean degree 5
sim2$addContact(newConfigurationModel(function(n) rpois(n, 5)))


