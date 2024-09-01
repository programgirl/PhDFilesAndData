#  rm(list = ls())

library(dplyr)

# Get the random seed file
TheRandomSeeds <- readRDS("PhDRData/TheRandomSeeds.rds")

# bring in employer data
JOB09000 <- read.csv("Stats NZ downloaded files/Business/MB_Ind6_GUEC_Timaru Dist.csv")


# remove totals
EmployersNoTots <- JOB09000 %>%
  filter(!(Meshblock == "Total"),
         !(ANZSIC06_Industry == "Total"),
         Year == 2013)

EmployersTots <- JOB09000 %>%
  filter(Meshblock == "Total" | ANZSIC06_Industry == "Total",
         Year == 2013)

# get number of meshblocks
EmployersMeshblocks2013 <- EmployersNoTots %>%
  group_by(Meshblock) %>%
  summarise(NumInd = n())


sum(EmployersMeshblocks2013$NumInd)
median(EmployersMeshblocks2013$NumInd)

EmployerSummary <- EmployersTots %>%
  filter(!(ANZSIC06_Industry == "Total"))

# number of industries
nrow(EmployerSummary)

# construct dataframe giving the random-rounded parameters
RandomRoundedDef <- data.frame("MinValue" = c(0, 25, 100, 1000, 5000),
                               "MinCount" = c(1, 25, 100, 1000, 5000),
                               "MaxCount" = c(21,95, 999, 4999, 50000),
                               "Rounding" = c(3,  5,  10,  50,  100))

# the ones at 20 can't be adjusted using this method, pull into separate data frame

EmployersNot20 <- EmployerSummary %>%
  filter(!(Geo_Unit == 20) & !(Employee_count == 20))

Employers20 <- EmployerSummary %>%
  filter(Geo_Unit == 20 | Employee_count == 20)

divisors <- function(x){
  #  Vector of numberes to test against
  y <- seq_len(x)
  #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  y[ x%%y == 0 ]
}

divisors(111)
divisors(123)
divisors(135)
divisors(165)
divisors(177)
divisors(186)
divisors(204)
divisors(243)
divisors(654)
divisors(5352)
# all divisible by 3, only factor that is common to all. Last two numbers only share 1, 3. Therefore all counts random-rounded to base-3.

# add in minimum and maximum values for employers and employees
EmployersToUse <- EmployerSummary %>%
  select(-c(Meshblock, Year)) %>%
  mutate(MinEmprCnt = ifelse(Geo_Unit == 0, 1, Geo_Unit-2),
         MaxEmprCnt = Geo_Unit+2,
         MinPrsnCnt = ifelse(Employee_count == 0, 1,
                             ifelse(between(Employee_count, 3, 21), Employee_count-2,
                                    ifelse(between(Employee_count, 25, 95), Employee_count-4,
                                           ifelse(between(Employee_count, 100, 800), Employee_count-9, 
                                                  Employee_count-49)))),
         MaxPrsnCnt = ifelse(Employee_count == 0, 2,
                             ifelse(between(Employee_count, 3, 21), Employee_count+2,
                                    ifelse(between(Employee_count, 25, 95), Employee_count+4,
                                           ifelse(between(Employee_count, 100, 800), Employee_count+9, 
                                                  Employee_count+49)))),
         IndCode = gsub( " .*$", "", ANZSIC06_Industry),
         IndName = gsub("^\\S+ ", "", ANZSIC06_Industry)
  )



File14Employers <- createemp(EmployersToUse, "IndCode", "MinEmprCnt", "MaxEmprCnt", "MinPrsnCnt", "MaxPrsnCnt", "StaffCts", 
                            "Company", userseed = TheRandomSeeds[114])            #################### seed 114 %%%%%%%%%%%%%%%%


saveRDS(File14Employers, file = "PhDRData/File14Employers.rds")




# some stats - number range and achieved

#####################################################################################
# NOTE!!!
# number of employees and employers from the EmployersTots last row.
# NOTE!!!
#####################################################################################

MinEmployerCount <- sum(EmployersToUse$MinEmprCnt)
MaxEmployerCount <- sum(EmployersToUse$MaxEmprCnt)
MinEMPLOYEESCount <- sum(EmployersToUse$MinPrsnCnt)
MaxEMPLOYEESCount <- sum(EmployersToUse$MaxPrsnCnt)

NumEmployersAchieved <- as.numeric(nrow(File14Employers))
NumEMPLOYEESAchieved <- sum(File14Employers$StaffCts)

# differences between random-rounded and achieved:
EmployerDiff <- as.numeric(EmployersTots[851,4]) - NumEmployersAchieved
EmployeeDiff <- as.numeric(EmployersTots[851,5]) - NumEMPLOYEESAchieved

NumIndustriesAchieved <- File14Employers %>%
  select(IndCode) %>%
  distinct(IndCode)

# explain why fewer employers
# how many had more employers than employees?

# number of industries
IndEmployersGtEmployees <- EmployersTots %>%
  mutate(EDiff = Geo_Unit - Employee_count) %>%
  filter(EDiff > 0) %>%
  filter(!ANZSIC06_Industry == "Total")
  
nrow(IndEmployersGtEmployees) # number of industries

# sum of differences
sum(IndEmployersGtEmployees$EDiff)

# get industry that is mainly affected

LargestDifference <- IndEmployersGtEmployees %>%
  arrange(desc(EDiff)) %>%
  slice_head(n=1) %>%
  select(ANZSIC06_Industry) %>%
  left_join(File14Employers, by = "ANZSIC06_Industry")


rm(list = ls())




















































################################################################
################################################################
# employers to employees, prep and stats
################################################################
################################################################

File14Employers <- readRDS("PhDRData/File14Employers.rds")
File13SchoolsAdded <- readRDS("PhDRData/File13SchoolsAdded.rds")


# distribution of working hours

HoursWorkedTable <- table(File13SchoolsAdded$HoursWorked)

HoursWorkedTable

round(prop.table(table(File13SchoolsAdded$HoursWorked)),3)



# prop 14 years and younger - none of whom work, by definition
FourteensAndYounger <- File13SchoolsAdded %>%
  filter(Age <= 14)
# Prop 14 and younger
round(nrow(FourteensAndYounger)/nrow(File13SchoolsAdded), 3)

TotalNotWorking <- HoursWorkedTable[1]

FourteenPropOfNotWorking <- nrow(FourteensAndYounger) / TotalNotWorking

# super population
Superannuatants <- File13SchoolsAdded %>%
  filter(Age >=65)
# Prop 65 and younger
nrow(Superannuatants)/nrow(File13SchoolsAdded)
# how many working?
round(prop.table(table(Superannuatants$HoursWorked)),3)


# what prop of population is under 15 and superannuatants not working?
SupersNotWorking <- Superannuatants %>%
  filter(HoursWorked == "No Hours")

(nrow(FourteensAndYounger)+ nrow(SupersNotWorking)) / nrow(File13SchoolsAdded)


# how many working?
Working <- File13SchoolsAdded %>%
  filter(!HoursWorked == "No Hours") 

# calculate the number of employee spaces needed
NumEmpNeeded <- as.numeric(nrow(Working))

# what proportion of employees?
round((NumEmpNeeded/sum(File14Employers$StaffCts)),3)

# what prop of outside area employees needed?
round(1-(NumEmpNeeded/sum(File14Employers$StaffCts)),3)

File14ReducedEmp <- File14Employers %>%
  select(IndCode, IndName, StaffCts, Company)


# what prop of employees come from outside TUA ?

1-(NumEmpNeeded/sum(File14Employers$StaffCts))


# look at schools
PrimarySchoolEmployers <- File14Employers %>%
  filter(IndCode == "P802100")

SecondarySchoolEmployers <- File14Employers %>%
  filter(IndCode == "P802200")

ECEEmployers <- File14Employers %>%
  filter(IndCode == "P801000")


################################################################
################################################################
# employers to employees
################################################################
################################################################


################################################################

File14EmployersAdded <- pairemp(File14ReducedEmp, "Company", "StaffCts", File13SchoolsAdded, "ID", "HoursWorked", 2,
                              missval = "Not employed", 
                              userseed = TheRandomSeeds[115])            #################### seed 115 %%%%%%%%%%%%%%%%

File14EmployersAdded <- File14EmployersAdded %>%
  mutate(IndCode = ifelse(is.na(IndCode), "Not employed", IndCode),
         IndName = ifelse(is.na(IndName), "Not employed", IndName))


saveRDS(File14EmployersAdded, file = "PhDRData/File14EmployersAdded.rds")


# how many in Q860900 - residential care facilities

InQ860900 <- File14EmployersAdded %>%
  filter(IndCode == "Q860900")

Q860900Employers <- InQ860900 %>%
  select(IndCode, Company) %>%
  group_by(Company) %>%
  summarise(NumCos = n())

# how many in correctional and detention services

InO771400 <- File14EmployersAdded %>%
  filter(IndCode == "O771400")

InO771400Employers <- InO771400 %>%
  select(IndCode, Company) %>%
  group_by(Company) %>%
  summarise(NumCos = n())

# get some stats

# how many industries originally?
EmployersIndSummary <- File14Employers %>%
  group_by(ANZSIC06_Industry, IndCode, IndName) %>%
  summarise(NumInIndOriginal = n())


OnlyEmployed <- File14EmployersAdded %>%
  filter(!(HoursWorked == "No Hours"))


# how many industries employ people in the synthetic population?
ByIndustry <- OnlyEmployed %>%
  group_by(IndCode, Company) %>%
  summarise(NumCo = n()) %>%
  group_by(IndCode) %>%
  summarise(NumInd = n())

# what industries are not represented? - these are NA
IndRepCompare <- left_join(EmployersIndSummary, ByIndustry)

MissingInd <- IndRepCompare %>%
  filter(is.na(NumInd))

# which company has the largest number of employees?
# just compare to the File9Employers data frame, sorted descending by staff counts

AchievedStaffCts <- OnlyEmployed %>%
  group_by(Company, IndCode) %>%
  summarise(NumStaff = n()) %>%
  arrange(desc(NumStaff))

NumInTop5 <- as.numeric(AchievedStaffCts[1,3] + AchievedStaffCts[2,3] + AchievedStaffCts[3,3] + + AchievedStaffCts[4,3] +
                          + AchievedStaffCts[5,3])

round(100*(NumInTop5/nrow(OnlyEmployed)),1)

# # what is median number of employees per company in synthetic population THIS IS SYNTHETIC POPULATION ONLY?
# # NOT SINGLE-EMPLOYEE COMPANIES
# 
# median(AchievedStaffCts$NumStaff)
# table(AchievedStaffCts$NumStaff)
# prop.table(table(AchievedStaffCts$NumStaff))


EmployersWithOneStaffMember <- File14Employers %>%
  filter(StaffCts == 1)

# prop of employers who only have one staff member - all employers who employed a person in the synthetic population

round((nrow(EmployersWithOneStaffMember)/nrow(File14Employers)),3)

# what prop of staff do the one-person businesses have?
round((nrow(EmployersWithOneStaffMember)/sum(File14ReducedEmp$StaffCts)),3)


OneStaffEmployersInSyntheticPop <- inner_join(EmployersWithOneStaffMember, OnlyEmployed, 
                                             by = c("IndCode", "IndName", "Company"))

# what prop of the synthetic pop work in these one-person businesses?
round((nrow(OneStaffEmployersInSyntheticPop)/nrow(OnlyEmployed)),3)

EmployersUsed <- File14ReducedEmp %>%
  filter(Company %in% File14EmployersAdded$Company) %>%
  left_join(AchievedStaffCts, by = c("IndCode", "Company")) %>%
  mutate(NumMissing = StaffCts - NumStaff,
         PropMissing = round((NumMissing/StaffCts),3),
         PropFilled = round((NumStaff/StaffCts),3))

# median number employees
quantile(EmployersUsed$StaffCts)

quantile(EmployersUsed$PropFilled)

# cf synthetic employees, num by company
# median number employees
quantile(AchievedStaffCts$NumStaff)


# employers of employed school students

OnlyEmployedInSchool <- OnlyEmployed %>%
  filter(EducationStatus == "Y")

# number of companies

InSchoolCompanies <- OnlyEmployedInSchool %>%
  group_by(Company, IndCode, IndName) %>%
  summarise(NumAdolescents = n())

# get those in education

InSchoolCompanies <- OnlyEmployedInSchool %>%
  filter(IndCode %in% c("P801000", "P802100", "P802200")) %>%
  group_by(Company, IndCode, IndName) %>%
  summarise(NumAdolescents = n())

sum(InSchoolCompanies$NumAdolescents)
