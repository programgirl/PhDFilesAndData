#' Create couples using a weighted age group structure.
#'
#' Creates couples when the only information is the proportions of people in couples, by age group. If there is an age range that should be up-sampled compared to other ages, this can be specified using the uwProp, uwLA, and uwUA variables. If uwProp is not provided, a simple random sampling without replacement is used. The number of couples that are output is determined by probSS. At least one same-sex couple will be output.
#'
#' @export
#' @param people A data frame containing individual people.
#' @param pplage The variable containing the ages. 
#' @param probSS The probability of a person being in a same-sex couple.
#' @param uwProp The proportion of individuals who are to be over-sampled. By default, no age group is up-sampled, and people are selected based on simple random sampling, without replacement.
#' @param uwLA The youngest age for the over-sampling. Required if uwProp value is provided.
#' @param uwUA The oldest age for the over-sampling. Required if uwProp value is provided.
#' @param HHStartNum The starting value for HHNumVar Must be numeric.
#' @param HHNumVar The name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If HHStartNum is specified, household allocation will be performed.
#'
#' @examples
#' PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#' # create unweighted sample with no household variable
#' UnweightedExample <- SameSex(PersonDataframe,.1)
#' # create unweighted sample with household numbers
#' UnweightedExampleHouseholds <- SameSex(PersonDataframe, probSS=.1, AgeCol=2, HHStartNum=51, HHNumVar = "TheHouseholds")
#' # must supply the required columns when using household assignment
#' # doesn't work
#' ExampleHouseholdsWrong <- SameSex(PersonDataframe, AgeCol=2, probSS=.1, HHStartNum=51, HHNumVar = "TheHouseholds", userseed = 5)
#' ExampleHouseholdsAlsoWrong <- SameSex(PersonDataframe, probSS=.1, HHStartNum=51, HHNumVar = TheHouseholds, userseed = 5)
#' # No HHStartNum means that the household numbering subfunction is not performed
#' ExampleHouseholdsAlsoAlsoWrong <- SameSex(PersonDataframe, AgeCol=2, probSS=.1, HHNumVar = "TheHouseholds", userseed = 5)
#' # create weighted example where 40% of people in same-sex couples are aged between 24 and 50 years
#' WeightedExample <- SameSex(PersonDataframe, .1, .4, 24, 50, 2, 5)
#' # add household numbering
#' WeightedWithNumbering <- SameSex(PersonDataframe, 2, .1, .4, 24, 50, 101, "TheHouseholds", 5)
#' # check weighted subfunction worked
#' WeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(WeightedWithNumbering))
#' Example of downweights, can be used with upweight range is not contiguous
#' DownWeightedWithNumbering <- SameSex(PersonDataframe, 2, .1, .2, 24, 50, 101, "TheHouseholds", 5)
#' DownWeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(DownWeightedWithNumbering))
#'

fastmatch <- function(people, pplage, probSS = NULL, uwProp = NULL, uwLA = NULL,
                    uwUA = NULL, HHStartNum = NULL, HHNumVar = NULL, userseed = NULL) {

  # ProbExpected only used if UpWeight is not NULL, is the probability associated with the upweighted age range
  # uwLA/uwUA only used if UpWeight is not NULL

  # get total number of partnered people
  CountPartneredCouples <- as.numeric(nrow(people))
  NumberRequired <- as.numeric(plyr::round_any((probSS*CountPartneredCouples), 2))
  
  #InternalHHNumVar <- get()

  # ensure a probability for same sex couples is included
  if(is.null(probSS)) {
    stop("The probability of being in a same sex couple must be supplied.")
  }

  

  # create simple random sample without weights

   if (is.null(uwProp)) {
    SameSexCouples <- people[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]

  }

  
  
  
  # create weighted samples
  # now need to fix this code in accordance with amendment above.
  if (is.numeric(uwProp)) {

    if(is.null(uwLA) == TRUE) {
      stop("A minimum age for the upweights is required.")
    }
    if(is.null(uwUA) == TRUE) {
      stop("A maximum age for the upweights is required.")
    }
    if (!pplage %in% names(people)) {
      stop("The age variable in the data frame does not exist.")
    }

    
    # get column names as symbols to use inside data frame subfunctions
    
    AgeColName <- sym(names(people[pplage]))
    

  # get proportion of ages for upweighted observations

  PropToUpWeight <- people %>%
    filter(people[[pplage]] >= uwLA & people[[pplage]] <= uwUA) %>%
    summarise(Value=n()) %>%
    mutate(PropResult = Value/nrow(people)) %>%
    pull(PropResult)


  UpWeightObs <- people %>%
    filter(people[[pplage]] >= uwLA & people[[pplage]] <= uwUA)
  
  # print(nrow(UpWeightObs))

  # seed must come before  sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # check against actual proportion
  # only adjust proportion if this differs to expected

  if (PropToUpWeight != uwProp) {

    # upweight fix
    # create upweighted subset
    # create downweighted subset
    # merge to form output dataset

    UpWeightCount <- plyr::round_any(as.numeric((probSS*(uwProp/PropToUpWeight)*nrow(UpWeightObs))), 2)
    
    
    # fix problem if dealing with extremely small number
    # force selection from up-weighted sample
    if(UpWeightCount == 0) {
      UpWeightCount<- 2
    }
    
    #cat("The upweight count is", UpWeightCount, "The number of rows for the UpWeightObs are", nrow(UpWeightObs), "\n")
    
    UpWeightObsSample <- UpWeightObs[sample(1:as.numeric(nrow(UpWeightObs)), UpWeightCount, replace=FALSE),]

    DownWeightObs <- people %>%
      filter(people[[pplage]] < uwLA | people[[pplage]] > uwUA)

    # change output depending on whether we have remaining to be sampled
    # if there is only two total to output, there is no downweight sample
    if((NumberRequired - UpWeightCount) > 0) {
    DownWeightObsSample <- DownWeightObs[sample(1:as.numeric(nrow(DownWeightObs)), (NumberRequired - UpWeightCount),
                                                replace=FALSE),]

    SameSexCouples <- rbind(UpWeightObsSample, DownWeightObsSample)
    
    } else {
      SameSexCouples <- UpWeightObsSample
      # closes if((NumberRequired - UpWeightCount) > 0) {
    }


  } else {

    # the expected and actual proportions are the same so just output a random sample

    SameSexCouples <- people[[pplage]][sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]

  }

  }

  # create households if a start household number is provided
  if (is.numeric(HHStartNum)) {

    # if(is.null(AgeCol)) {
    #   stop("The column number for the age variable must be supplied.")
    # }

    if(is.null(HHNumVar)) {
      stop("A name for the household count variable must be supplied.")
    }

    MaxHHStartNum <- (nrow(SameSexCouples)/2)-1
    SameSexCouples <- SameSexCouples %>%
      arrange(SameSexCouples[[pplage]]) %>%
      mutate({{HHNumVar}} := rep((HHStartNum):(HHStartNum+MaxHHStartNum),
                                             each=2))

  } else {

    HHStartNum <- 1
    MaxHHStartNum <- (nrow(SameSexCouples)/2)-1

    SameSexCouples <- SameSexCouples %>%
      arrange(SameSexCouples[[pplage]]) %>%
      mutate({{HHNumVar}} := rep((HHStartNum):(HHStartNum+MaxHHStartNum),
                                             each=2))

    }



 return(SameSexCouples)
}
