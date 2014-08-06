## rankall(outcome, rank) returns for each state the 
##   hospital names (first alphabetically) with the mortality rate "rank"
##   from condition "outcome"
## outcome - "heart attack", "heart failure", or "pneumonia"
## rank - desired numeric rank
## data source is outcome-of-care-measures.csv file in the working directory

rankall <- function(outcome, rank) {
    hospitalnamecol = "Hospital.Name" # col 2
    statecol = "State" # col 7
    heartattackcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" # col 11
    heartfailurecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" # col 17
    pneumoniacol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" # col 23

    
    ## Read outcome data file
    outcomedata <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
    
    # index returned by match() is used to convert the input argument to the
    # appropriate column in the data. So outcomearg and outcomecol need to match
    outcomearg = c("heart attack", "heart failure", "pneumonia")
    outcomecol = c(heartattackcol, heartfailurecol, pneumoniacol)
    datacol <- match(outcome, outcomearg) 
    if (is.na(datacol)) {
        stop("invalid outcome")
    }
    else {
        datacol <- outcomecol[datacol]
    }

    # prune data to just the state, needed condition, hospital name columns
    outcomedata <- outcomedata[, c(statecol, hospitalnamecol, datacol)]

    # change outcome condition data to numeric/NA
    outcomedata[, datacol] <- suppressWarnings(as.numeric(outcomedata[, datacol]))

    # suppress rows with NA data values
    # warnings of casting from character to NA are suppressed
    outcomedata <- outcomedata[!is.na(outcomedata[, datacol]), ]

    # sort by outcome first priority, then hospital name
    ranking <- order(outcomedata[, datacol], outcomedata[, hospitalnamecol])  
    outcomedata <- outcomedata[ranking, ]
    
    # state by state add hospital name with desired rank
    rankedbystate <- data.frame(hospital = character(), state = character())
    statelist <- sort(unique(outcomedata[, statecol]))
    for (eachstate in statelist) {
        statedata <- outcomedata[outcomedata[, statecol] == eachstate, ]
        ranked <- rank
        if (ranked == "best") ranked <- 1
        if (ranked == "worst") ranked <- nrow(statedata)
        # check validity of ranking function argument
        if (ranked < 1 || ranked > nrow(statedata)) {
            addstate <- data.frame(hospital = NA, state = eachstate)
            rankedbystate <- rbind(rankedbystate, addstate)
         }
        else {
            addstate <- data.frame(hospital = statedata[ranked, hospitalnamecol]
                                   , state = eachstate)
            rankedbystate <- rbind(rankedbystate, addstate)
         }
    }
    
    # return data.frame of hospital with desired rank for each state
    rankedbystate
}
