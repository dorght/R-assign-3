## best(state, outcome) returns the hospital name with the best (lowest) 30 day
##   mortality rate from condition "outcome" and "state". Only the first
##   hospital in alphabetical order is returned
## state - two letter state abbreviation in caps
## outcome - "heart attack", "heart failure", or "pneumonia"
## data source is outcome-of-care-measures.csv file in the working directory

best2 <- function(state, outcome) {
    hospitalnamecol = "Hospital.Name" # col 2
    statecol = "State"                # col 7
    heartattackcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" # col 11
    heartfailurecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" # col 17
    pneumoniacol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" # col 23

    
    # Read outcome data file
    outcomedata <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
    
    # validate function input arguments
    statelist <- unique(outcomedata[, statecol])
    if (is.na(match(state, statelist))) {
        stop("invalid state")
    }

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

    # prune data to include just rows with needed state
    # and the needed condition and hospital name columns
    outcomedata <- outcomedata[outcomedata[, statecol] == state,
                               c(hospitalnamecol, datacol)]

    # change outcome condition data to numeric/NA
    # warnings of casting from character to NA are suppressed
    outcomedata[, datacol] <- suppressWarnings(as.numeric(outcomedata[, datacol]))

    # suppress rows with NA data values
    outcomedata <- outcomedata[!is.na(outcomedata[, datacol]), ]
    
    # find best (minimum) rate then prune data to hospitals with that rate
    minoutcome <- min(outcomedata[, datacol])
    outcomedata <- outcomedata[which(outcomedata[, datacol] == minoutcome),
                               hospitalnamecol]

    #return first hospital in alpha order (minimum) that has best rate
    min(outcomedata)
}