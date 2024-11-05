          ## Finding the best hospital in a state


best <- function(state, outcome) {
        # Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Define column names for outcomes
        outcome_column <- list(
          "heart attack" = 11,
          "heart failure" = 17,
          "pneumonia" = 23
        )
        
        # Validate state
        if (!state %in% unique(data$State)) {
          stop("invalid state")
        }
        
        # Validate outcome
        if (!outcome %in% names(outcome_column)) {
          stop("invalid outcome")
        }
        
        # Filter data for the specified state
        state_data <- subset(data, State == state)
        
        # Convert the relevant column to numeric
        state_data[, outcome_column[[outcome]]] <- as.numeric(state_data[, outcome_column[[outcome]]])
        
        # Remove rows with NA in the outcome column
        state_data <- state_data[!is.na(state_data[, outcome_column[[outcome]]]), ]
        
        # Find the hospital with the lowest 30-day mortality rate
        min_value <- min(state_data[, outcome_column[[outcome]]])
        best_hospitals <- state_data[state_data[, outcome_column[[outcome]]] == min_value, "Hospital.Name"]
        
        # Sort hospitals alphabetically and return the first one
        return(sort(best_hospitals)[1])
}



      ## Here is some sample output from the function.


> source("best.R")
> best("TX", "heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
> best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER"
> best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"
> best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"
> best("BB", "heart attack")
Error in best("BB", "heart attack") : invalid state
> best("NY", "hert attack")
Error in best("NY", "hert attack") : invalid outcome

