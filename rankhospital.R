rankhospital <- function(state, outcome, num = "best") {
          # Read outcome data
          data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
          
          # Define columns for outcomes
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
          
          # Convert the relevant outcome column to numeric
          state_data[, outcome_column[[outcome]]] <- as.numeric(state_data[, outcome_column[[outcome]]])
          
          # Remove rows with NA in the outcome column
          state_data <- state_data[!is.na(state_data[, outcome_column[[outcome]]]), ]
          
          # Order by outcome and then by hospital name to handle ties
          state_data <- state_data[order(state_data[, outcome_column[[outcome]]], state_data$Hospital.Name), ]
          
          # Handle num argument
          if (num == "best") {
            num <- 1
          } else if (num == "worst") {
            num <- nrow(state_data)
          } else if (!is.numeric(num) || num > nrow(state_data)) {
            return(NA)
          }
          
          # Return the hospital name at the specified rank
          return(state_data[num, "Hospital.Name"])
}
