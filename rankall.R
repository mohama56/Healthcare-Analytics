rankall <- function(outcome, num = "best") {
            # Read outcome data
            data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
            
            # Define columns for outcomes
            outcome_column <- list(
              "heart attack" = 11,
              "heart failure" = 17,
              "pneumonia" = 23
            )
            
            # Validate outcome
            if (!outcome %in% names(outcome_column)) {
              stop("invalid outcome")
            }
            
            # Convert the relevant outcome column to numeric
            data[, outcome_column[[outcome]]] <- as.numeric(data[, outcome_column[[outcome]]])
            
            # Remove rows with NA in the outcome column
            data <- data[!is.na(data[, outcome_column[[outcome]]]), ]
            
            # Split the data by state
            state_data_list <- split(data, data$State)
            
            # Initialize a list to store results
            result <- list()
            
            # Loop over each state and find the hospital with the given rank
            for (state in names(state_data_list)) {
              state_data <- state_data_list[[state]]
              
              # Order by outcome and then by hospital name
              state_data <- state_data[order(state_data[, outcome_column[[outcome]]], state_data$Hospital.Name), ]
              
              # Determine the rank number
              if (num == "best") {
                rank_num <- 1
              } else if (num == "worst") {
                rank_num <- nrow(state_data)
              } else if (is.numeric(num) && num > nrow(state_data)) {
                result[[state]] <- NA
                next
              } else {
                rank_num <- num
              }
              
              # Append the hospital name and state to the result
              hospital_name <- ifelse(rank_num > nrow(state_data), NA, state_data[rank_num, "Hospital.Name"])
              result[[state]] <- data.frame(hospital = hospital_name, state = state, stringsAsFactors = FALSE)
            }
            
            # Combine the results into a data frame
            result_df <- do.call(rbind, result)
            rownames(result_df) <- NULL
            
            # Return the result data frame
            return(result_df)
}
