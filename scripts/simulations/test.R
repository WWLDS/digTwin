# Initialize variables
days <- 5  # Number of days to simulate
initial_waiting_list <- 10
waiting_list <- initial_waiting_list
waiting_list_sizes <- numeric(days)  # Vector to store waiting list size at the end of each day
patients_processed <- numeric(days)  # Vector to store the number of patients processed each day

patient_records <- list()

results <- data.frame(
    patient_id = integer(0),
    Day = integer(0),
    Dr_visits = integer(0),
    Sister_visits = integer(0),
    Specialist_visits = integer(0),
    Immuno_visits = integer(0),
    First_Clinic = character(0),
    Total_Followups = integer(0),
    Total_Days = integer(0),
    Waiting_Time = numeric(0),
    Discharged = logical(0),
    stringsAsFactors = FALSE
)

patient_id_counter <- 1  # Counter for assigning patient IDs

# Function to model daily arrivals using Weibull distribution
get_daily_arrivals <- function() {
    return(0)
}



# Simulate each day
for (day in 1:days) {
    cat("\nDay", day, ":\n")  # Debugging: Print the day
    
    # Determine how many new patients arrive today
    new_arrivals <- ceiling(get_daily_arrivals())
    waiting_list <- waiting_list + new_arrivals
    
    cat("  New arrivals:", new_arrivals, "\n")  # Debugging: Print new arrivals
    cat("  Total patients in waiting list:", waiting_list, "\n")  # Debugging: Print waiting list size
    
    # Get daily slots for each clinic
    daily_slots <- get_daily_slots()
    cat("  Daily slots available: Dr:", daily_slots$Dr, "Sister:", daily_slots$Sister, 
        "Specialist:", daily_slots$Specialist, "Immuno:", daily_slots$Immuno, "\n")  # Debugging: Print available slots
    
    patients_processed_today <- 0  # Track how many patients are processed
    
    # Process the patients in the waiting list
    for (arrival in 1:new_arrivals) {
        patient_id <- patient_id_counter
        patient_id_counter <- patient_id_counter + 1
        
        # Initialize patient data
        first_clinic <- sample(c("Dr", "Sister"), 1, prob = c(0.84, 0.16))
        
        # Select priority based on the first clinic
        if (first_clinic == "Dr") {
            priority <- sample(c("2 week", "Urgent", "Routine"), 1, prob = c(0.495, 0.073, 0.432))
        } else {
            priority <- "Routine"  # Default priority for Sister clinic
        }
        
        # current_clinic <- first_clinic
        followup_count <- 1
        total_followups <- get_followup_count(first_clinic)
        waiting_time <- get_initial_waiting_time(first_clinic, priority)  # Use the specified distribution for initial waiting time
        total_days <- waiting_time  # Start with the initial waiting time
        discharged <- FALSE
        
        # Track visits to each clinic
        patient_record <- list(
            patient_id = patient_id,
            first_clinic = first_clinic,
            priority = priority,
            total_followups = total_followups,
            total_days = total_days,
            waiting_time = waiting_time,
            Dr_visits = 0,
            Sister_visits = 0,
            Specialist_visits = 0,
            Immuno_visits = 0,
            discharged = FALSE
        )
        
        # Add the patient record to the list
        patient_records[[patient_id]] <- patient_record
        
        
        # Process the patients in the waiting list
        for (i in seq_along(patient_records)) {
            patient <- patient_records[[i]]
            
            if (patient$discharged) next
            
            current_clinic <- patient$first_clinic
            
        # Simulate the clinic visits for this patient
        while (followup_count <= total_followups) {
            # Check if there are available slots at the current clinic
            
            daily_slots <- list(
                Dr = pmax(0, rnorm(1, 48.12, 16.84)),
                Sister = pmax(0, rnorm(1, 67.4, 23.58)),
                Specialist = pmax(0, rnorm(1, 29.63, 4.24)),
                Immuno = pmax(0, rnorm(1, 29.63, 4.24))
            )
            
            if (daily_slots[[current_clinic]] > 0) {
                # Decrease the available slots for this clinic
                daily_slots[[current_clinic]] <- daily_slots[[current_clinic]] - 1
                
                # Increment the visits count for the current clinic
                if (current_clinic == "Dr") {
                    patient$Dr_visits <- patient$Dr_visits + 1
                } else if (current_clinic == "Sister") {
                    patient$Sister_visits <- patient$Sister_visits + 1
                } else if (current_clinic == "Specialist") {
                    patient$Specialist_visits <- patient$Specialist_visits + 1
                } else if (current_clinic == "Immuno") {
                    patient$Immuno_visits <- patient$Immuno_visits + 1
                }
                
                # Determine the next clinic
                next_clinic <- get_next_clinic(current_clinic, patient$total_followups)
                
                # Add the waiting time before the next follow-up visit
                followup_waiting_time <- get_followup_waiting_time(next_clinic)
                patient$total_days <- patient$total_days + followup_waiting_time
                
                # If there's no next clinic, stop the simulation
                if (is.na(next_clinic)) {
                    patient$discharged <- TRUE
                    break
                }
                
                # Update the current clinic and follow-up count
                current_clinic <- next_clinic
                patient$total_followups <- patient$total_followups + 1
                
            #     # If transitioning to Specialist or Immuno, add their follow-up counts
            #     if (current_clinic %in% c("Specialist", "Immuno")) {
            #         patienttotal_followups <- total_followups + get_followup_count(current_clinic)
            #     }
            # } else {
            #     # No slots available at this clinic, stop processing and wait until the next day
            #     cat("    Patient", patient_id, "cannot be processed today at", current_clinic, "due to full capacity.\n")  # Debugging
            #     total_days <- total_days + 1  # Add a day of waiting
            #     break  # Exit the loop to process this patient in the next day
            # }
        # }
        
        # If the loop exits, the patient is discharged
            if (patient$total_followups > followup_count) {
                patient$discharged <- TRUE
            }
            }
        }
        
        # Track the number of patients processed today
        if (daily_slots[[current_clinic]] >= 0) {
            patients_processed_today <- patients_processed_today + 1
            cat("    Patient", patient_id, "processed at", current_clinic, "on Day", day, "\n")  # Debugging
        }
        
            # Update the patient record in the list
            patient_records[[i]] <- patient
        }
            
            # Remove discharged patients from the waiting list
            waiting_list <- waiting_list - sum(sapply(patient_records, function(x) x$discharged))
        }
        
            # Convert the patient records list into a data frame for analysis
        results <- do.call(rbind, lapply(patient_records, as.data.frame))
        
        # Print the results dataframe to inspect the final patient journeys
        print(results)


