set.seed(123)  # For reproducibility

# Arrival rate parameters (Weibull distribution)
arrival_mean <- 30.62
arrival_sd <- 13.28

# Sample next clinic probabilities DataFrame
propClinic

# Function to get the next clinic based on current clinic and follow-up number
get_next_clinic <- function(current_clinic, followup_count) {
    clinic_options <- propClinic[propClinic$current_clinic == current_clinic &
                                           propClinic$apptNum == followup_count, ]
    
    if (nrow(clinic_options) == 0) {
        return(NA)
    }
    
    next_clinic <- sample(clinic_options$next_clinic, 1, prob = clinic_options$prop)
    return(next_clinic)
}

# Functions to determine the number of follow-ups for each clinic
get_followup_count <- function(clinic) {
    if (clinic == "Dr") {
        return(rpois(1, 2))
    } else if (clinic == "Sister") {
        return(rpois(1, 2))
    } else if (clinic == "Specialist") {
        return(rweibull(1, 1.09, 24.3))
    } else if (clinic == "Immuno") {
        return(rpois(1, 1.5))
    } else {
        return(0)
    }
}

#### CHANGE NUMBERS HERE
# Function to model waiting time before the first visit, depending on the first clinic and priority
get_initial_waiting_time <- function(first_clinic, priority) {
    if (first_clinic == "Dr") {
        if (priority == "2 week") {
            return(pmax(0, rnorm(1, 10.1, 3.4)))  # Shortest wait time
        } else if (priority == "Urgent") {
            return(pmax(0, rnorm(1, 144, 117)))  # Medium wait time
        } else if (priority == "Routine") {
            return(pmax(0, rnorm(1, 220, 128.8)))  # Longest wait time
        }
    } else if (first_clinic == "Sister") {
        if (priority == "Routine") {
            return(pmax(0, rnorm(1, 150, 100)))
    } else {
        return(0)  # Default, although it shouldn't happen since we only have Dr and Sister
        }
    }
}

# Function to model waiting time before each follow-up visit, depending on the next clinic
get_followup_waiting_time <- function(next_clinic) {
    if (next_clinic %in% c("Dr", "Sister")) {
        return(pmax(0, rweibull(1, 1, 50)))  # Ensure non-negative waiting time
    } else if (next_clinic %in% c("Specialist", "Immuno")) {
        return(pmax(0, rpois(1, 2.785)))  # Ensure non-negative waiting time
    } else {
        return(0)  # Default, just in case
    }
}

# Function to get daily slots for each clinic
get_daily_slots <- function() {
    list(
        Dr = pmax(0, rnorm(1, 48.12 ,16.84)),
        Sister = pmax(0, rnorm(1, 67.4, 23.58)),
        Specialist = pmax(0, rnorm(1, 29.63, 4.24)),
        Immuno = pmax(0, rnorm(1, 29.63, 4.24))
    )
}

# Initialize variables
days <- 500  # Number of days to simulate
initial_waiting_list <- 850
waiting_list <- initial_waiting_list
waiting_list_sizes <- numeric(days)  # Vector to store waiting list size at the end of each day
patients_processed <- numeric(days)  # Vector to store the number of patients processed each day

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
    return(pmax(0, rnorm(1, mean = arrival_mean, sd = arrival_sd)))
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
    for (i in 1:waiting_list) {
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
        
        current_clinic <- first_clinic
        followup_count <- 1
        total_followups <- get_followup_count(first_clinic)
        waiting_time <- get_initial_waiting_time(first_clinic, priority)  # Use the specified distribution for initial waiting time
        total_days <- waiting_time  # Start with the initial waiting time
        discharged <- FALSE
        
        # Track visits to each clinic
        visits <- data.frame(
            Dr_visits = 0,
            Sister_visits = 0,
            Specialist_visits = 0,
            Immuno_visits = 0
        )
        
        # Simulate the clinic visits for this patient
        while (followup_count <= total_followups) {
            # Check if there are available slots at the current clinic
            if (daily_slots[[current_clinic]] > 0) {
                # Decrease the available slots for this clinic
                daily_slots[[current_clinic]] <- daily_slots[[current_clinic]] - 1
                
                # Increment the visits count for the current clinic
                visits[paste0(current_clinic, "_visits")] <- visits[paste0(current_clinic, "_visits")] + 1
                
                # Determine the next clinic
                next_clinic <- get_next_clinic(current_clinic, followup_count)
                
                # Add the waiting time before the next follow-up visit
                followup_waiting_time <- get_followup_waiting_time(next_clinic)
                total_days <- total_days + followup_waiting_time
                
                # If there's no next clinic, stop the simulation
                if (is.na(next_clinic)) {
                    break
                }
                
                # Update the current clinic and follow-up count
                current_clinic <- next_clinic
                followup_count <- followup_count + 1
                
                # If transitioning to Specialist or Immuno, add their follow-up counts
                if (current_clinic %in% c("Specialist", "Immuno")) {
                    total_followups <- total_followups + get_followup_count(current_clinic)
                }
            } else {
                # No slots available at this clinic, stop processing and wait until the next day
                # cat("    Patient", patient_id, "cannot be processed today at", current_clinic, "due to full capacity.\n")  # Debugging
                total_days <- total_days + 1  # Add a day of waiting
                break  # Exit the loop to process this patient in the next day
            }
        }
        
        # If the loop exits, the patient is discharged
        discharged <- followup_count > total_followups
        
        # Track the number of patients processed today
        if (daily_slots[[current_clinic]] >= 0) {
            patients_processed_today <- patients_processed_today + 1
            # cat("    Patient", patient_id, "processed at", current_clinic, "on Day", day, "\n")  # Debugging
        }
        
        # Append the results for this patient to the results dataframe
        results <- rbind(
            results,
            data.frame(
                patient_id = patient_id,
                Day = day,
                Dr_visits = visits$Dr_visits,
                Sister_visits = visits$Sister_visits,
                Specialist_visits = visits$Specialist_visits,
                Immuno_visits = visits$Immuno_visits,
                First_Clinic = first_clinic,
                Total_Followups = total_followups,
                Total_Days = total_days,
                Waiting_Time = waiting_time,  # Store the calculated initial waiting time
                Priority = priority,  # Include the patient's priority
                Discharged = discharged,
                stringsAsFactors = FALSE
            )
        )
    }
    
    # Record the waiting list size at the end of the day
    waiting_list_sizes[day] <- waiting_list
    
    # Record the number of patients processed today
    patients_processed[day] <- patients_processed_today
    
    # Adjust the waiting list size by removing processed patients
    waiting_list <- max(0, waiting_list - patients_processed_today)
}

# Print the results dataframe
print(results)

