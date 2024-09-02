library(simmer)
library(dplyr)

# Load the probabilities DataFrame
propClinic <- read_csv(here("propClinic.csv"))

# Function to get the next clinic based on current clinic and follow-up number
get_next_clinic <- function(current_clinic, followup_count) {
    clinic_options <- propClinic %>%
        filter(current_clinic == current_clinic, apptNum == followup_count)
    
    if (nrow(clinic_options) == 0) {
        return(NA)
    }
    
    next_clinic <- sample(clinic_options$next_clinic, 1, prob = clinic_options$prop)
    return(next_clinic)
}

# Functions to determine the number of follow-ups for each clinic
get_followup_count <- function(clinic) {
    if (clinic == "Dr") {
        return(rpois(1, 1.03))
    } else if (clinic == "Sister") {
        return(rpois(1, 1.04))
    } else if (clinic == "Specialist") {
        return(rweibull(1, 1.09, 24.3))
    } else if (clinic == "Immuno") {
        return(rpois(1, 1.5))
    } else {
        return(0)
    }
}

# Define a function to create a patient trajectory
create_patient_trajectory <- function(patient_id) {
    # Randomly select the first clinic as a character string
    first_clinic <- sample(c("Dr", "Sister"), 1, prob = c(0.84, 0.16))
    
    trajectory(paste("Patient", patient_id)) %>%
        set_attribute("followup_count", 1) %>%
        set_attribute("current_clinic", keys = "first_clinic", 1) %>%
        set_attribute("total_followups", values = function() get_followup_count(first_clinic)) %>%
        set_attribute("Dr_visits", 0) %>%
        set_attribute("Sister_visits", 0) %>%
        set_attribute("Specialist_visits", 0) %>%
        set_attribute("Immuno_visits", 0) %>%
        
        seize(first_clinic, 1) %>%
        timeout(1) %>%
        release(first_clinic, 1) %>%
        set_attribute(paste(first_clinic, "_visits", sep = ""), 
                      function() get_attribute(env, paste(first_clinic, "_visits", sep = "")) + 1) %>%
        
        branch(
            option = function() get_attribute(env, "followup_count") <= get_attribute(env, "total_followups"),
            continue = TRUE,
            trajectory() %>%
                # Determine the next clinic
                set_attribute("current_clinic", function() {
                    followup_count <- get_attribute(env, "followup_count")
                    current_clinic <- get_attribute(env, "current_clinic")
                    next_clinic <- get_next_clinic(current_clinic, followup_count)

                    if (is.na(next_clinic)) {
                        stop("No further follow-ups available")
                    }

                    # If transitioning to Immuno or Specialist, add their follow-up counts
                    if (next_clinic %in% c("Specialist", "Immuno")) {
                        extra_followups <- get_followup_count(next_clinic)
                        current_total <- get_attribute(env, "total_followups")
                        env$set_attribute(env$patients$patient_id, "total_followups", current_total + extra_followups)
                    }

                    return(next_clinic)
                }) #%>%
                # set_attribute("Dr_visits", function() {
                #     if (get_attribute(env, "current_clinic") == "Dr") {
                #         return(get_attribute(env, "Dr_visits") + 1)
                #     } else {
                #         return(get_attribute(env, "Dr_visits"))
                #     }
                # }) %>%
                # set_attribute("Sister_visits", function() {
                #     if (get_attribute(env, "current_clinic") == "Sister") {
                #         return(get_attribute(env, "Sister_visits") + 1)
                #     } else {
                #         return(get_attribute(env, "Sister_visits"))
                #     }
                # }) %>%
                # set_attribute("Specialist_visits", function() {
                #     if (get_attribute(env, "current_clinic") == "Specialist") {
                #         return(get_attribute(env, "Specialist_visits") + 1)
                #     } else {
                #         return(get_attribute(env, "Specialist_visits"))
                #     }
                # }) %>%
                # set_attribute("Immuno_visits", function() {
                #     if (get_attribute(env, "current_clinic") == "Immuno") {
                #         return(get_attribute(env, "Immuno_visits") + 1)
                #     } else {
                #         return(get_attribute(env, "Immuno_visits"))
                #     }
                # }) %>%
                # 
                # seize("Dr", 1) %>%
                # timeout(1) %>%
                # release("Dr", 1) %>%
                # set_attribute("followup_count", function() get_attribute(env, "followup_count") + 1) %>%
                # rollback(11, times = Inf)
        )
}

# Create the simulation environment
env <- simmer("Outpatient_Department")

# Add clinic resources with daily varying capacities using schedules
for (clinic in c("Dr", "Sister", "Specialist", "Immuno")) {
    env %>%
        add_resource(clinic, capacity = create_capacity_schedule(40, 2))
}

# Add patients to the environment
for (i in 1:1000) {  # Simulate 10 patients for simplicity
    env %>%
        add_generator(paste("Patient", i), create_patient_trajectory(i), at(0))
}

# Run the simulation
env %>% run(until = 100)

# Retrieve and print simulation logs
arrivals <- env %>% get_mon_arrivals()
attributes <- env %>% get_mon_attributes()

# Combine the logs for better analysis
combined_logs <- attributes %>%
    group_by(name, key) %>%
    summarize(value = max(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value)

combined_logs %>% head()
