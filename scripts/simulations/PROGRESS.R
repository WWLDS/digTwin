
# # Load the CSV file (update the path as necessary)
propClinic <- read_csv(here("propClinic.csv"))

# Create a mapping vector
clinic_map <- c("Dr" = 1, "Sister" = 2, "Immuno" = 3, "Specialist" = 4)

# Apply the mapping to the relevant columns
propClinic <- propClinic %>%
    mutate(current_clinic = clinic_map[current_clinic],
           next_clinic = clinic_map[next_clinic])

# Function to determine the number of follow-ups
follow_up_count <- function() {
    sample(1:50, 1)  # Example: Randomly select between 1 and 5 follow-ups
}

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
get_next_clinic(1, 3)
# Define the patient trajectory
?set_attribute
### ----------------------------------------------------------------------------
# start simulation
simmer_wrapper <- mclapply(1:5, function(i) {
    
    sim <- simmer("sim")
    
patient_trajectory <- 
    trajectory() %>%
    # Initialize attributes
    # set_attribute("current_clinic", 1) %>%  # Assuming "1" refers to "Doctor Clinic" or similar
    # set_attribute("followup_num", 1) %>%
    set_prioritization(c(0, NA, NA)) %>%
    set_attribute(tag = "FuToDo", keys = "FuToDo", 
                  values = function() floor(rpois(1, 
                                                  fuCountDistDrRate))) %>%
    set_attribute(tag = "ApptCounter", keys = "ApptCounter", 
                  values = 0, mon = "+") %>%
    # record time patient initially received appt letter
    set_attribute(tag = "ToR", keys = "TimeOfRef",
                  values = function() now()) %>%

    log_("Patient booked") %>%
    timeout(function() rexp(1, rate = 1/5)) %>%  # Wait time

    # Step 1: Initial visit to either Doctor or Sister Clinic
    branch(function() ifelse(runif(1) < firstIsDr, 1, 2), continue = TRUE,
           trajectory("Doctor Visit") %>%
               seize("Doctor Clinic", 1) %>%
               timeout(function() rnorm(1, mean = 20, sd = 5)) %>%
               release("Doctor Clinic", 1) %>%
               set_attribute("current_clinic", 1),  # 1 refers to Doctor Clinic
           trajectory("Sister Visit") %>%
               seize("Sister Clinic", 1) %>%
               timeout(function() rnorm(1, mean = 15, sd = 5)) %>%
               release("Sister Clinic", 1) %>%
               set_attribute("current_clinic", 2)  # 2 refers to Sister Clinic
    ) %>%
    
    set_attribute(tag = "ApptCounter", keys = "ApptCounter", 
                  values = 1, mon = "+") %>%
    
    log_(function() {
        current_clinic_name <- get_attribute(.env = sim, "current_clinic")
        # current_clinic_name <- clinic_reverse_map[as.character(current_clinic_code)]
        paste("Current clinic:", current_clinic_name)
        
    }) %>%  
    # Step 2: Follow-up appointments
    # trajectory() %>%
              branch(
                  option = function() {
                      # Get the current clinic and followup number
                      current_clinic <- get_attribute(.env = sim, "current_clinic")
                      followup_num <- get_attribute(.env = sim, "ApptCounter")
                      # Determine the next clinic
                      next_clinic <- get_next_clinic(current_clinic, followup_num)
                      
                      # Map the clinic name to a numeric value to use in the branch
                      clinic_map <- c("Doctor Clinic" = 1, "Sister Clinic" = 2, 
                                      "Immuno Clinic" = 3, "Derm Clinic" = 4)
                      # Return the numeric value corresponding to the next clinic
                      return(clinic_map[next_clinic])
                  }, continue = c(T,T,T,T),
                  trajectory("Doctor Clinic") %>%
                      # log_("test") %>%
                      set_attribute(tag = "Doctor", keys = "Doctor", 
                                    values = 1, mon = "+") %>%
                      set_attribute("FuCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      log_(function() {
                          paste("this is: ", get_attribute(.env = sim, "Doctor"))
                          }) %>%

                      seize("Doctor Clinic", 1) %>%
                      timeout(function() rnorm(1, mean = 10, sd = 3)) %>%
                      release("Doctor Clinic", 1) %>%
                      set_attribute("FuToDo", function()
                          get_attribute(.env = sim, "FuToDo") - 1),
                  trajectory("Sister Clinic") %>%
                      set_attribute(tag = "Sister", keys = "Sister", 
                                    values = 2, mon = "+") %>%
                      set_attribute("FuCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      log_(function() {
                          paste("this is: ", get_attribute(.env = sim, "Doctor"))
                      }) %>%
                      seize("Sister Clinic", 1) %>%
                      timeout(function() rnorm(1, mean = 10, sd = 3)) %>%
                      release("Sister Clinic", 1) %>%
                      set_attribute("FuToDo", function()
                          get_attribute(.env = sim, "FuToDo") - 1),
                  trajectory("Immuno Clinic") %>%
                      set_attribute(tag = "Immuno", keys = "Immuno", 
                                    values = 3, mon = "+") %>%
                      set_attribute("FuCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      seize("Immuno Clinic", 1) %>%
                      timeout(function() rnorm(1, mean = 10, sd = 3)) %>%
                      release("Immuno Clinic", 1) %>%
                      set_attribute("FuToDo", function()
                          get_attribute(.env = sim, "FuToDo") - 1),
                  trajectory("Derm Clinic") %>%
                      set_attribute(tag = "Derm", keys = "Derm", 
                                    values = 3, mon = "+") %>%
                      set_attribute("FuCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      seize("Derm Clinic", 1) %>%
                      timeout(function() rnorm(1, mean = 10, sd = 3)) %>%
                      release("Derm Clinic", 1) %>%
                      set_attribute("FuToDo", function()
                          get_attribute(.env = sim, "FuToDo") - 1)
    ) %>%

    # rollback(3,
    #          check = function() get_attribute(.env = sim,
    #                            "FuToDo") > 0) %>%
    # rollback(5) %>%
    # Monitor time of discharge
    set_attribute(tag = "Discharge", keys = "Discharge", 
                  values = function() now(), mod = "+") %>%
    seize(resource = "Discharge", 1) %>%
    release(resource = "Discharge", 1) %>%
    log_("Patient processing completed")

# check traj plot
print(plot(patient_trajectory))

# Add resources (clinics) to the environment
sim %>%
    add_resource("Doctor Clinic", capacity = slots_per_day) %>%
    add_resource("Sister Clinic", capacity = slots_per_day) %>%
    add_resource("Immuno Clinic", capacity = slots_per_day) %>%
    add_resource("Derm Clinic", capacity = slots_per_day) %>%
    add_resource("Discharge", capacity = Inf)

# Generate patients and add them to the simulation
sim %>%
    add_generator("Patient", patient_trajectory, at(seq(0, 1000, by = 40)), mon = 2)  # 10 patients per day

# Run the simulation
set.seed(3); 
sim %>% 
    reset() %>% 
    run(until = 1000,
        progress=progress::progress_bar$new()$update) %>% 
    wrap()
})


# Plot the resource usage over time
# Check results section
envs<-simmer_wrapper
res <- envs %>%
    get_mon_resources()
arr <- envs %>%
    get_mon_arrivals()
arrivals_no_resource<-get_mon_arrivals(envs)
arrivals_no_resource$ALOS<-arrivals_no_resource$activity_time
arrivals_no_resource$waiting<-arrivals_no_resource$end_time - 
    arrivals_no_resource$start_time - 
    arrivals_no_resource$activity_time

print(plot(get_mon_resources(envs),steps=TRUE))
print(plot(get_mon_arrivals(envs), steps = TRUE))
print(plot(get_mon_attributes(envs), steps = TRUE))

df_attributes <- get_mon_attributes(envs) |>
    mutate(time = floor(time))
df_arrivals <- get_mon_arrivals(envs)
df_out <- fn_summarise(df_attributes)


