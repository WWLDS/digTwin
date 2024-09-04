# to do
# ALL wait times
# - done but should check immuno and follow up times
# - time between pre-Derm clinic and first Derm clinic
# priorities
# - need to do for nurses and derm
# distributions
# cancellations
# - how many days from appt date do cancels occurs? 
# - how soon do people get an appt after cancellation?
# - use renege instead for these two?
# dna
# do patients take multiple slots per day?? Is this already accounted for?
# ADD CANCELLED AND DNA TO EACH BRANCH
# Seasonality
# 


# # Load the CSV file (update the path as necessary)
# propClinic <- read_csv(here("propClinic.csv"))
write_csv(propClinic, here("propClinicNum.csv"))
# Create a mapping vector
clinic_map <- c("Dr" = 1, "Sister" = 2, "Immuno" = 3, "Specialist" = 4)
priority_map <- c("2 week" = 1, "Urgent" = 2, "Routine" = 3)
# Apply the mapping to the relevant columns
propClinic <- propClinic %>%
    mutate(current_clinic = clinic_map[current_clinic],
           next_clinic = clinic_map[next_clinic])


# Function to get the next clinic based on current clinic and follow-up number
get_next_clinic <- function(current_clinic, followup_count) {
    # Filter the clinic options based on the current clinic and follow-up count
    clinic_options <- propClinic[propClinic$current_clinic == current_clinic &
                                     propClinic$apptNum == followup_count, ]
    
    # If no options are available, return 1 (default to Doctor Clinic)
    if (nrow(clinic_options) < 2) {
        warning("No valid clinic options found for current_clinic =", current_clinic,
                " and followup_count =", followup_count, ". Defaulting to Doctor Clinic.")
        return(4)  # Default to "Derm Clinic"
    }
        # Sample the next clinic based on the provided probabilities
        next_clinic <- sample(clinic_options$next_clinic, 1, prob = clinic_options$prop)
        return(next_clinic)
}

# Define the patient trajectory
### ----------------------------------------------------------------------------
# start simulation
simmer_wrapper <- mclapply(1:5, function(i) {
    
    sim <- simmer("sim")
    
patient_trajectory <- 
    trajectory("Main") %>%

    set_prioritization(c(0, NA, NA)) %>%
    
    set_attribute(tag = "FuToDo", keys = "FuToDo", 
                  values = function() floor(rpois(1, 
                                                  fuCountDistDrRate))) %>%
    
    set_attribute(tag = "ToR", keys = "TimeOfRef",
                  values = function() now(.env = sim)) %>%
#### CREATING PRIORITIES HERE - SHOULD I DO IT OUTSIDE FIRST DOCTOR BRANCH OR INSIDE?
    ### PRIO AND FIRST APPT COULD ALL BE ATTRIBUTES, THEN BRANCH LIKE THE BELOW ONE FOR SUBSEQUENT APPT. WOULD LOOK CLEANER
    set_attribute(tag = "Priority", keys = "Priority",
                  values = function() sample(c(1,2,3), 1, prob = c(0.495, 0.073, 0.432))) %>%
    set_attribute(tag = "Priority Timeout", keys = "Priority Timeout",
                  values = function() {
                      priority <- get_attribute(.env = sim, "Priority")
                      timeout <- if_else(priority == 1, pmax(0, floor(rnorm(1, 10.1, 3.4))),
                                         if_else(priority == 2, pmax(0, floor(rnorm(1, 144, 117))),
                                                 pmax(0, floor(rnorm(1, 220, 128.8)))))
                      return(timeout)
                  }) %>%

    # Step 1: Initial visit to either Doctor or Sister Clinic
    branch(option = function() ifelse(runif(1) < firstIsDr, 1, 2), continue = TRUE,
           trajectory("Doctor Visit") %>%
               timeout_from_attribute("Priority Timeout", keys = "DrTimeout", tag = "DrTimeout") %>%
               branch(option = function() ifelse(runif(1) < 0.9, 1, 2), continue = c(T, F),
                      tag = "Cancellation",
                      trajectory("Not cancelled") %>%
                          branch(option = function() ifelse(runif(1) < 0.9, 1, 2), continue = c(T, F),
                                 tag = "Attendance",
                                 trajectory("Attended") %>%
                                     seize("Doctor Clinic", 1) %>%
                                     timeout(1) %>%
                                     release("Doctor Clinic", 1) %>%
                                     set_attribute(tag = "Doctor", keys = "Doctor", 
                                                   values = 1, mod = "+") %>%
                                     set_attribute("current_clinic", 1),
                                 trajectory("DNA") %>%
                                     seize("Doctor Clinic", 1) %>%
                                     timeout(1) %>%
                                     release("Doctor Clinic", 1) %>%
                                     set_attribute("current_clinic", 1) %>%
                                     set_attribute(tag = "DNA", keys = "DNA", 
                                                   values = 1, mod = "+") %>%
                                     rollback(target = "DrTimeout")
                          ),
                      trajectory("Cancelled") %>%
                          set_attribute(tag = "Cancelled", keys = "Cancelled", 
                                        values = 1, mod = "+") %>%
                          rollback(target = "DrTimeout")
               ),
           trajectory("Sister Visit") %>%
               timeout_from_attribute("Priority Timeout") %>%
               seize("Sister Clinic", 1) %>%
               timeout(1) %>%
               release("Sister Clinic", 1) %>%
               set_attribute(tag = "Sister", keys = "Sister", 
                             values = 1, mod = "+") %>%
               set_attribute("current_clinic", 2)  # 2 refers to Sister Clinic
    ) %>%
    
    set_attribute(tag = "ApptCounter", keys = "ApptCounter", values = 1, mod = "+") %>%
    log_(function() {
        paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
    }) %>%
        
    # Step 2: Follow-up appointments
    timeout(0, tag = "followups") %>%
              branch(
                  option = function() {
                      # Get the current clinic and followup number
                      current_clinic <- get_attribute(.env = sim, "current_clinic")
                      followup_num <- get_attribute(.env = sim, "ApptCounter")
                      # Determine the next clinic
                      next_clinic <- get_next_clinic(current_clinic, followup_num)
                      
                      if (is.na(next_clinic)) {
                          next_clinic <- "Discharge"
                      }
                      # next_clinic <- if(is.na(next_clinic)) {1} else {next_clinic}
                      # Map the clinic name to a numeric value to use in the branch
                      clinic_map <- c("Doctor Clinic" = 1, "Sister Clinic" = 2, 
                                      "Immuno Clinic" = 3, "Derm Clinic" = 4,
                                      "Discharge" = 5)
                      # Return the numeric value corresponding to the next clinic
                      return(clinic_map[next_clinic])
                  }, continue = c(T,T,T,T,F),
                  tag = "followup branch",
                  
                  # Doctor clinic
                  trajectory("Doctor Clinic") %>%
                      timeout(0, tag = "DoctorFU") %>%
                      # log_("test") %>%
                      set_attribute("current_clinic", 1) %>%
                      set_attribute(tag = "Doctor", keys = "Doctor", 
                                    values = 1, mod = "+") %>%
                      set_attribute("ApptCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      log_(function() {
                          paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
                      }) %>%
                      log_(function() {
                          paste("this is: ", get_attribute(.env = sim, "current_clinic"))
                          }) %>%
                      
                      timeout(pmax(0, rweibull(1, 1, 50))) %>%
                      branch(option = function() ifelse(runif(1) < 0.9, 1, 2), continue = c(T, F),
                             tag = "cancellation",
                             trajectory("Not cancelled") %>%
                             branch(option = function() ifelse(runif(1) < 0.9, 1, 2), continue = c(T, F),
                                    tag = "attendance",
                                    trajectory("attended") %>%
                                        seize("Doctor Clinic", 1) %>%
                                        timeout(1) %>%
                                        release("Doctor Clinic", 1) %>%
                                        set_attribute("FuToDo", function()
                                            get_attribute(.env = sim, "FuToDo") - 1),
                                    trajectory("DNA") %>%
                                        seize("Doctor Clinic", 1) %>%
                                        timeout(1) %>%
                                        release("Doctor Clinic", 1) %>%
                                        set_attribute(tag = "DNA", keys = "DNA", 
                                                      values = 1, mod = "+") %>%
                                        rollback(target = "DoctorFU")
                             ),
                             trajectory("Cancelled") %>%
                                 set_attribute(tag = "Cancelled", keys = "Cancelled", 
                                               values = 1, mod = "+") %>%
                                 rollback(target = "DoctorFU")
                      ),
                  
                  # Nurse clinic
                  trajectory("Sister Clinic") %>%
                      timeout(0, tag = "SisterFU") %>%
                      set_attribute("current_clinic", 2) %>%
                      set_attribute(tag = "Sister", keys = "Sister", 
                                    values = 1, mod = "+") %>%
                      set_attribute("ApptCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      log_(function() {
                          paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
                      }) %>%
                      log_(function() {
                          paste("this is: ", get_attribute(.env = sim, "current_clinic"))
                      }) %>%
                      timeout(pmax(0, rweibull(1, 1, 50))) %>%
                      branch(option = function() ifelse(runif(1) < 0.9, 1, 2), continue = c(T, F),
                             tag = "attendance",
                             trajectory("attended") %>%
                                 seize("Sister Clinic", 1) %>%
                                 timeout(1) %>%
                                 release("Sister Clinic", 1) %>%
                                 set_attribute("FuToDo", function()
                                     get_attribute(.env = sim, "FuToDo") - 1),
                             trajectory("DNA") %>%
                                 seize("Sister Clinic", 1) %>%
                                 timeout(1) %>%
                                 release("Sister Clinic", 1) %>%
                                 set_attribute(tag = "DNA", keys = "DNA", 
                                               values = 1, mod = "+") %>%
                                 rollback(target = "SisterFU")
                      ),
                  
                  # Immunotherapy Clinic
                  trajectory("Immuno Clinic") %>%
                      set_attribute("current_clinic", 3) %>%
                      set_attribute(tag = "Immuno", keys = "Immuno", 
                                    values = 1, mod = "+") %>%
                      set_attribute("ApptCounter", function()
                          get_attribute(.env = sim, "ApptCounter") + 1) %>%
                      log_(function() {
                          paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
                      }) %>%
                      log_(function() {
                          paste("this is: ", get_attribute(.env = sim, "current_clinic"))
                      }) %>%
                      timeout(pmax(0, rpois(1, 2.785))) %>%
                      seize("Immuno Clinic", 1) %>%
                      timeout(1) %>%
                      release("Immuno Clinic", 1) %>%
                      set_attribute("FuToDo", function()
                          get_attribute(.env = sim, "FuToDo") - 1),
                  
                  # Specialist Clinic
                  trajectory("Derm Clinic") %>%
                      set_attribute("current_clinic", 4) %>%
                      set_attribute(tag = "Derm", keys = "Derm", 
                                    values = 1, mod = "+") %>%
                          log_(function() {
                              paste("this is: ", get_attribute(.env = sim, "current_clinic"))
                          }) %>%
                      branch(
                          option = function() {
                              current_derm <- get_attribute(.env=sim, "Derm")
                              out <- if(current_derm == 1) {1} else {2}
                              return(out)
                          }, continue = c(T,T),
                          tag = "derm branch",

                          trajectory("First Derm") %>%
                              set_attribute("ApptCounter", function()
                                  get_attribute(.env = sim, "ApptCounter") + 1) %>%
                              log_(function() {
                                  paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
                              }) %>%

                              # set FollowUp number now so it remains static throughout patient traj
                              set_attribute(tag = "DermClinicFUs", keys = "DermClinicFUs",
                                            values = function() floor(rweibull(1, d_followup_shape,
                                                                               d_followup_scale))) %>%
                              set_attribute(tag = "FuToDo", keys = "FuToDo",
                                    values = function() get_attribute(.env = sim,
                                                                      "DermClinicFUs") + get_attribute(.env = sim, "FuToDo")) %>%
                              set_prioritization(c(1, NA, NA)) %>%
                              timeout(pmax(0, rpois(1, 2.785))) %>%
                              seize("Derm Clinic", 1) %>%
                              timeout(1) %>%
                              release("Derm Clinic", 1) %>%
                              set_attribute("FuToDo", function()
                                  get_attribute(.env = sim, "FuToDo") - 1),

                          trajectory("Subsequent Derm") %>%
                              set_attribute("ApptCounter", function()
                                  get_attribute(.env = sim, "ApptCounter") + 1) %>%
                              log_(function() {
                                  paste("ApptCounter incremented to:", get_attribute(.env = sim, "ApptCounter"))
                              }) %>%
                              set_prioritization(c(1, NA, NA)) %>%
                              timeout(pmax(0, rpois(1, 2.785))) %>%
                              seize("Derm Clinic", 1) %>%
                              timeout(1) %>%
                              release("Derm Clinic", 1) %>%
                              set_attribute("FuToDo", function()
                                  get_attribute(.env = sim, "FuToDo") - 1)
                      ),
                  # Branch for Discharge (when next_clinic is NA)
                  trajectory("Discharge") %>%
                      set_attribute(tag = "DischargeTime", keys = "DischargeTime", 
                                    values = function() now(.env = sim), mod = "+") %>%
                      set_attribute(tag = "Discharge", keys = "Discharge", 
                                    values = 1, mod = "+") %>%
                      seize("Discharge", 1) %>%
                      release("Discharge", 1) %>%
                      log_("Patient discharged")
    ) %>%
    
    rollback(target = "followups",
             check = function() get_attribute(.env = sim,
                                              "FuToDo") > 0) %>%
    # rollback(5) %>%
    # Monitor time of discharge
    set_attribute(tag = "DischargeTime", keys = "DischargeTime", 
                  values = function() now(.env = sim), mod = "+") %>%
    set_attribute(tag = "Discharge", keys = "Discharge", 
                  values = 1, mod = "+") %>%
    seize(resource = "Discharge", 1) %>%
    release(resource = "Discharge", 1) %>%
    log_("Patient processing completed")

# check traj plot
print(plot(patient_trajectory))

# Add resources (clinics) to the environment
sim %>% 
    add_resource("Doctor Clinic", capacity = rnorm(1, 48, 1)) %>%
    add_resource("Sister Clinic", capacity = rnorm(1, 13, 1)) %>%
    add_resource("Immuno Clinic", capacity = 20) %>%
    add_resource("Derm Clinic", capacity = schedule(timetable = c(1,2,3,4,5,6,7), 
                                             values = 
                                                 c(rnorm(1,capMean,capSD),
                                                   0,
                                                   rnorm(1,capMean,capSD),
                                                   0,
                                                   rnorm(1,capMean,capSD),
                                                   0,
                                                   0),
                                             period = 7.0)) %>%
    add_resource("Discharge", capacity = Inf) %>%
    # add_generator(name_prefix = "Backlog", trajectory = patient_trajectory, 
    #               mon = 2,
    #               # understand initial list size
    #               distribution = at(rep(x = 0,
    #                                     times = (initWlDr +
    #                                                  initWlSister)))) %>%
    add_generator(name_prefix = "NewPatient", trajectory = patient_trajectory, 
                  distribution = function() rweibull(1, 1/demDistDrShape,
                                                     1/demDistDrScale),
                  mon = 2)

# Run the simulation
set.seed(4); 
sim %>% 
    reset() %>% 
    run(until = 2000,
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
# print(plot(get_mon_arrivals(envs), steps = TRUE))
# print(plot(get_mon_attributes(envs), steps = TRUE))

df_attributes <- get_mon_attributes(envs) |>
    mutate(time = floor(time),
           value = floor(value))
df_arrivals <- get_mon_arrivals(envs)
df_out <- fn_summarise(df_attributes) |>
    mutate(TimeOfRef = floor(TimeOfRef),
           DischargeTime = floor(DischargeTime))


