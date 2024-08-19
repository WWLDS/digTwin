### 
# Wrightington, Wigan & Leigh Digital Twin
# Grouped Clinics Sim 
# v1.1 - "the gpt version"

# To do:
# - proper discharge probability at each FU
# - fix capacity with schedules
# - cancellations / DNA


### ----------------------------------------------------------------------------
# check if the package librarian installed then load librarian
# librarian is a package installation and management pacakage that merges CRAN, 
# GitHub and Bioconductor

if(!require(librarian)){
    install.packages("librarian")
    library(librarian)
}
librarian::shelf(tidyverse, here, glue, simmer, simmer.plot, data.table, MASS,
                 parallel)


### ----------------------------------------------------------------------------
# start simulation
simmer_wrapper <- mclapply(1:5, function(i) {
    
    first_clinic <- sample(c("Dr", "Sister"), 1, prob = c(0.84, 0.16))
    
    sim <- simmer("sim")
    
    traj <- 
        trajectory() %>%
    
    set_attribute(tag = "ToR", keys = "TimeOfRef", 
                  values = function() now(.env = sim)) %>%
        
        trajectory(paste("Patient", patient_id)) %>%
        set_attribute("followup_count", 1) %>%
        set_attribute("current_clinic", first_clinic) %>%
        set_attribute("total_followups", function(attrs) get_followup_count(attrs[["current_clinic"]])) %>%
        set_attribute("Dr_visits", 0) %>%
        set_attribute("Sister_visits", 0) %>%
        set_attribute("Derm_visits", 0) %>%
        set_attribute("Immuno_visits", 0) %>%
    
    set_attribute(tag = "current_clinic", keys = first_clinic, values = 1)
    # 84% chance of "Dr", 16% chance of "Sister"
    # follow_up <- 1 # Start at the first appt
    
    # Conditional timeout based on the initial clinic
    # traj <- traj %>%
    #     set_attribute("first_clinic", 1,
    #                   values = 1, mon = "+") %>%
    #     simmer::timeout(
    #         task = if (current_clinic == "Dr") {
    #             function() rlogis(1, 3.47, 1.54)
    #         } else {
    #             function() rlogis(1, 3.18, 1.33)
    #         }
    #     ) %>%
    #     simmer::seize(current_clinic) %>%
    #     timeout(1) %>%
    #     simmer::release(current_clinic) 

# going for break - next thing is to add follow up appointments and I want to do something with the Discharge probability dependent on FU number and clinic (table of all Discharge probs or something like a predefined FU number counter?)
    
    # for (i in 1:max(propClinic$apptNum)) {  # Handling up to the maximum follow-up defined in the data
    #     traj <- traj %>%
    #         branch(
    #             option = function() rbinom(1, 1, discharge_prob), 
    #             continue = c(TRUE, FALSE),
    #             trajectory() %>% seize("discharge") %>% timeout(0), # Discharge path
    #             trajectory() %>% # Continue follow-up appointments
    #                 simmer::seize(function() {
    #                     next_clinic <- select_next_clinic(current_clinic, i, propClinic)
    #                     current_clinic <<- next_clinic
    #                     traj <<- traj %>%
    #                         simmer::set_attribute("current_clinic", current_clinic)
    #                     return(current_clinic)
    #                 }) %>%
    #                 simmer::timeout(1) %>% # Assume each follow-up takes 1 day
    #                 simmer::release(function() current_clinic)
    #         )
    # }
    
    # return(traj)
})
        
    sim <- simmer("Dermatology Department")
