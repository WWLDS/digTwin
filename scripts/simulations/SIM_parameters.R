### 
# Wrightington, Wigan & Leigh Digital Twin
# Grouped Clinics Sim


### ----------------------------------------------------------------------------
# check if the package librarian installed then load librarian
# librarian is a package installation and management pacakage that merges CRAN, 
# GitHub and Bioconductor

if(!require(librarian)){
    install.packages("librarian")
    library(librarian)
}
librarian::shelf(tidyverse, here, glue, simmer, simmer.plot, data.table, MASS)


### ----------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# simulation parameters

### first appt ---
# Proportion of NEW clinics that are Dr (the rest are Nurse)
firstIsDr

fn_firstAppt <- function () {
    
    out <- if(runif(1) <= firstIsDr) {0} else {1}
    
    return(out)
    
}
runif(1)

### Doctors parameters ---
## NEW ---
# capacity
capDistDrMean
capDistDrSD
# intake wait time
waitDistDrMean
waitDistDrSD
# demand (arrival rate)
demDistDrShape
demDistDrScale
# initial waiting list size
initWlDr <- 782
# dist of FU count
fuCountDistDrRate
# follow up where
# make this a function?
# cancellation chance
# dna chance


### Sisters parameters ---
## NEW ---
# capacity
capDistSis
# intake wait time
waitDistMean
waitDistSD
# demand (arrival rate)
demDistRate
# initial waiting list size
initWlSister <- 60
# likeilihood of FU
sisPropFU
# follow up where
# make this a function?
# cancellation chance
# dna chance


### Shared parameters ---
## FU
# wait between FU
waitBetFUDrSisMean
waitBetFUDrSisSD

### DERM parameters ---
# capacity
capMean <- 29.63
capSD <- 4.24

# intake wait time
d_intake_shape <- 1.43
d_intake_scale <- 383.97
t_intake <- 1

# cancellation
# 1 - cancellation rate
p_eligible_intake <- 0.993
p_eligible_intake_FU <- 0.986

# time between cancel-reschedule
t_cancel_resch <- 2

# 1 - dna rate
p_dna_first <- 0.94
p_dna_FU <- 0.9

# follow up
# 1) time between 2 -> n appts (not DNA)
t_followup_rate <- 2.785

# 2) time between 2 -> n appts (after DNA)
t_followup_DNA_rate <- 2.842

# 2) n follow up appts
d_followup_shape <- 1.09
d_followup_scale <- 24.3

# arrival rate (1 patient every 2.22 days added to list)
d_arrival_rate <- 2.22

# initial waiting list size
initSize <- 55


# cancellation branch function
fn_eligible_intake <- function() {
    
    # Function to determine whether the individual is eligible to continue for testing at the
    # time of the intake, with the following output value that is used to determine the sub-
    # trajectory in the corresponding branch:
    #   0) continue to testing (0 = skip the branch)
    #   1) not eligible for testing (1 = enter the first sub-trajectory in the branch)
    # what prop of people have a subsequent follow up?
    
    out <- if(runif(1) <= p_eligible_intake) {0} else {1}
    
    return(out)
    
}

# cancellation at follow up branch function
fn_eligible_intake_FU <- function () {
    
    out <- if(runif(1) <= p_eligible_intake_FU) {0} else {1}
    
    return(out)
    
}

# dna at follow up branch function
fn_dna_first <- function () {
    
    out <- if(runif(1) <= p_dna_first) {0} else {1}
    
    return(out)
    
}

# dna at follow up branch function
fn_dna_FU <- function () {
    
    out <- if(runif(1) <= p_dna_FU) {0} else {1}
    
    return(out)
    
}

fn_distr_first <- function() {
    
    out <- rweibull(1, 0.98, 2.98)
    return(out)
    
}

chances <- function() { 
    x <- runif(1)
    out <- ifelse((x), 
                  ifelse((x) > 0.25, 1,
                         ifelse((x) > 0.01 & 
                                    (x) <= 0.25, 
                                2, 3)))
    return(out)
}

# summary dataframe function
fn_summarise <- fn_summarize <- function(sim_out, keys = NULL) {
    
    # This function summarizes monitored attribute values extracted from the simmer()
    # environment into a data.frame using the get_mon_attributes() function, which is provided
    # to function through the 'sim_out' argument, to their last recorded value per individual.
    # If no specific attributes are defined through the 'keys' argument, all attributes/keys
    # are summarized. Functions from the data.table package are used due to the potentially 
    # large size of 'sim_out'.
    
    if(is.null(keys)) keys <- unique(sim_out$key);
    
    df <- as.data.table(sim_out)[key %in% keys];
    setorder(df, name, time);
    df <- df[, .(value = value[.N]), by = list(name, key)];
    df <- dcast(df, name~key, value.var = "value");
    setcolorder(df, c("name", keys));
    
    return(df)
    
}

### ----------------------------------------------------------------------------

