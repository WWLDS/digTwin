### 
# Wrightington, Wigan & Leigh Digital Twin
# DERM single clinic only


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
# start simulation
simmer_wrapper <- function(i) {
    
    sim <- simmer("sim")
    
    trajMain <-
        
        trajectory() %>%
        
        # patient has lowest prioritisation before first DERM appt
        set_prioritization(c(0, 7, T)) %>%
        
        # set FollowUp number now so it remains static throughout patient traj
        set_attribute(tag = "Follow Up", keys = "FollowUp", 
                      values = function() floor(rweibull(1, d_followup_shape,
                                                         d_followup_scale))) %>%
        
        # record time patient initially received appt letter
        set_attribute(tag = "ToR", keys = "TimeOfRef", 
                      values = function() now(.env = sim)) %>%
        
        # waits an amount of time before first follow up
        timeout(task = function() rweibull(1, 
                                        d_intake_shape, 
                                        d_intake_scale)) %>%
        
        # branch for patient cancellation. If cancel then patient restart wait
        # fully. This may be incorrect? Do cancelled patients get seen quickly?
        branch(option = function() fn_eligible_intake(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() t_cancel_resch) %>%
                   ## understand distribution of cancellations - timeout before cancellation
                   set_attribute(keys = "Cancelled", values = 1, mon = "+") #%>%
                   # rollback(target = 4, times = 1)
        ) %>%
        
        # branch for patient dna. If dna then patient restart wait
        # fully. This may be incorrect? Do dna patients get seen quickly?
        # do some dna's lead to discharge?
        branch(option = function() fn_dna_first(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() rpois(1, t_followup_DNA_rate)) %>%
                   set_attribute(keys = "Dna", values = 1, mon = "+") #%>%
        ) %>%
        
        # Seize one unit of capacity
        seize(resource = "Intake", 1) %>%
        # Hold one unit of capacity for one day
        timeout(task = t_intake) %>%
        # Release one unit of capacity
        release(resource = "Intake", 1) %>%
        
        # once patient had first appt they have highest priority for all 
        # subsequent appts
        set_prioritization(c(1, 7, T)) %>%
        
        # Wait period before subsequent follow up appts
        timeout(task = function() rpois(1, t_followup_rate)) %>%
        
        # does the patient cancel during the second wait?
        branch(option = function() fn_eligible_intake_FU(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() t_cancel_resch) %>%
                   set_attribute(keys = "Cancelled", values = 1, mon = "+") #%>%
                   # rollback(target = 4, times = 1)
        ) %>%
        
        # branch for patient dna. If dna then patient restart wait
        # do some dna's lead to discharge?
        branch(option = function() fn_dna_FU(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() rpois(1,t_followup_DNA_rate)) %>%
                   set_attribute(keys = "Dna", values = 1, mon = "+") #%>%
                   # rollback(target = 5, times = 1)
        ) %>%
        
        # seize Intake to ensure clinic capacity being used for followUps too
        seize("Intake", 1) %>%
        
        # reduce number of FollowUps still required by one each time a FollowUp
        # has completed
        set_attribute("FollowUp", function() 
            get_attribute(.env = sim, "FollowUp") - 1) %>%
        # Hold one unit of capacity for one day
        timeout(task = t_intake) %>%
        # Release one unit of capacity
        release("Intake", 1) %>%
        # Rollback to the follow up timeout until FollowUp count has reduced
        # to zero
        rollback(target = 7,
                 check = function()
                     get_attribute(.env = sim,
                                   "FollowUp") > 0) %>%
        
        # Monitor time of discharge
        set_attribute(tag = "Discharge", keys = "Discharge", 
                      values = function() now(.env = sim), mod = "+") %>%
        seize(resource = "Discharge", 1) %>%
        release(resource = "Discharge", 1)
    
    # check traj plot
    print(plot(trajMain))
    ??simmer.plot

    # run simulation
    sim %>% 
        add_resource("Intake", capacity = schedule(timetable = c(1,2,3,4,5,6,7), 
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
        add_resource("FollowUp branch", capacity = Inf) %>%
        add_generator(name_prefix = "Initial", trajectory = trajMain, mon = 2,
                      # understand initial list size
                      distribution = at(rep(x = 0, times = initSize))) %>%
        add_generator(name_prefix = "NewOnList", trajectory = trajMain, 
                      distribution = function() rpois(1, d_arrival_rate),
                      mon = 2)
                      # from_to(0, 1,
                      #         function() floor(rweibull(1, 1/0.98, 1/0.5)),
                      #         arrive = FALSE,
                      #         every = 1))

    set.seed(3); 
    sim %>% 
        reset() %>% 
        run(until = 2000,
            progress=progress::progress_bar$new()$update) %>% 
        wrap()
}

# Check results section
envs<-simmer_wrapper()
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


plot(res, "usage", names = "Intake", items = c("queue", "system", steps = T))
plot(res, "utilization", names = "Intake", items = c("queue", "server", steps = T))
plot(arr, metric = "waiting_time")
plot(arr, metric = "flow_time")

n_arrivals<-arr 
n_arrivals$waiting <- n_arrivals$end_time - n_arrivals$start_time - n_arrivals$activity_time
n_arrivals$ALOS<-n_arrivals$activity_time


p3<-plot(arr, type = "waiting_time")
p3<-p3 + geom_hline(yintercept=mean(arrivals_no_resource$waiting), colour="red", lwd=1)
p3<-p3 + geom_hline(yintercept=mean(n_arrivals$waiting), colour="green", lwd=1)
plot(p3)

p4<-ggplot(data=n_arrivals, aes(n_arrivals$ALOS)) +
    geom_histogram() +
    geom_density(col=2) +
    labs(title="Length of Stay") +
    labs(x="Stay", y="Count")
p4

# Summarizing the recorded attributes using the custom function
df_attributes <- get_mon_attributes(envs) |>
    mutate(time = floor(time))
df_arrivals <- get_mon_arrivals(envs)
df_out <- fn_summarise(df_attributes)

