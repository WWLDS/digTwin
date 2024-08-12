### 
# Wrightington, Wigan & Leigh Digital Twin
# Grouped Clinics Sim

# To do:
# - add FUs
# - fix capacity with schedules
# - cancellations / DNA
# - number of FUs
# - add DERM and immuno

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
    
    sim <- simmer("sim")
    
    trajDr <-
        
        trajectory() %>%
        
        set_attribute(tag = "Doctor", keys = "Doctor", 
                      values = 1, mon = "+") %>%
        
        branch(option = function() ifelse(get_attribute(.env = sim, 
                                                        "ApptCounter") == 0, 
                                          1, 2), continue = c(T,T),
               trajectory() %>%
                   timeout(task = function() rlogis(1, waitDistDrMean,
                                                    waitDistDrSD)),
               trajectory() %>%
                   timeout(task = function() rlogis(1, waitBetFUDrSisMean,
                                                    waitBetFUDrSisSD))
        ) %>%
        
        # Seize one unit of capacity
        seize(resource = "Doctor", 1) %>%
        # Hold one unit of capacity for one day
        timeout(task = t_intake) %>%
        # Release one unit of capacity
        release(resource = "Doctor", 1)
    
        
    trajNurse <-
        
        trajectory() %>%
        
        set_attribute(tag = "Nurse", keys = "Nurse", 
                      values = 1, mon = "+") %>%
        timeout(task = function() rlogis(1, waitDistNurseMean,
                                         waitDistNurseSD)) %>%
        # Seize one unit of capacity
        seize(resource = "Nurse", 1) %>%
        # Hold one unit of capacity for one day
        timeout(task = t_intake) %>%
        # Release one unit of capacity
        release(resource = "Nurse", 1)
    
    
    trajDerm <-
        
        trajectory() %>%
        
        set_attribute(tag = "Derm", keys = "Derm", 
                      values = 1, mon = "+") %>%
        
        # set FollowUp number now so it remains static throughout patient traj
        set_attribute(tag = "DermClinicFUs", keys = "DermClinicFUs", 
                      values = function() floor(rweibull(1, d_followup_shape,
                                                         d_followup_scale))) %>%
        
        set_attribute(tag = "FuToDo", keys = "FuToDo",
                      values = function() get_attribute(.env = sim, 
                                                        "DermClinicFUs") + get_attribute(.env = sim, "FuToDo")) %>%
        
        set_prioritization(c(1, NA, NA)) %>%
        
        # Wait period before subsequent follow up appts
        timeout(task = function() rpois(1, t_followup_rate)) %>%
        
        # does the patient cancel during the second wait?
        branch(option = function() fn_eligible_intake_FU(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() t_cancel_resch) %>%
                   set_attribute(keys = "Cancelled", values = 1, mon = "+")
        ) %>%
        
        # branch for patient dna. If dna then patient restart wait
        # do some dna's lead to discharge?
        branch(option = function() fn_dna_FU(), continue = c(T),
               trajectory() %>%
                   timeout(task = function() rpois(1,t_followup_DNA_rate)) %>%
                   set_attribute(keys = "Dna", values = 1, mon = "+")
        ) %>%
        
        # seize Intake to ensure clinic capacity being used for followUps too
        seize("Derm", 1) %>%
        
        # reduce number of FollowUps still required by one each time a FollowUp
        # has completed
        set_attribute("FuToDo", function() 
            get_attribute(.env = sim, "FuToDo") - 1) %>%
        # Hold one unit of capacity for one day
        timeout(task = t_intake) %>%
        # Release one unit of capacity
        release("Derm", 1) %>%
        # Rollback to the follow up timeout until FollowUp count has reduced
        # to zero
        rollback(target = 8,
                 check = function()
                     get_attribute(.env = sim,
                                   "FuToDo") > 0)
    
    
    trajMain <-
        
        trajectory() %>%
        
        set_prioritization(c(0, NA, NA)) %>%
        
        set_attribute(tag = "FuToDo", keys = "FuToDo", 
                      values = function() floor(rpois(1, 
                                                      fuCountDistDrRate))) %>%
        
        set_attribute(tag = "ApptCounter", keys = "ApptCounter", 
                      values = 0, mon = "+") %>%
        
        # record time patient initially received appt letter
        set_attribute(tag = "ToR", keys = "TimeOfRef", 
                      values = function() now(.env = sim)) %>%
        
        branch(
            option = function() ifelse(runif(1) < firstIsDr, 1, 2),
            continue = c(T,T),
            trajDr,
            trajNurse
        ) %>%
        
        set_attribute(tag = "ApptCounter", keys = "ApptCounter", 
                      values = 1, mon = "+") %>%
        
        branch(
            option = function() chances(), 
            continue = c(T,T,T),
            trajDr,
            trajNurse,
            trajDerm
        ) %>%
            
        set_attribute("FuCounter", function() 
            get_attribute(.env = sim, "ApptCounter") + 1) %>%
        
        set_attribute("FuToDo", function() 
            get_attribute(.env = sim, "FuToDo") - 1) %>%
        
        rollback(amount = 3,
                 check = function()
                     get_attribute(.env = sim,
                                   "FuToDo") > 0) %>%
        
        # Monitor time of discharge
        set_attribute(tag = "Discharge", keys = "Discharge", 
                      values = function() now(.env = sim), mod = "+") %>%
        seize(resource = "Discharge", 1) %>%
        release(resource = "Discharge", 1)


    # check traj plot
    print(plot(trajMain))
    
    # run simulation
    sim %>% 
        add_resource("Doctor", capacity = rnorm(1, 48, 1)) %>%
        add_resource("Nurse", capacity = rnorm(1, 13, 1)) %>%
        add_resource("Derm", capacity = schedule(timetable = c(1,2,3,4,5,6,7), 
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
        # add_generator(name_prefix = "Backlog", trajectory = trajMain, mon = 2,
        #               # understand initial list size
        #               distribution = at(rep(x = 0, 
        #                                     times = (initWlDr + 
        #                                                  initWlSister)))) %>%
        add_generator(name_prefix = "NewPatient", trajectory = trajMain, 
                      distribution = function() rweibull(1, 1/demDistDrShape,
                                                         1/demDistDrScale),
                      mon = 2)
    
    set.seed(3); 
    sim %>% 
        reset() %>% 
        run(until = 500,
            progress=progress::progress_bar$new()$update) %>% 
        wrap()
})

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
