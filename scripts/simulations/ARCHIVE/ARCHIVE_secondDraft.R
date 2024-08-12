simmer_wrapper <- function(i) {
    
    sim_start_date<- ymd("2024-01-01")
    sim_end_date<- ymd("2024-01-03")
    sim_start_num<-as.numeric(sim_start_date)
    sim_end_num<-as.numeric(sim_end_date)

env <- simmer("Dermatology")

?set_global
# transferout<-trajectory() %>%
#     set_global("leave",1,mod="+")
?renege_in
newPatient <- 
    # trajectory() %>%
    # branch(
    #     function() sample(c(1,2), size=1, replace=T, prob=c(0.32,0.67)), 
    #     continue=c(T, T), 
        trajectory("New patients") %>%
            log_("New on list") %>%
            timeout(function() rexp(1, 1/48)) %>%
            log_("New wait finished") %>%
            seize("clinician") %>%
            timeout(1) %>%
            release_all("clinician")#,
    #     trajectory("Follow Up") %>%
    #         log_("New on list") %>%
    #         timeout(function() rexp(1, 1/37)) %>%
    #         log_("New wait finished") %>%
    #         seize("clinician") %>%
    #         timeout(1) %>%
    #         release_all("clinician")
    # )
fuPatient <-
    trajectory("Follow Up") %>%
    log_("New on list") %>%
    timeout(function() rexp(1, 1/37)) %>%
    log_("New wait finished") %>%
    seize("clinician") %>%
    timeout(1) %>%
    release_all("clinician")
## Display the trajectory for sense-checking (remove before running this multiple times!)  
print(plot(patient))
?from_to
env %>% 
    add_generator("New", patient, function() rexp(1,16), mon = 2) %>%  ## Type 1
    add_generator("FU", patient, function() rexp(1,18), mon = 2) %>%   ## Type 2
    add_resource("clinician", 1) %>% 
    run(until = 100) %>% 
    wrap()
}
?add_resource
envs<-simmer_wrapper()
test <- envs %>% get_mon_arrivals()
print(plot(get_mon_resources(envs),steps=TRUE))
print(plot(get_mon_arrivals(envs), steps = TRUE))
print(plot(get_mon_attributes(envs), steps = TRUE))
plot(get_mon_arrivals(envs))
