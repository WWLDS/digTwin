### 
# Wrightington, Wigan & Leigh Digital Twin
#### Clinics Run by Sisters ####

# To do:
# - Understand Sisters patterns of work
# - Is it appropriate to treat all Sisters the same? Or need individual traj?
# - Bind Sisters traj to DERM clinic
# - Create col that flags whether they have at least one subsequent DERM visit

# Questions:
# - Do Sisters do different things? Can I treat as the same?
# - Only using Sisters that have had appts since 2024. Is this reasonable?
# - Bimodal distribution - why is this? Need to understand differences

# Simplifications:
# - Grouped Sisters together & GEETA
# - Grouped schedule per week rather than day
# - NEW only

### ----------------------------------------------------------------------------
# check if the package librarian installed then load librarian
# librarian is a package installation and management pacakage that merges CRAN, 
# GitHub and Bioconductor

if(!require(librarian)){
    install.packages("librarian")
    library(librarian)
}
librarian::shelf(tidyverse, here, glue, simmer, simmer.plot, data.table, MASS,
                 padr)


### ----------------------------------------------------------------------------

### ----------------------------------------------------------------------------
# import example dataset 
data <- read_csv(here("rawData/dermSimData.csv")) 

# filter to dermatology and select relevant features for simulation building
derm <- data |>
    filter(specialty_spec_desc %in% "Dermatology",
           time_to_be_seen_days < 2000,
           appointment_dt < "2024-07-01",
           !(appointment_dt == "2023-05-08"),
           !(appointment_dt == "2023-01-02")) |>
    dplyr::select(appointment_dt, day_of_week, referral_serial, 
                  appointment_type, referral_priority, in_person_appt, 
                  time_to_be_seen_days, date_letter_received_dt, clinic_code,
                  dna_flag, cancel_flag, DischargeDate, 
                  nhs_number, clinic_name) |>
    mutate(weekday = wday(appointment_dt),
           DischargeDate = ymd_hms(DischargeDate))

# understand SISTERS clinics
clinicLastAppt <- data |>
    filter(specialty_spec_desc %in% "Dermatology",
           time_to_be_seen_days < 2000,
           appointment_dt < "2024-07-01",
           !(appointment_dt == "2023-05-08"),
           !(appointment_dt == "2023-01-02")) |>
    dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                  appointment_dt) |>
    group_by(clinic_name) |>
    slice(which.max(appointment_dt))

clinicInfo <- data |>
    dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                  appointment_dt) |>
    group_by(clinic_name) |>
    summarise(count = n()) |>
    distinct(clinic_name, .keep_all = T)

join <- left_join(clinicInfo, clinicLastAppt, by = "clinic_name") |>
    dplyr::select(clinic_name, clinic_code, count, appointment_dt)

# pull only Sisters info
sisterOnly <- join |>
    filter(grepl(c("*BAC|SISTER|*GEETA"), clinic_name),
           appointment_dt > "2024-01-01") |>
    pull(clinic_name)

# filter derm df to only Sister clinics
sistersDf <- derm |>
    filter(clinic_name %in% sisterOnly)


#### INITIAL WL SIZE #### ------------------------------------------------------
wlSize <- data |>
    filter(appointment_type == "New",
           grepl(c("*BAC|SISTER|*GEETA"), clinic_name),
           appointment_dt > "2024-07-01") |>
    summarise(count = n())

#### CAPACITY #### -------------------------------------------------------------
# leaving this code chunk for future use when we increase complexity
# capacity <- sistersDf |>
#     mutate(clinic_name = str_replace(clinic_name, 
#                                      "JOANNE BACZYNSKI|SISTER BACZYNKSI",
#                                      "SISTER BACZYNSKI")) |>
#     filter(appointment_type == "New") |>
#     group_by(weekday, clinic_name, appointment_type, appointment_dt) |>
#     summarise(count = n()) |>
#     mutate(mean = mean(count),
#            sd = sd(count),
#            weekday = as.factor(weekday))

# to simplify: first take new appts and treat all Sisters as the same
capacity <- sistersDf |>
    filter(
           appointment_dt > "2023-09-01") |>
    group_by(weekday, appointment_dt) |>
    summarise(count = n())

# cap values
capacity |>
    mutate(weekday = as.factor(weekday),
           mean = mean(count),
           sd = sd(count)) |>
    # only one appt on a Monday so remove for simplicity
    # filter(!(weekday == 2)) |>
    distinct(mean, .keep_all = T)
    
# plot cap dist
ggplot(capacity, aes(x = count)) +
    geom_density()

# modelling
capDist <- capacity |>
    pull(count)
capDistSis <- fitdistr(capDist, "normal")
capDistSisMean <- unname(capDistSis$estimate[1]) /7*5
capDistSisSD <- unname(capDistSis$estimate[2]) /7*5
#### DEMAND #### ---------------------------------------------------------------
# demand of clinic (number of additions to list per day)
# Use rexp(1, 1/25)
# need to check if same people have multiple follow ups planned
# perPerson <- sistersDf |>
#     filter(appointment_type == "New",
#            appointment_dt > "2023-09-01") |>
#     group_by(referral_serial) |>
#     summarise(count = n())
# 
# # plot perPerson dist |>
# ggplot(perPerson, aes(x = count)) +
#     geom_density()
# 
# # per person values
# ppWei <- perPerson |>
#     pull(count)
# set to 1
# fitdistr(ppWei, "poisson")

# set to 1 New appt per person

# daily demand for New appts
demand <- sistersDf |>
    filter(appointment_type == "New",
           appointment_dt > "2023-09-01") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    ungroup() |>
    group_by(appointment_dt) |>
    summarise(count = n()) |>
    drop_na() |>
    pad(by = "appointment_dt") |>
    mutate(count = if_else(is.na(count), 0, count))

# plot demand
ggplot(demand, aes(x = count)) +
    geom_density()

# modelling
demDist <- demand |>
    pull(count)
demDist <- fitdistr(demDist, "poisson")
demDistRate <- unname(demDist$estimate[1])

#### WAIT TIME ON LIST #### ----------------------------------------------------
# overall wait for New appt
wait <- sistersDf |>
    filter(appointment_type == "New",
           appointment_dt > "2023-09-01") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt))

# plot wait
ggplot(wait, aes(x = time_to_be_seen_days)) +
    geom_density()

#modelling
waitDist <- wait |>
    pull(time_to_be_seen_days)
waitDist <- fitdistr(waitDist, "lognormal")
waitDistNurseMean <- unname(waitDist$estimate[1])
waitDistNurseSD <- unname(waitDist$estimate[2])


#### What prop have a follow up ? #### -----------------------------------------
propFU <- data |>
    filter(
           grepl(c("*BAC|SISTER|*GEETA"), clinic_name),
           !(is.na(DischargeDate))) |>
    group_by(referral_serial) |>
    summarise(count = n()) |>
    mutate(noFU = if_else(count == 1, 1, 0)) |>
    group_by(noFU) |>
    summarise(count = n()) |>
    mutate(prop = count / sum(count))
sisPropFU <- filter(propFU, noFU == 0)$prop


#### FOLLOW UP INFO #### -------------------------------------------------------
#### What prop have a follow up ? #### -----------------------------------------
propFU <- data |>
    filter(
        grepl(c("*BAC|SISTER|*GEETA"), clinic_name),
        !(is.na(DischargeDate))) |>
    group_by(referral_serial) |>
    summarise(count = n()) |>
    mutate(count = count - 1)


#modelling
fuCountDist <- propFU |>
    pull(count)
fuCountDistSister <- fitdistr(fuCountDist, "poisson")
fuCountDistSisterRate <- unname(fuCountDistSister$estimate[1])

#plot fuCountDr
ggplot(propFU, aes(x = count)) +
    geom_histogram()

rpois(100, fuCountDistDrRate)

# how long wait between follow ups

