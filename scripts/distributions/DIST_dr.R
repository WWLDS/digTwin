### 
# Wrightington, Wigan & Leigh Digital Twin
#### Clinics Run by Dr ####

# To do:
# - Understand Dr
# - 

# Questions:
# - Do Dr's see different patients?
# - Only using Dr that have had appts since 2024. Is this reasonable?

# Simplifications:
# - Grouped All Dr together
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
           # time_to_be_seen_days < 2000,
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

# understand Dr clinics
clinicLastAppt <- data |>
    filter(specialty_spec_desc %in% "Dermatology",
           # time_to_be_seen_days < 2000,
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

# pull only Dr info
drOnly <- join |>
    filter(grepl(c("*DR"), clinic_name),
           appointment_dt > "2024-01-01") |>
    pull(clinic_name)

# filter derm df to only Dr clinics
drDf <- derm |>
    filter(clinic_name %in% drOnly)


#### INITIAL WL SIZE #### ------------------------------------------------------
wlSize <- data |>
    filter(appointment_type == "New",
           grepl(c("*DR"), clinic_name),
           appointment_dt > "2024-07-01") |>
    summarise(count = n())


#### CAPACITY #### -------------------------------------------------------------
capacity <- drDf |>
    filter(
           appointment_dt > "2023-09-01") |>
    group_by(referral_serial, appointment_dt) |>
    distinct(referral_serial, .keep_all = T) |>
    mutate(weekday = as.factor(weekday)) |>
    group_by(weekday, appointment_dt) |>
    summarise(count = n())

# cap values
capacity |>
    mutate(weekday = as.factor(weekday),
           mean = mean(count),
           sd = sd(count)) |>
    distinct(mean, .keep_all = T)

# plot cap dist
ggplot(capacity, aes(x = count, colour = weekday)) +
    geom_density()

# modelling
capDist <- capacity |>
    pull(count)
capDistDr <- fitdistr(capDist, "normal")
capDistDrMean <- unname(capDistDr$estimate[1]) /7*5
capDistDrSD <- unname(capDistDr$estimate[2]) /7*5
capDistDr

#### DEMAND #### ---------------------------------------------------------------
# daily demand for New appts
demand <- derm |>
    filter(appointment_type == "New",
           appointment_dt > "2023-09-01") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    ungroup() |>
    group_by(appointment_dt) |>
    summarise(count = n()) |>
    drop_na() #|>
    pad(by = "appointment_dt") |>
    mutate(count = if_else(is.na(count), 0, count))

# plot demand
ggplot(demand, aes(x = count)) +
    geom_density()

# modelling
demDist <- demand |>
    pull(count)
demDist <- fitdistr(demDist, "normal")
demDistDrShape <- unname(demDist$estimate[1])
demDistDrScale <- unname(demDist$estimate[2])


#### WAIT TIME ON LIST #### ----------------------------------------------------
# overall wait for New appt
referral_priority <- drDf |>
    distinct(referral_priority) |>
    pull()
for(i in referral_priority) {
    wait <- drDf |>
        filter(appointment_type == "New",
              appointment_dt > "2023-09-01",
              referral_priority == "Routine") |>
        group_by(referral_serial) |>
        slice(which.min(appointment_dt))

    # plot wait
    test <- ggplot(wait, aes(x = time_to_be_seen_days)) +
        geom_density() +
        labs(title = i)
    print(test)

    #modelling
    waitDist <- wait |>
        filter(time_to_be_seen_days > 0) |> 
                   # time_to_be_seen_days < 15) |>
        pull(time_to_be_seen_days) 
    
    waitDistDr <- fitdistr(waitDist, "normal")
    waitDistDrMean <- unname(waitDistDr$estimate[1])
    cat("\nDist", i, "\n=", waitDistDrMean)
    waitDistDrSD <- unname(waitDistDr$estimate[2])
    cat("\nDist", i, "\n=", waitDistDrSD)
}
test <- as.data.frame(pmax(0, rnorm(100, 10, 3.4))) |>
    rename("col" = 1) |>
    ggplot(aes(x = col)) +
    geom_histogram()
test

#### Priorities #### -----------------------------------------------------------
prioDr <- drDf |>
    group_by(referral_priority) |>
    summarise(count = n()) |>
    mutate(prop = count/sum(count)) |>
    dplyr::select(-count)

# to understand - done
prioUnder <- drDf |>
    group_by(referral_serial, referral_priority) |>
    summarise(count = n()) |>
    mutate(prop = count / sum(count))
# seems like a referral keeps the same priority throughout traj

#### What prop have a follow up ? #### -----------------------------------------
propFU <- data |>
    filter(
        grepl(c("*DR"), clinic_name)) |>
        # !(is.na(DischargeDate))) |>
    group_by(referral_serial) |>
    summarise(count = n()) |>
    mutate(count = count - 1) |>
    dplyr::select(-referral_serial) |>
    ungroup()

mean <- propFU |>
    summarise(mean = mean(count))
var <- propFU |>
    summarise(var = var(count))

#modelling
fuCountDist <- propFU |>
    pull(count)
fuCountDistDr <- fitdistr(fuCountDist, "poisson")
fuCountDistDrRate <- unname(fuCountDistDr$estimate[1])

#plot fuCountDr
ggplot(propFU, aes(x = count)) +
    geom_histogram()

rpois(100, fuCountDistDrRate)
#### NUMBER OF FOLLOW UPS #### -------------------------------------------------
noFuDr <- drDf |>
        filter(appointment_dt > "2023-09-01") |>
        group_by(referral_serial) |>
        summarise(count = n() - 1)

ggplot(noFuDr, aes(x = count)) +
    geom_histogram()


#### WAIT BETWEEN FOLLOW UPS
# wait between subsequent appts (after an attendance)
waitBet <- derm |>
    filter(grepl(c("*DR|*BAC|SISTER|*GEETA"), clinic_name)) |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    mutate(dayDiff = (time_to_be_seen_days) - lag(time_to_be_seen_days)) |>
    drop_na() |>
    filter(dayDiff != 0,
           dayDiff != -1,
           dna_flag == 0)

waitVal <- waitBet |>
    group_by(dayDiff) |>
    summarise(count = n())

# dist
waitBetFUDrSis <- waitBet |>
    pull(dayDiff)
waitBetFUDrSis <- fitdistr(waitBetFUDrSis, "weibull")
waitBetFUDrSisMean <- unname(waitBetFUDrSis$estimate[1])
waitBetFUDrSisSD <- unname(waitBetFUDrSis$estimate[2])

# plot
ggplot(waitBet, aes(x = dayDiff)) +
    geom_density()

