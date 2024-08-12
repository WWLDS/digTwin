### 
# Wrightington, Wigan & Leigh Digital Twin


# Questions:
# - DERM is all follow up
# - need to choose better waiting time dist
# - three DERM clinics per week - are they different dists etc?

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
           appointment_dt < "2024-04-16",
           !(appointment_dt == "2023-05-08"),
           !(appointment_dt == "2023-01-02")) |>
    dplyr::select(appointment_dt, day_of_week, referral_serial, 
                  appointment_type, referral_priority, in_person_appt, 
                  time_to_be_seen_days, date_letter_received_dt, clinic_code,
                  dna_flag, cancel_flag, DischargeDate, nhs_number, clinic_name)

# DERM clinic only
dermClinic <- derm |>
    filter(clinic_code == "DERM",
           appointment_dt > "2023-09-01") |>
    mutate(weekday = wday(appointment_dt),
           DischargeDate = ymd_hms(DischargeDate)) |>
    filter(weekday >= 2 & weekday <= 6)

# to begin: patient traj = on list, wait for certain amount of time, 
# seize a resource, get held for a unit of time

#### Waiting list size #### ----------------------------------------------------
# using different file here
dateRange <- seq.Date(
    from= ymd("2023-09-02"),
    to= ymd("2024-01-01"),
    by="day")

wlData <- read_csv(here("rawData/wlSizeTest.csv"))
dermWL <- wlData |>
    filter(Specialty %in% "Dermatology") |>
    mutate(appointment_date = date(ymd_hms(appointment_date_dt))) |>
    mutate(test = if_else(date > appointment_date, 1, 0)) |>
    filter(test == 0) |>
    group_by(date) |>
    summarise(count = n())

listDate <- list()
for(i in dateRange) {
    dermList <- derm |>
        filter(date_letter_received_dt < i) |>
        mutate(test = if_else(appointment_dt <= i, 0, 1)) |>
        filter(test == 1) |>
        count() |>
        mutate(date = i)
    
    listDate[[paste0(i)]] <- dermList
    wlKey <- bind_rows(listDate)
        
}

# use actual DERM file
dermList <- data |>
    filter(appointment_dt > "2024-04-16",
           specialty_spec_desc %in% "Dermatology",
           clinic_code == "DERM") |>
    group_by(referral_serial) |>
    count()

#### DEMAND #### ---------------------------------------------------------------
# demand of clinic (number of additions to list per day)
# Use rexp(1, 1/25)
# need to check if same people have multiple follow ups planned
perPerson <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    summarise(count = n())

# plot perPerson dist |>
    ggplot(perPerson, aes(x = count)) +
               geom_density()
# per person values
ppWei <- perPerson |>
    pull(count)
fitdistr(ppWei, "weibull")
?fitdistr
# demand for first follow up
demand <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    slice(which.min(date_letter_received_dt)) |>
    ungroup() |>
    pad(by = "appointment_dt")
demandMean <- demand |>
    summarise(count = n()) |>
    mutate(test = naCount - (count + naCount)) #|>
    mutate(test2 = test / count)
277/119
naCount <- demand |>
    group_by(day_of_week) |>
    count() |>
    filter(is.na(day_of_week)) |>
    pull(n)
# dem values
demVal <- demand |>
    distinct(mean, .keep_all = T)
count = 322
naCount = 177
322/145

# interarrival time
demandInter <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    group_by(date_letter_received_dt) |>
    filter(date_letter_received_dt > "2023-01-01") |>
    summarise(count = n()) |>
    pad(by = "date_letter_received_dt") |>
    mutate(count = if_else(is.na(count), 0, count)) |>
    pull(count)

ggplot(demandInter, aes(x = count)) +
    geom_histogram()
fitdistr(demandInter, "poisson")


# weibull
demWei <- demand |>
    pull(count)
fitdistr(demWei, "weibull")

# plot cap dist
ggplot(demand, aes(x = count)) +
    geom_density()


#### WAIT TIME ON LIST #### ----------------------------------------------------

# overall wait for first follow up appt
wait <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    group_by(time_to_be_seen_days) |>
    summarise(count = n()) |>
    mutate(mean = mean(time_to_be_seen_days),
           sd = sd(time_to_be_seen_days)) |>
    ungroup()

# wait values
waitVal <- wait |>
    distinct(mean, .keep_all = T)

# weibull
waitWei <- wait |>
    pull(time_to_be_seen_days)
fitdistr(waitWei, "weibull")
    
# plot wait dist
ggplot(wait, aes(x = time_to_be_seen_days)) +
    geom_density()

# wait between subsequent appts (after an attendance)
waitBet <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    mutate(dayDiff = (time_to_be_seen_days) - lag(time_to_be_seen_days)) |>
    drop_na() |>
    filter(dayDiff != 0,
           dna_flag == 0)

waitVal <- waitBet |>
    group_by(dayDiff) |>
    summarise(count = n())

# poisson
waitBetWei <- waitBet |>
    pull(dayDiff)
fitdistr(waitBetWei, "poisson")

ggplot(waitBet, aes(x = dayDiff)) +
    geom_density()



#### CAPACITY #### -------------------------------------------------------------
# will need to choose more suitable dist in future
# capacity of clinic
capacity <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(weekday, appointment_dt) |>
    summarise(count = n()) |>
    mutate(mean = mean(count),
           sd = sd(count),
           weekday = as.factor(weekday)) |>
    ungroup() #|>
    pull(count)
fitdistr(capacity, "normal")
# cap values
capVal <- capacity |>
    distinct(weekday, .keep_all = T)
    
# plot cap dist
ggplot(capacity, aes(x = count, colour = weekday)) +
    geom_density()


#### PRIORITY ####
# is priority remained over followups? Assuming yes at the moment. Need to check
prio <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    group_by(referral_priority, time_to_be_seen_days, .drop = F) |>
    summarise(count = n()) |>
    mutate(mean = mean(time_to_be_seen_days)) |>
    mutate(prop = count / sum(count))

prioPullRoutine <- prio |>
    filter(referral_priority == "Routine") |>
    pull(time_to_be_seen_days)
fitdistr(prioPullRoutine, "weibull")

prioPullUrgent <- prio |>
    filter(referral_priority == "Urgent") |>
    pull(time_to_be_seen_days)
fitdistr(prioPullUrgent, "weibull")

wait <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    group_by(time_to_be_seen_days) |>
    summarise(count = n()) |>
    mutate(mean = mean(time_to_be_seen_days),
           sd = sd(time_to_be_seen_days)) |>
    ungroup()

ggplot(prio, aes(x = time_to_be_seen_days, colour = referral_priority)) +
    geom_density()

fitdistr(prioPull, "weibull")

# end
#### CANCELLATIONS #### --------------------------------------------------------
cancel <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    dplyr::select(cancel_flag, appointment_dt, referral_serial, 
                  time_to_be_seen_days)

# work out cancellation rate of first appt
cancelFirst <- cancel |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) |>
    group_by(cancel_flag) |>
    summarise(count = n()) |>
    mutate(prop = count/sum(count))

# work out cancellation rate of subsequent appt
subOnly <- cancel |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    slice(which.min(appointment_dt))
cancelSub <- anti_join(cancel, subOnly, by = c("referral_serial", 
                                               "appointment_dt"))
cancelSub |>    
    group_by(cancel_flag) |>
        summarise(count = n()) |>
        mutate(prop = count/sum(count))

# work out time between cancel and reschedule - NOT PRIORITY YET as so few cancels
# do the third point for both first and subsequent

# time between cancel-reschedule for second appt -> n
cancelWaitBet <- cancelSub |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    mutate(dayDiff = (time_to_be_seen_days) - lag(time_to_be_seen_days)) |>
    drop_na() |>
    filter(dayDiff != 0,
           cancel_flag == 1)

cancelWaitBet |>
    group_by(dayDiff) |>
    summarise(count = n())

# time between cancel-reschedule for first appt -> second appt
cancelWaitBet <- cancel |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    mutate(dayDiff = (time_to_be_seen_days) - lag(time_to_be_seen_days)) |>
    # drop_na() |>
    filter(
           cancel_flag == 1)


#### DNAs #### -----------------------------------------------------------------
dna <- dermClinic |>
    filter(appointment_type == "Follow Up") |>
    dplyr::select(dna_flag, appointment_dt, referral_serial, time_to_be_seen_days)

# work out dna rate of first appt
dnaFirst <- dna |>
    group_by(referral_serial) |>
    slice(which.min(appointment_dt)) #|>
    group_by(dna_flag) |>
    summarise(count = n()) |>
    mutate(prop = count/sum(count))

# work out dna rate of subsequent appt
subOnly <- dna |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    slice(which.min(appointment_dt))
dnaSub <- anti_join(dna, subOnly, by = c("referral_serial", 
                                               "appointment_dt"))

dnaSub |>
    group_by(dna_flag) |>
    summarise(count = n()) |>
    mutate(prop = count/sum(count))

# time between dna and next appt
dnaWaitBet <- dnaSub |>
    group_by(referral_serial) |>
    arrange(appointment_dt) |>
    mutate(dayDiff = (time_to_be_seen_days) - lag(time_to_be_seen_days)) |>
    drop_na() |>
    filter(dayDiff != 0,
           dna_flag == 1)

dnaWaitBet |>
    group_by(dayDiff) |>
    summarise(count = n())

# poisson
waitBetWei <- dnaWaitBet |>
    pull(dayDiff)
fitdistr(waitBetWei, "poisson")

ggplot(dnaWaitBet, aes(x = dayDiff)) +
    geom_density()

# no pt in DERM discharged due to DNA


#### ROUTE OF REFERRAL #### ----------------------------------------------------
referralData <- data |>
    filter(clinic_code == "DERM",
           appointment_dt < "2024-04-16",
           time_to_be_seen_days < 2000,
           !(appointment_dt == "2023-05-08"),
           !(appointment_dt == "2023-01-02")) |>
    group_by(source_of_ref_description, source_of_ref_main_code) |>
    summarise(count = n())


#### CLINIC ROUTE #### ---------------------------------------------------------
dermClinic <- derm |>
    filter(
           appointment_dt > "2021-01-01") |>
           # referral_serial %in% "WI_463925||18||202101280001") |>
    mutate(weekday = wday(appointment_dt),
           DischargeDate = ymd_hms(DischargeDate))

# referralSerial <- derm |>
#     distinct(referral_serial) |>
#     pull(referral_serial)
# list_test <- list()
# for(i in referralSerial) {
#     # determine the route for referrals that include DERM clinic
#     dermClinicOrder <- dermClinic |>
#         filter(referral_serial == i) |>
#         dplyr::select(referral_serial, nhs_number, appointment_dt, appointment_type, 
#                       clinic_code) |>
#         arrange(appointment_dt) |>
#         group_by(referral_serial, .drop = F) |>
#         mutate(apptNum = seq_along(appointment_dt)) |>
#         # group_by(referral_serial, nhs_number, .drop = F) |>
#         pivot_wider(id_cols = c("nhs_number", 
#                                 "referral_serial",
#                                 "appointment_type"), 
#                     names_from = apptNum, values_from = clinic_code) #|>
#     # filter(appointment_type == "New",
#     #        !(is.na(`1`))) #|>
#     # filter_all(any_vars(grepl("DERM", .)))
#     
#     if(dermClinicOrder$appointment_type[1] == "New") {
#         test2 <- dermClinic |>
#             filter(referral_serial == i) |>
#             dplyr::select(referral_serial, nhs_number, appointment_dt, appointment_type, 
#                           clinic_code) |>
#             arrange(appointment_dt) |>
#             group_by(referral_serial, .drop = F) |>
#             mutate(apptNum = seq_along(appointment_dt)) |>
#             # group_by(referral_serial, nhs_number, .drop = F) |>
#             pivot_wider(id_cols = c("nhs_number", 
#                                     "referral_serial"), 
#                         names_from = apptNum, values_from = clinic_code)
#         
#     }
#     list_test[[paste0(i)]] <- test2
#     test_keys <- bind_rows(list_test)
# }
dermClinicOrder$appointment_type[1] == "New"
dermClinicOrder$appointment_type

dermCount <- derm |>
    dplyr::select(referral_serial, nhs_number, appointment_dt, appointment_type, 
                  clinic_code, DischargeDate) |>
    # group_by(referral_serial) |>
    mutate(DERM = case_when(
        clinic_code == "DERM" ~ 1,
        .default = 0
    )) |>
    group_by(referral_serial, DERM) |>
    summarise(apptCount = n()) |>
    # filter(clinic_code %in% "DERM") |>
    mutate(dermFreq = (1 - apptCount/sum(apptCount)),
           total_visits = sum(apptCount)) |>
    mutate(dermFreq = if_else((DERM == 0 & dermFreq == 1), 
                                0, dermFreq)) |>
    dplyr::select(-c(DERM, apptCount)) |>
    distinct(referral_serial, .keep_all = T)
    # mutate(prop = dermCount / sum(dermCount))




    # determine the route for referrals that include DERM clinic
    dermClinicOrder <- derm |>
        dplyr::select(referral_serial, nhs_number, appointment_dt, appointment_type, 
                      clinic_code, DischargeDate, clinic_name) |>
        filter(!(clinic_code %in% clinicLastAppt)) |>
        filter(!(clinic_code %in% "T4PSTDI")) |>
        filter(!(clinic_code %in% "LYE/TEL")) |>
        # filter(!(is.na(DischargeDate))) |>
        arrange(appointment_dt) |>
        group_by(referral_serial, .drop = F) |>
        
        mutate(apptNum = seq_along(appointment_dt)) |>
        # mutate(clinicGrouped = case_when(
        #     grepl(c("*L1"), clinic_code) ~ "L1",
        #     grepl(c("*L2"), clinic_code) ~ "L2",
        #     grepl(c("*L3"), clinic_code) ~ "L3",
        #     grepl(c("*L4"), clinic_code) ~ "L4",
        #     grepl(c("*L5"), clinic_code) ~ "L5",
        #     grepl(c("*W1"), clinic_code) ~ "W1",
        #     grepl(c("*W5"), clinic_code) ~ "W5",
        #     grepl(c("*MOP"), clinic_code) ~ "MOP",
        #     grepl(c("*DERM"), clinic_code) ~ "DERM",
        #     grepl(c("*TELEDERM"), clinic_code) ~ "TELEDERM",
        #     .default = "0"
        # )) |>
        mutate(clinicGrouped = case_when(
            grepl(c("*DR"), clinic_name) ~ "Dr",
            grepl(c("*SISTER"), clinic_name) ~ "Sister",
            grepl(c("*SPECIALIST"), clinic_name) ~ "Specialist",
            grepl(c("*JOA"), clinic_name) ~ "Sister",
            grepl(c("*IMMUN"), clinic_name) ~ "Immuno",
            grepl(c("GEETA"), clinic_name) ~ "Sister",
            .default = "0"
        )) |>
        filter(!(clinicGrouped %in% "0")) |>
        # group_by(referral_serial, nhs_number, .drop = F) |>
        pivot_wider(id_cols = c("nhs_number", 
                                "referral_serial",
                                ), 
                    names_from = apptNum, values_from = clinicGrouped) |>
        filter(!(`1` %in% "Immuno"),
               !(`1` %in% "Specialist")) |>
        ungroup()
        
# sankey clinic order
# install.packages("devtools")
# install.packages("rtools")
# remotes::install_github("davidsjoberg/ggsankey")
# devtools::install_github("davidsjoberg/ggsankey")
# shelf(networkD3)
# shelf(plotly)

prepSan <- dermClinicOrder |>
    dplyr::select(c(3:25)) |>
    rename_with( ~ paste0("clinic", .x))
    
links <-
    prepSan %>%
    mutate(row = row_number()) %>%  # add a row id
    pivot_longer(-row, names_to = "column", values_to = "source") %>%  # gather all columns
    mutate(column = match(column, names(prepSan))) %>%  # convert col names to col ids
    group_by(row) %>%
    mutate(target = lead(source, order_by = column)) %>%  # get target from following node in row
    ungroup() %>% 
    filter(!is.na(target))

links <-
    links %>%
    mutate(source = paste0(source, '_', column)) %>%
    mutate(target = paste0(target, '_', column + 1)) %>%
    select(source, target) |>
    filter(!(grepl(c("*NA"), source)))

nodes <- data.frame(name = unique(c(links$source, links$target)))
nodes$label <- sub('_[0-9]*$', '', nodes$name)
# nodes <- nodes |>
#     filter(!(grepl(c("*NA"), name))) # remove column id from node label

links$source_id <- match(links$source, nodes$name) - 1
links$target_id <- match(links$target, nodes$name) - 1
links$value <- 1

library(networkD3)

sankeyNetwork(Links = links, Nodes = nodes, Source = 'source_id',
              Target = 'target_id', Value = 'value', NodeID = 'label')

   
COtest <- dermClinicOrder |>
    mutate(dermFlag = (if_any(starts_with("DERM"),  ~. %in% c(3:300))))
clinicLastAppt <- data |>
    dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                  appointment_dt) |>
    group_by(clinic_name) |>
    slice(which.max(appointment_dt)) |>
    filter(appointment_dt < "2023-01-01") |>
    pull(clinic_code)

    clinicInfo <- data |>
        dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                      appointment_dt) |>
        group_by(clinic_code) |>
        summarise(count = n()) |>
        distinct(clinic_code, .keep_all = T)
joinTest <- left_join(clinicInfo, clinicLastAppt, by = "clinic_name") |>
    dplyr::select(clinic_name, count, appointment_dt)

sisterOnly <- joinTest |>
    filter(grepl(c("*BAC|SISTER"), clinic_name)) |>
    pull(clinic_name)
    
# first <- dermClinicOrder |>
#     group_by(`2`) |>
#     summarise(count = n()) |>
#     mutate(prop = count / sum(count))
    
summaryOrder <- dermClinicOrder |>
    pivot_longer(cols = c(3:110), names_to = "clinicNum", values_to = "clinic") |>
    group_by(clinic, clinicNum) |>
    summarise(count = n())

ggplot(summaryOrder, aes(x =))

