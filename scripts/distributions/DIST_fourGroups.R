### 
# Wrightington, Wigan & Leigh Digital Twin


# To do:
# - Work out prop that are New Dr and New Sister. Then work out prop that go to each route from the previous
# - 
# - 
# - 

# Questions:
# - 
# - 
# - 

# Simplifications:
# - Group all Dr, all Nurses. Four groups only.
# - 
# - 

### ----------------------------------------------------------------------------
# check if the package librarian installed then load librarian
# librarian is a package installation and management pacakage that merges CRAN, 
# GitHub and Bioconductor

if(!require(librarian)){
    install.packages("librarian")
    library(librarian)
}
librarian::shelf(tidyverse, here, glue, simmer, simmer.plot, data.table, MASS,
                 padr, networkD3)

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


### ----------------------------------------------------------------------------
# create vector of old clinics that should be removed from simulation
clinicLastAppt <- data |>
    dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                  appointment_dt) |>
    group_by(clinic_name) |>
    slice(which.max(appointment_dt)) |>
    filter(appointment_dt < "2023-01-01") |>
    pull(clinic_code)

# count number of appts per clinic
clinicInfo <- data |>
    dplyr::select(clinic_code, clinic_name, core_hrg, core_hrg_desc, hrg_desc,
                  appointment_dt) |>
    group_by(clinic_code) |>
    summarise(count = n()) |>
    distinct(clinic_code, .keep_all = T)


# joinTest <- left_join(clinicInfo, clinicLastAppt, by = "clinic_name") |>
#     dplyr::select(clinic_name, count, appointment_dt)

### ----------------------------------------------------------------------------
# number of appointments for each referral
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


# ordered appointments in each referral
# here we're creating four groups only. Uncomment to expand groups in the future
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


### Sankey ---------------------------------------------------------------------
prepSan <- dermClinicOrder |>
    dplyr::select(c(3:6)) |>
    rename_with( ~ paste0("clinic", .x))

# create links
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
    dplyr::select(source, target) |>
    filter(!(grepl(c("*NA"), source)))

# create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))
nodes$label <- sub('_[0-9]*$', '', nodes$name) # remove col id from node label

links$source_id <- match(links$source, nodes$name) - 1
links$target_id <- match(links$target, nodes$name) - 1
links$value <- 1

# create Sankey plot
sankeyNetwork(Links = links, Nodes = nodes, Source = 'source_id',
              Target = 'target_id', Value = 'value', NodeID = 'label')

# prop of patients that move to each subsequent clinic
propEachBranch <- links |>
    group_by(source, target, .drop = F) |>
    summarise(count = n()) |>
    mutate(prop = count / sum(count))

# prop of first appt (NEW) being Dr or Nurse
newClinicProp <- links |>
    mutate(node = case_when(
        grepl(c("*1"), source) ~ "1",
        .default = "0"
    )) |>
    filter(node == 1) |>
    group_by(source) |>
    summarise(count = n()) |>
    mutate(prop = count / sum(count))
firstIsDr <- filter(newClinicProp, source %in% "Dr_1")$prop

