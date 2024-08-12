### 
# Wrightington, Wigan & Leigh Digital Twin


# Questions:
# - Dermatology in Leigh and Wrightington

### ----------------------------------------------------------------------------
# check if the package librarian installed then load librarian
# librarian is a package installation and management pacakage that merges CRAN, 
# GitHub and Bioconductor

if(!require(librarian)){
    install.packages("librarian")
    library(librarian)
}

librarian::shelf(tidyverse, here, glue, simmer, simmer.plot, data.table)


### ----------------------------------------------------------------------------
patient <-
    trajectory() %>%
    branch(
        function() sample(c(1,2), size=1, replace=T, prob=c(0.32,0.67)), 
        continue=c(T, T), 
    trajectory("New patients") %>%
        set_attribute("new", function() rexp(1, 1/48)) %>%
        timeout_from_attribute("new") %>%
        log_("New"),
    trajectory("Follow Up") %>%
        set_attribute("fu", function() rexp(1, 1/37)) %>%
        timeout_from_attribute("fu") %>%
        log_("Follow Up"))

hospital <-
    simmer("hospital") %>%
    add_generator("Patient", patient, function() rexp(1, 22), mon = 2)

hospital %>% run(until = 1000)

table <- hospital %>% get_mon_arrivals() |>
    mutate(start_time = floor(start_time),
           end_time = floor(end_time),
           activity_time = floor(activity_time))
table <-
tableAttr <- hospital %>% get_mon_attributes() |>
    mutate(time = floor(time),
           value = floor(value)) |>
    mutate(endTime = time + value)
tableAttrSum <- tableAttr |>
    group_by(time, key) |>
    summarise(count = n())

ggplot(tableAttrSum, aes(x = time, y = count, colour = key)) +
    geom_line()

ggplot(tableAttr, aes(x = value, colour = key)) +
    geom_density()


get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(table, metric = "waiting_time")

# interArrivalTime
# length of stay each phase
# waiting queue
# waiting time
# patients seen per day
# probabilities of going to each stage
# patient priorities
# can later add doctor resource

# calculate total waiting list size
time <- tableAttr |>
    group_by(time, key) |>
    summarise(added = n())

seen <- tableAttr |>
    group_by(key, endTime) |>
    count() |>
    rename("time" = endTime,
           "removed" = n) |>
    mutate(removed = if_else(is.na(removed), 0, removed))

join <- left_join(time, seen, by = c("time", "key")) |>
    group_by(time, key, .drop = F) |>
    mutate(backlog = added - removed) #|>
    mutate(backlog = if_else(is.na(backlog), added, backlog)) |>
    group_by(key) |>
    group_split()

fu <- bind_rows(join[1]) |>
    pull(backlog)
fuSum <- fu; for(i in seq_along(fuSum)[-1]) fuSum[i] <- max(0, fu[i] + fuSum[i-1])
fuSum
new <- bind_rows(join[2]) |>
    pull(backlog)
newSum <- new; for(i in seq_along(newSum)[-1]) newSum[i] <- max(0, new[i] + newSum[i-1])
newSum


