# 1. Distribution of Total Visits by Clinic
results_long <- results %>%
    pivot_longer(cols = c(Dr_visits, Sister_visits, Specialist_visits, Immuno_visits),
                 names_to = "Clinic",
                 values_to = "Visits")

ggplot(results_long, aes(x = Clinic, y = Visits, fill = Clinic)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Total Visits by Clinic", x = "Clinic", y = "Total Visits") +
    theme_minimal()

# 2. Distribution of Total Follow-Ups
ggplot(results, aes(x = Total_Followups)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(title = "Distribution of Total Follow-Ups", x = "Number of Follow-Ups", y = "Count of Patients") +
    theme_minimal()

# 3. Distribution of Total Days Spent in the System
ggplot(results, aes(x = Total_Days_on_list)) +
    geom_histogram(binwidth = 5, fill = "coral", color = "black") +
    labs(title = "Distribution of Total Days Spent in the System", x = "Total Days", y = "Count of Patients") +
    theme_minimal()

# 4. Discharge Rate by First Clinic
discharge_rate <- results %>%
    group_by(First_Clinic) %>%
    summarize(Discharge_Rate = mean(Discharged))

ggplot(discharge_rate, aes(x = First_Clinic, y = Discharge_Rate, fill = First_Clinic)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Discharge Rate by First Clinic", x = "First Clinic", y = "Discharge Rate") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()

library(ggplot2)

# Create a data frame for plotting
waiting_list_df <- data.frame(
    Day = 1:days,
    Waiting_List_Size = waiting_list_sizes
)

# Plot the waiting list size over time
ggplot(waiting_list_df, aes(x = Day, y = Waiting_List_Size)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Waiting List Size Over Time",
         x = "Day",
         y = "Number of Patients on Waiting List") +
    theme_minimal()

# Create a data frame for plotting
processed_df <- data.frame(
    Day = 1:days,
    Patients_Processed = patients_processed
)

# Plot the number of patients processed each day
ggplot(processed_df, aes(x = Day, y = Patients_Processed)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Number of Patients Processed Each Day",
         x = "Day",
         y = "Number of Patients Processed") +
    theme_minimal()


#### Priorities ----------------------------------------------------------------
# Analyze the distribution of priorities
priority_distribution <- results %>%
    group_by(Priority) %>%
    summarize(Count = n())

# Print the distribution
print(priority_distribution)

# Visualize the priority distribution
ggplot(priority_distribution, aes(x = Priority, y = Count, fill = Priority)) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of Patient Priorities",
         x = "Priority",
         y = "Number of Patients") +
    theme_minimal()

# Analyze the average waiting time by priority
waiting_time_by_priority <- results %>%
    group_by(Priority) %>%
    summarize(Average_Waiting_Time = mean(Waiting_Time))

# Print the average waiting time by priority
print(waiting_time_by_priority)

# Visualize the average waiting time by priority
ggplot(waiting_time_by_priority, aes(x = Priority, y = Average_Waiting_Time, fill = Priority)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Waiting Time by Patient Priority",
         x = "Priority",
         y = "Average Waiting Time (days)") +
    theme_minimal()

