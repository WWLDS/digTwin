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

