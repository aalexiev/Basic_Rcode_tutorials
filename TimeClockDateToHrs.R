## Converting timepoint data from clock and date to hours elapsed

times <- read.table("Timepoint_clock.txt", header = T, sep = "")
int1 <- interval(ymd_hms(times$Clock[1]), ymd_hms(times$Clock[2]))
T1_hr <- time_length(int1, "hour")
int2 <- interval(ymd_hms(times$Clock[1]), ymd_hms(times$Clock[3]))
T2_hr <- time_length(int2, "hour")
int3 <- interval(ymd_hms(times$Clock[1]), ymd_hms(times$Clock[4]))
T3_hr <- time_length(int3, "hour")
int4 <- interval(ymd_hms(times$Clock[1]), ymd_hms(times$Clock[5]))
T4_hr <- time_length(int4, "hour")
int5 <- interval(ymd_hms(times$Clock[1]), ymd_hms(times$Clock[6]))
T5_hr <- time_length(int5, "hour")
times$HoursElapsed <- round(c(0, T1_hr, T2_hr, T3_hr, T4_hr, T5_hr), digits = 0)
