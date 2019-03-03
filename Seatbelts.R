## Seatbelts 

seatbelts <- as.data.frame(Seatbelts)
head(seatbelts)

#subsetting the data into law  and no law 
law <- seatbelts[seatbelts$law == 1, -3]
law
no_law <- seatbelts[seatbelts$law == 0, -3]
no_law


#1 
ob_diff <- mean(law$DriversKilled/ law$drivers) - mean(no_law$DriversKilled/no_law$drivers)
ob_diff
# hhmm does this pass the eye ball test, seams small 

plot(seatbelts$DriversKilled, seatbelts$drivers)
# I think it passes the eye ball test, and I will proceed with the question


#2 
n_bs <- 10000
sample_bs <- replicate(n_bs, mean(sample(law$DriversKilled/law$drivers, replace = TRUE))
                          - mean(sample(no_law$DriversKilled/no_law$drivers, replace = TRUE)))


plot(density(sample_bs))

seat_ci <- quantile(sample_bs, probs = c(0.05, 0.9))
seat_ci

#3
abline(v = seat_ci, col = "red")
c(lower = seat_ci[1],
  upper = seat_ci[2])

#4 

#looks like it did not make a statistically significant change
#because 0 is contained in the confidence interval, 



