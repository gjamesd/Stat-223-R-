## Showers 

roommate <- c(6,6.25,5.75,6.5,6,15,35,20,4,5.25,5.75)

you <- c(3.5,5.5,4,4,4,6,11,12,3,4,4.25,7,3.25)

t.test(roommate, you, conf.level = 0.9)

#just playing around with the data 
mean(roommate)
mean(you)

observed <- mean(roommate) - mean(you)
observed

n_times <- 10000
sample_times <- replicate(n_times, mean(sample(you, replace = TRUE))
                          - mean(sample(roommate, replace = TRUE)))


plot(density(sample_times))

ci <- quantile(sample_times, probs = c(0.05, 0.9))
ci


abline(v = ci, col = "red")

