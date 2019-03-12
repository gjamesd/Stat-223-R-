#Mice 

weights <- c(82, 107, 93)


#what is the mean weight of the three mice?
mean(weights)
#94 

pos_combos <- expand.grid(weights, weights, weights)
nrow(pos_combos)

#use apply function to run mean across all of the rows 

list_mean <-apply(pos_combos, 1, mean)
#Using 1 in my apply statement to take the means across
#the rows of my data 

mean(list_mean)
#same mean as my sample 

plot(density(list_mean))
ci <- quantile(list_mean, probs = c(.025, .975))
ci

abline(v = ci, col = "red")
