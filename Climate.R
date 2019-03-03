## Exam 2 Climate Change 

climate <- read.csv(file = "oil-temp.csv", header = TRUE)
head(climate)

#visualizing my data a little bit so I know what I'm looking at 
plot(climate$crude_oil_consumption, climate$temp_change)


observed_cor <- cor(climate$crude_oil_consumption, climate$temp_change)
observed_cor
#got .5285

#because the null hypothessis is that there is no correlation between columns, I am going 
#to sample within each column and then run perm tests on that. 


#2 
n_perms <- 10000

temp <- sample(climate$temp_change, replace = FALSE)
temp

oil <- sample(climate$crude_oil_consumption, replace = FALSE)
oil 

cor(temp, oil)

results <- replicate(n_perms, {
  temp <- sample(climate$temp_change, replace =  FALSE)
  oil <- sample(climate$crude_oil_consumption, replace =  FALSE)
 cor(temp, oil)
})

head(results)

#3 

plot(hist(results))
abline(v = observed_cor, col = "red")

#4 

p_value <- mean(abs(results) >= abs(observed_cor))
p_value

#really low p value 

ci <- p_value + c(-1, 1) * qnorm(.975) * sqrt(p_value * (1 - p_value) / n_perms)

c(lower = ci[1],
  p_value = p_value,
  upper = ci[2])

# Conclusion:
#reject the null hypothesis and say that there is a relationship between oil consumption and temperature change 


