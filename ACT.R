#ACT 

act <- read.table("act.txt", header = TRUE)

act$class

#subsetting data into different classes 
class1 <- act[act$class == "class1", -3]
class2 <- act[act$class == "class2", -3]

#looking at observed diff between pre and post of the two
observed_diff_c1 <- mean(class1$post - class1$pre)
observed_diff_c1



observed_diff_c2 <- mean(class2$post -class2$pre)
observed_diff_c2


perm_sample_1 <- apply(class1, 1, sample)
perm_sample_2 <- apply(class2, 1, sample)

perm_sample_1
perm_sample_2

mean(perm_sample_1[1,]) - mean(perm_sample_1[2, ])

mean(perm_sample_2[1,]) - mean(perm_sample_2[2, ])
#.01
# Ok so if i understand the perm sample this means that
#this sample of my rows without replacement
#the post test was 1.5 of a point higher 
#replicate this a bunch 
  
n_perms <- 10000
results_c1 <- replicate(n_perms, {
    perm_sample_1 <- apply(class1, 1, sample)
    mean(perm_sample_1[1,]) - mean(perm_sample_1[2,])
  })
#print(results_c1)

plot(density(results_c1))

#p_1 <- mean(cl1_perm >= cl1_observed)

(pv_1 <- mean(results_c1 >= observed_diff_c1))

ci_1 <- pv_1 + c(-1, 1) * qnorm(1 - 0.05 / 2) * sqrt(pv_1 * (1 - pv_1) / length(results_c1))
ci_1

abline(v = ci_1, col = "red")

c(lower = ci_1[1],
  p_value = pv_1,
  upper = ci_1[2])

#wow that is a right on the border of being statistically significant 


results_c2 <- replicate(n_perms, {
  perm_sample_2 <- apply(class2, 1, sample)
  mean(perm_sample_2[1,]) - mean(perm_sample_2[2,])
})


(pv_2<- mean(results_c2 >= observed_diff_c2))

ci_2 <- pv_2 + c(-1, 1) * qnorm(1 - 0.05 / 2) * sqrt(pv_2 * (1 - pv_2) / length(results_c2))


plot(density(results_c2))

abline(v = ci_2, col = "blue")


c(lower = ci_2[1],
  p_value = pv_1,
  upper = ci_2[2])

#I got that class 1 was not significant, but class 2 is statistically significant 

. 


