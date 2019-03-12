## Trees 


head(trees)

lm(Volume ~ Height + Girth, data=trees)

ints <- lm(Volume ~ Height + Girth, data=trees)$coefficients
est <- ints[2]/ints[3]

## estimate for ratio = .072

n_samples <- length(1:nrow(trees))

## want to keep my observations together, height and girth go with a certain volume 

boot_samp <- sample(1:nrow(trees),replace = TRUE)

lm(Volume ~ Height + Girth, data = trees[boot_samp, ])

ints_boot <- lm(Volume ~ Height + Girth, data=trees[boot_samp, ])$coefficients
est_boot <- ints_boot[2]/ints_boot[3]
est_boot


estimators <- replicate(1000,{
    boot_samp <- sample(1:nrow(trees),replace = TRUE)
    ints_boot <- lm(Volume ~ Height + Girth, data=trees[boot_samp,])$coefficients
    ints_boot[2]/ints_boot[3]
  })


plot(density(estimators))

ci <- quantile(estimators, probs = c(0.025, 0.975))

ci

abline(v = ci, col = "red")
