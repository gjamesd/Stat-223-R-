## Tom Brady is the GOAT 

# And disclaimer, I am a chiefs fan, my dad is from KC and 
# i know a player for them, and even tho he beat us in the 
# playoffs Tom Brady is the goat


the_goat <- read.csv(file = "brady.csv", header = TRUE)
head(the_goat)

the_goat$game_type

#subsetting my data 
reg <- the_goat[the_goat$game_type == "Regular", -3]
reg
playoffs <- the_goat[the_goat$game_type == "Playoffs", -3]
playoffs

#making sure I have all of the rows
nrow(reg) 
nrow(playoffs) 
nrow(the_goat)

observed_diff <- mean(playoffs$yards) - mean(reg$yards)
observed_diff

#ok so I am seeing an average of 17 yards more per playoff
#game than i am in the playoffs 

# so something that I'm thinking about tho, since my sample size 
#is so much bigger for reg than playoffs, I need to create a dataframe 
#with the yards for playoff and regular season side by side, and just
#sample randomly from the regular season for the second column 


reg_sample <- sample(reg$yards, 40, replace = FALSE)

#staying consistant witn my top observed stat and going to 
#keep regular season in first column so I can do
# playoffs - regular  

#Much like with the tommy john question, if there is no difference between 
#post season and regular season, then we should be able to switch the yards 
# per game and it shouldn't make a difference 



equal_games <- data.frame(playoffs$yards, reg_sample)
head(equal_games)

perm_brady <- apply(equal_games, 1, sample)
perm_brady
#awesome, looks like that worked, now do it alot 

n_perms <- 10000

six_rings <- replicate(n_perms, {
  perm_brady <- apply(equal_games, 1, sample)
  mean(perm_brady[1,]) - mean(perm_brady[2,])
})

print(six_rings)

plot(density(six_rings))

## calculating p value 
p_v <- mean(six_rings > observed_diff)
p_v

#looking like a p value of .189, which is not statistically
#significant 

#confidence interval of 95% 
ci <- p_v + c(-1, 1) * qnorm(1 - 0.05) * sqrt(p_v * (1 - p_v) / length(six_rings))
ci

abline(v = ci, col = "red")

c(lower = ci[1],
  p_value = p_v,
  upper = ci[2])

# It looks like the difference between regular season and post season 
# passing yards is not statistically significant 

