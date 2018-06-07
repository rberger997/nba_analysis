## Combine regular season and playoff composite z score
player.reg <- read.csv(file = '../career-reg-season-player-stats.csv')
player.post <- read.csv(file = '../career_playoff_PER_by_player.csv')

head(player.reg)
x <- player.reg[,c(2,3,5,7)]
y <- player.post[,c(2,3,5,7)]
z <- left_join(y, x, by = 'Player')

colnames(z) <- c('Player', 'PER_p','WS.G_p','G_p','PER_r','WS.G_r','G_r')
z$Total_Games <- z$G_p + z$G_r

## Set games filter
z <- filter(z, G_r > 500 & G_p >= 40)

## Calculate z scores
z$Season <- round(scale((z$PER_r + z$WS.G_r)/2, scale = T, center = T),2) %>% as.numeric()
z$Playoffs <- round(scale((z$PER_p + z$WS.G_p)/2, scale = T, center = T),2) %>% as.numeric()
z$Combined <- round((z$Season + z$Playoffs)/2,2) %>% as.numeric()


z <- z[,c('Player', 'Total_Games', 'Season', 'Playoffs', 'Combined')] %>% 
  arrange(-Combined)

## Save file
write.csv(z, file = '../nba-best-players.csv')
