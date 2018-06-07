## NBA playoff advanced player stats analysis
## Ryan Berger PhD
## 5-29-18

getwd()  ## Should be data_projects/nba_analysis

## Set ggplot theme
ggthemr::ggthemr('fresh')

## Data was scraped from basketball reference and saved as csv
## Read in file if already saved
full <- read.csv('../playoff_advanced_stats_1960-2018.csv')
full <- full[,-1]

## Arrange by win shares
full <- arrange(full, desc(WS))
head(full)

## Look at top 20 playoff PER (filter by 200 minutes to eliminate outliers)
filter(full[,1:10], MP > 200) %>% 
  arrange(-PER) %>% 
  head(., 20)
head(full)

filter(full, Player == 'Michael Jordan')

## Compare career playoff PER
## Normalize PER by games played
full$PER_total <- full$PER * full$G
full$MP.G <- round(full$MP / full$G, 1)
full$WAR <- full$VORP * 2.7
full$WAR.G <- full$WAR / full$G
full$WAR.48 <- full$WAR * 48 /full$MP
full$WS.G <- full$WS / full$G

head(arrange(filter(full, G > 10 & MP > 200), -WS.48))

## Filter out det 2000s playoff stats
det <- filter(full, Tm == 'DET' & MP > 200 & Year %in% 2000:2010) %>% 
  arrange(-WS.G)

head(det, 10)
head(arrange(filter(full, G > 15), -WS.G),10)


## Function to get career PER for each player
career_PER <- function(player){
  sum(filter(full, Player == player)$PER_total) / sum(filter(full, Player == player)$G)
}
## Function to get career WS per 48 minutes for each player
career_WS.48 <- function(player){
  sum(filter(full, Player == player)$WS) * 48/ sum(filter(full, Player == player)$MP)
}

## Function to get career WS per game for each player
career_WS.G <- function(player){
  sum(filter(full, Player == player)$WS) / sum(filter(full, Player == player)$G)
}

mean(filter(full, Player == 'LeBron James')$WAR.G)
sum(filter(full, Player == 'LeBron James')$WS) / sum(filter(full, Player == 'LeBron James')$G)

career_PER('LeBron James')
career_WS.G('LeBron James')

## Make dataframe of player career playoff PER
player.PER <- NULL
head(full)
full.1 <- filter(full, MP.G > 10 & MP > 100)
for(player in unique(full.1$Player)){
  temp <- as.data.frame(list(player,  ## Name
                             round(career_PER(player),2),  ## Career playoff PER
                             round(career_WS.48(player), 4),  ## Career playoff Win shares per 48min
                             round(career_WS.G(player), 4),  ## Career playoff Win shares per 48min
                             sum(filter(full, Player == player)$WAR) / sum(filter(full, Player == player)$G),
                             sum(filter(full.1, Player == player)$G)))  ## Games played
  colnames(temp) <- c('Player', 'PER','WS.48','WS.G','WAR.G' ,'G')
  player.PER <- rbind(player.PER, temp) %>% 
    arrange(-PER) %>% filter(G > 10)
}

player.PER <- player.PER[,1:6]
## Filter out low sample size
player.PER <- filter(player.PER, G > 50)

## Calculate z scores for each category
player.PER$PER_z <- round(scale(player.PER$PER, center = T, scale = T),2)
player.PER$WS.48_z <- round(scale(player.PER$WS.48, center = T, scale = T),2)
player.PER$WS.G_z <- round(scale(player.PER$WS.G, center = T, scale = T),2)
player.PER$WAR.G_z <- round(scale(player.PER$WAR.G, center = T, scale = T),2)


## Calculate composite z score (use win shares per game and PER to maintain eras without WAR)
player.PER$z_composite <- round((player.PER$PER_z + player.PER$WS.G_z)/2,2)

## Save results
write.csv(player.PER, '../career_playoff_PER_by_player.csv')

## If already saved:
player.PER <- read.csv(file = '../career_playoff_PER_by_player.csv')

## Filter out low sample size
player.PER <- filter(player.PER, G > 100)

head(player.PER)


## Look at the help on each player's team in finals runs
## Extract Jordan's teammates
mjhelp <- filter(full, Tm == 'CHI' & Year %in% c(1991:1993, 1996:1998) & MP.G > 15) %>% 
  filter(Player != 'Michael Jordan')

## Extract Lebron's Cleveland teammates
ljhelp.cle <- filter(full, Tm == 'CLE' & Year %in% c(2007,2015:2018) & Player != 'LeBron James' & MP.G > 15)

## Extract Lebron's Miami teammates
ljhelp.mia <- filter(full, Tm == 'MIA' & Year %in% 2011:2014 & Player != 'LeBron James' & MP.G > 15)

## Combine all Lebron's teammates together
ljhelp <- rbind(ljhelp.cle, ljhelp.mia)
mean(ljhelp$PER)
mean(ljhelp$WS.48)

## Combine Both MJ and LJ sets together
mjhelp$MJ.LJ <- 'MJ'
ljhelp$MJ.LJ <- 'LJ'
allhelp <- rbind(mjhelp, ljhelp)

boxplot(allhelp$PER)

boxplot(WAR.G ~ MJ.LJ,
        data = allhelp,
        col = c('maroon', 'red'))
t.test(WAR.G ~ MJ.LJ, data = allhelp)

library(ggpubr)
## Plot teammates help - Win shares per game
metric <- 'Win shares per game'
p <- ggplot(allhelp, aes(y = WS.G, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red'), outlier.color = 'white') +
  geom_jitter(width = 0.1, size = 0.5, color = 'slategray') + 
  xlab('Player') +
  ylab(paste('Teammate help (',metric,')', sep = '')) + 
  ggtitle('Playoffs teammate help - Lebron vs. Jordan')
p + stat_compare_means(data = allhelp, comparisons = list(c('LJ', 'MJ')), method = 't.test', label = 'p.signif')

t.test(WS.G ~ MJ.LJ, data = allhelp)  ## Stats show non-significant


## Plot teammates help - WAR per game
metric <- 'WAR per game'
p <- ggplot(allhelp, aes(y = WAR.G, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red'), outlier.color = 'white') +
  geom_jitter(width = 0.1, size = 0.5, color = 'slategray') + 
  xlab('Player') +
  ylab(paste('Teammate help (',metric,')', sep = '')) + 
  ggtitle('Playoffs teammate help - Lebron vs. Jordan')
p + stat_compare_means(data = allhelp, comparisons = list(c('LJ', 'MJ')), method = 't.test', label = 'p.signif')


for(i in c(1991:1993, 1996:1998, 2007, 2011:2018)){
  print(paste(i, round(mean(filter(allhelp, Year == i)$WS.G),3)))
}



## Look at efficiency over time
per.yr <- NULL
for(i in 1960:2018){
  temp <- as.data.frame(list(i,mean(filter(full, Year == i & MP.G > 20)$PER))) 
  colnames(temp) <- c('Year', 'PER')
  per.yr <- rbind(per.yr, temp)
  
}

## Average PER per player > 20 minutes hasn't really changed since 1960
ggplot(per.yr, aes(y = PER, x = Year)) + geom_line(size = 1)+
  scale_y_continuous(name = 'Average player PER \n(>20 MPG)',limits = c(10,20)) + 
  scale_x_continuous(limits = c(1960,2020), breaks = seq(1960, 2020, by = 10)) +
  ggtitle('Average player efficiency in the NBA playoffs 1960-2018')

## Reset ggplot theme
ggthemr::ggthemr_reset()

