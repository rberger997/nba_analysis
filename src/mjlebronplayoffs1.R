getwd()  # 'My files/nba_analysis/src"


playoffs <- read.csv('../mj-lj-playoff-ELO-tidy.csv')


# Compare Jordan and Lebrons playoff opponents
barplot(filter(playoffs, is_finals == 1)$opp_elo_i,
        col = filter(playoffs, is_finals == 1)$mj.lj,
        ylim = c(1400, 1900))

# Calculate yearly stats
opp.elo <- NULL



for(year in c(1991:1993,1996:1998,2007,2011:2018)){
  temp <- filter(playoffs, year_id == year)
  temp1 <- c(year, mean(temp$elo_i), 
                           mean(temp$opp_elo_i),mean(temp$win_prob)) 
  opp.elo <- as.data.frame(rbind(opp.elo, temp1) %>% 
    set_colnames(c('year_id','avg_elo_i','avg_opp_elo_i','avg_win_prob')))
}

barplot(opp.elo$avg_opp_elo_i,
        ylim = c(1500, 1850))

barplot(playoffs$opp_elo_i,
        col = playoffs$mj.lj,
        ylim = c(1400, 1900))
boxplot(opp_elo_i ~ mj.lj,
        data = filter(playoffs, is_finals == 1))
t.test(opp_elo_i ~ mj.lj, data = filter(playoffs, is_finals == 1))

# Compare conference opponents
conf <- playoffs[playoffs$Finals == 0, ]
conf.avg <- aggregate(conf$ELO, by = list(conf$MJ.LJ, conf$Year), FUN = mean)
boxplot(ELO ~ MJ.LJ,
        data = conf,
        col = c('maroon', 'red'),
        ylim = c(1500, 1800))

ggplot(conf, aes(y = ELO, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red')) +
  geom_jitter(width = 0.1, size = .75) + 
  xlab('Player') +
  ylab('Opponent ELO rating') + 
  ggtitle('NBA playoffs conference opponents - Lebron vs. Jordan')
t.test(ELO ~ MJ.LJ, data = conf)

# Compare finals opponents
finals <- playoffs[playoffs$Finals == 1,]
barplot(finals$ELO,
        col = finals$MJ.LJ,
        ylim = c(1600, 1900))

boxplot(ELO ~ MJ.LJ,
        data = finals,
        col = c('maroon', 'red'))
t.test(ELO ~ MJ.LJ, data = finals)

head(finals)
ggplot(finals, aes(y = ELO, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red')) +
  geom_jitter(width = 0.1) + 
  xlab('Player') +
  ylab('Opponent ELO rating') + 
  ggtitle('NBA Finals opponents - Lebron vs. Jordan')
  
a <- cbind(mjconf$ELO, ljconf$ELO)
colnames(a) <- c('MJ', 'LJ')
boxplot(a)
t.test(a)
