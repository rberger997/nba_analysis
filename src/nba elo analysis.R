
getwd()  ## Should be '../data_projects/nba_analysis/src'

# NBA ELO data

nba.all <- read.csv('../fivethirtyeight-nba-elo/nbaallelo.csv')
nba.all[1:10,]

pistons <- nba.all[nba.all$team_id == 'DET',]
pistons[1:5,]

# Generate average ELO ratings for every team every year
teamavg <- aggregate(nba.all$elo_n, by = list(nba.all$team_id, nba.all$year_id), FUN = mean)
teamavg <- teamavg[order(-teamavg$x),]

hist(teamavg$x,
     breaks = 25)

# Look at teams between 1990-1998 during Jordan championship era
x90s <- nba.all[nba.all$year_id > 1990 & nba.all$year_id < 1999, ]
avg90s <- aggregate(x90s$elo_n, by = list(x90s$fran_id, x90s$year_id), FUN = mean)
hist(avg90s$x,
     breaks = 25)
avg90s <- avg90s[order(-avg90s$x),]
# Look at all non bulls teams during 1990-1999 
mj.comp <- avg90s[avg90s$Group.1 != 'Bulls', ]
mj.comp <- mj.comp[order(-mj.comp$x),]
mj.top25 <- mj.comp[1:25,]
mj.top25avg <- mean(mj.top25$x)
hist(mj.comp$x)

# Look at teams between 2007-2015 during Lebron championship era
x00s <- nba.all[nba.all$year_id > 2007 & nba.all$year_id < 2015, ]
avg00s <- aggregate(x00s$elo_n, by = list(x00s$fran_id, x00s$year_id), FUN = mean)
hist(avg00s$x,
     breaks = 25)
avg00s <- avg00s[order(-avg00s$x),]
# Look at all non lebron teams during 1990-1999 
colnames(avg00s) <- c('Team', 'Year', 'ELO')
# No lebron cavs teams
x0710 <- nba.all[nba.all$year_id > 2007 & nba.all$year_id < 2010, ]
avg0710 <- aggregate(x0710$elo_n, by = list(x0710$fran_id, x0710$year_id), FUN = mean)
colnames(avg0710) <- c('Team', 'Year', 'ELO')
nocavs <- avg0710[avg0710$Team != 'Cavaliers',]
# No lebron heat teams
x1015 <- nba.all[nba.all$year_id > 2011 & nba.all$year_id < 2015, ]
avg1015 <- aggregate(x1015$elo_n, by = list(x1015$fran_id, x1015$year_id), FUN = mean)
colnames(avg1015) <- c('Team', 'Year', 'ELO')
noheat <- avg1015[avg1015$Team != 'Heat',]
# Merge no lebron team datasets
lj.comp <- merge(nocavs, noheat, all = TRUE)
rm(noheat, nocavs)

# Analyze lebron competetion
lj.comp <- lj.comp[order(-lj.comp$ELO),]
lj.top25 <- lj.comp[1:25,]
lj.top25avg <- mean(lj.top25$ELO)
hist(lj.comp$ELO,
     breaks = 30)
barplot(lj.top25$ELO,
        descending = TRUE,
        ylim = c(1640, 1740),
        main = 'Lebron James era competition',
        col = lj.top25$Team)
barplot(mj.top25$x,
        descending = TRUE,
        ylim = c(1640, 1740),
        main = 'Michael Jordan era competetion',
        col = mj.top25$Group.1)
boxplot(lj.top25$ELO, mj.top25$x,
        col = c('maroon', 'red'))
rm()


# Look at all Bulls playoff opponents in Jordan era
x90s[1:10, ]
mj.opp <- nba.all[nba.all$is_playoffs == 1 & nba.all$fran_id == 'Bulls' & nba.all$year_id %in% c(1985:1993, 1995:1998),]
mj.oppavg <- aggregate(mj.opp$opp_elo_i, by = list(mj.opp$opp_fran, mj.opp$year_id), FUN = mean)

mj.avg <- aggregate(mj.opp$elo_i, by = list(mj.opp$fran_id, mj.opp$year_id), FUN = mean)
colnames(mj.avg) <- c('Team', 'Year', 'ELO')
colnames(mj.oppavg) <- c('Team', 'Year', 'ELO')
mj.new <- rbind(mj.avg, mj.oppavg)

barplot(mj.oppavg$ELO,
        col = mj.oppavg$Team)
mj.oppelo <- mean(mj.oppavg$ELO)


# Look at lebron playoff opponents with Cleveland
lj.cle <- nba.all[nba.all$is_playoffs == 1 & nba.all$fran_id == 'Cavaliers' & nba.all$year_id > 2003 & nba.all$year_id < 2011,]
lj.mia <- nba.all[nba.all$is_playoffs == 1 & nba.all$fran_id == 'Heat' & nba.all$year_id > 2010 & nba.all$year_id < 2015,]
lj.cle2 <- nba.all[nba.all$is_playoffs == 1 & nba.all$fran_id == 'Cavaliers' & nba.all$year_id == 2015,]
lj.opp <- merge(lj.cle, lj.mia, all = TRUE)
lj.opp <- merge(lj.cle2, lj.opp, all = TRUE)

lj.oppavg <- aggregate(lj.opp$opp_elo_i, by = list(lj.opp$opp_fran, lj.opp$year_id), FUN = mean)
lj.avg <- aggregate(lj.opp$elo_i, by = list(lj.opp$fran_id, lj.opp$year_id), FUN = mean)

lj.new <- rbind(lj.avg, lj.oppavg)

colnames(lj.new) <- c('Team', 'Year', 'ELO')

## Combine Jordan and Lebrons opponents and teams
lj.mj <- rbind(mj.new, lj.new)
write.csv(lj.mj, file = '../lj-mj-playoff-ELO.csv')


# Compare Jordan & Lebrons career playoff opponents ELO ratings
boxplot(mj.oppavg$ELO, lj.oppavg$ELO,
        col = c('red', 'maroon'),
        main = 'Playoff opponents average ELO ratings')


rm(list = ls())

# Lebrons finals opponents
lj.oppf <- c(1712, 1715, 1732, 1718, 1739, 1803, 1788, 1855)
# Jordans finals opponents
mj.oppf <- c(1691, 1696, 1634, 1691, 1751, 1750)
boxplot(lj.oppf, mj.oppf)

