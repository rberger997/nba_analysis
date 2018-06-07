# Compare Jordan and Lebrons playoff opponents
playoffs <- read.csv('/Users/rberger/Desktop/My\ files/mjlebronplayoffs.csv')
barplot(playoffs$ELO,
        col = playoffs$MJ.LJ,
        ylim = c(1400, 1900))
boxplot(ELO ~ MJ.LJ,
        data = playoffs)
t.test(ELO ~ MJ.LJ, data = playoffs)

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
