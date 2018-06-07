getwd()  ## Should be '../data_projects/nba_analysis/src'

# NBA ELO data

nba.all <- read.csv('../fivethirtyeight-nba-elo/nbaallelo.csv')

## Add new columns to nba.elo data
nba.all$elo_diff <- nba.all$elo_i - nba.all$opp_elo_i
nba.all$win_prob <- 1/(1+10^(-nba.all$elo_diff/400))


##---------------------------------------------
## Filter out michael jordan teams
mj <- filter(nba.all, year_id %in% c(1985:1993, 1995:1998) & team_id == 'CHI')

## Filter out playoffs
mj.post <- filter(mj, is_playoffs == 1)
mean(mj.post$win_prob)

## Add labels to finals
mj.series <- NULL
for(year in 1985:1998){
temp <- filter(mj.post, year_id == year) %>%
filter(., !duplicated(opp_id))
mj.series <- rbind(mj.series, temp)
}

## Add labels to finals
mj.series$is_finals <- ifelse(mj.series$opp_id %in% c('LAL','UTA','PHO','SEA','POR'), 1,0)

## Look at average ELO win probability for Lebron in conference play vs finals
mean(filter(mj.series, is_finals == 0)$win_prob)
mean(filter(mj.series, is_finals == 1)$win_prob)
mean(filter(mj.series, is_finals == 1)$forecast)

## Expected championships for Jordan
6 * mean(filter(mj.series, is_finals == 1)$win_prob)

##---------------------------------------------

## Filter out Lebron james teams
lj <- filter(nba.all, year_id %in% c(2004:2010, 2015) & team_id == 'CLE') %>% 
  rbind(., filter(nba.all, year_id %in% c(2011:2014) & team_id == 'MIA')) %>% 
  arrange(., lj$gameorder)

## Filter out playoffs
lj.post <- filter(lj, is_playoffs == 1)
mean(lj.post$win_prob)

## Get game 1 of every series for better win probabilities
lj.series <- NULL
for(year in 2006:2015){
temp <- filter(lj.post, year_id == year) %>%
filter(., !duplicated(opp_id))
lj.series <- rbind(lj.series, temp)
}

## Add labels to finals
lj.series$is_finals <- ifelse(lj.series$opp_id %in% c('SAS','GSW','DAL','OKC'), 1,0)

## Look at average ELO win probability for Lebron in conference play vs finals
mean(filter(lj.series, is_finals == 0)$win_prob)
mean(filter(lj.series, is_finals == 1)$win_prob)

## Expected championships for Lebron
6 * mean(filter(lj.series, is_finals == 1)$win_prob) ## 2.64 through 2015
mean(filter(lj.series, is_finals == 1)$forecast)
##---------------------------------------------

lj.series$mj.lj <- 'lj'
mj.series$mj.lj <- 'mj'
mj.lj <- rbind(mj.series, lj.series)

t.test(win_equiv ~ mj.lj, data = filter(mj.lj, is_finals == 1))

write.csv(mj.lj, file = '../mj-lj-playoff-ELO.csv')


## Calculate series forecast - average forecast for every game in the series
head(lj.post)
lj.post$opp_id <- as.character(lj.post$opp_id)

val <- NULL
for(year in 2004:2015){
  temp <- filter(lj.post, year_id == year) ## Create single year dataframe
  for(i in unique(temp$opp_id)){
    temp1 <- c(year_id = year, opp_id = i,forecast_series=mean(filter(temp, opp_id == i)$forecast))
    val <- rbind(val, temp1)
    val <- as_data_frame(val)
    val$year_id <- as.numeric(val$year_id)
  }
}
lj.series <- left_join(lj.series, val, by = c('year_id', 'opp_id'))

## the five thirty eight series forecast is almost identical to the ELO win probability
plot(lj.series$forecast_series ~ lj.series$win_prob)
lm(lj.series$forecast_series ~ lj.series$win_prob)

## This confirms the ELO win probability formula is about what was used in their forecast