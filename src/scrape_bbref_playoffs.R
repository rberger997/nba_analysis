## NBA playoff advanced player stats analysis
## Ryan Berger PhD
## 5-29-18

##  Web scrape basketball-reference.com for data
##------------------------------------
setwd('~/Desktop/My files/data_projects/nba_analysis/')

# Web scrape nba playoff player stats
library(rvest)

## scrape playoff advanced player stats for 1985-2018
full <- NULL
for(i in 1960:2018){
bbref_url <- read_html(paste('https://www.basketball-reference.com/playoffs/NBA_',i,'_advanced.html',sep = ''))
adv_data <- html_table(html_nodes(bbref_url, 'table')) %>% as.data.frame()
adv_data$Year <- i
adv_data <- filter(adv_data, Rk != 'Rk')  ## Eliminate column header rows
adv_data[c(2,3,5)] <- sapply(adv_data[c(2,3,5)], as.character)  ## Set character columns
adv_data[c(1,4,6:30)] <- sapply(adv_data[c(1,4,6:30)], as.numeric)  ## Set numeric columns
adv_data <- adv_data[,-c(1,20,25)]  ## Drop empty columns
adv_data[is.na(adv_data)] <- 0
adv_data <- adv_data[,c(1,27,2:26)]  ## rearrange the columns
full <- rbind(full, adv_data)
full$Player <- gsub('\\*$','', full$Player)  ## Remove hall of fame asterisk from name
print(paste(i, 'done'))
}
write.csv(full, file = 'playoff_advanced_stats_1960-2018.csv')
