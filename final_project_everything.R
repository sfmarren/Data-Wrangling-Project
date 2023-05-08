#### Sean Marren & Michael Merfeld ####
#### Final Project ####
#### Mike Colbert ####
#### 12/7/22

rm(list=ls())

install.packages("rvest")
install.packages("ggplot2")
library(rvest)
library(dplyr)
library(ggplot2)
library(tidyr)


#### Scraping ####


link<- "https://en.wikipedia.org/wiki/Associated_Press_NFL_Rookie_of_the_Year_Award#cite_note-AP2018-66"
page<- read_html(link)

Year =  page %>% html_nodes("td:nth-child(1) td:nth-child(1) a") %>% html_text()
Player =  page %>% html_nodes("td:nth-child(1) .fn a") %>% html_text()
Pos =  page %>% html_nodes("td:nth-child(1) th+ td a") %>% html_text()
Team =  page %>% html_nodes("td:nth-child(1) td+ td > a") %>% html_text()

winners = data.frame(Player)
winners = data.frame(Player, Pos, Year, Team)
winners
winners['Round'] <- NA
winners['Pick'] <- NA
winners['RoY'] <- 1

winners2 <- winners %>% filter(!row_number() %in% c(1:43, 63:65))

##################################################################################################
#### Cleaning ####


combine_results<- read.csv("combine_data_since_2000_PROCESSED_2018-04-26.csv")

df<- combine_results[-c(3:10,12)]
df<- df[-c(4)]
df<- na.omit(df)
df['RoY']<- NA
###################################################################################################
#### Integration ####


total<- rbind(winners2, df)

#####################################################################################################
### Analysis ####


total_summary<- total[c(4, 7)]
total_summary<- na.omit(total_summary)
qplot(Team,
      RoY,
      data = total_summary,
      geom = "col",
      xlab = "Team",
      ylab = "RoY")


total2 <- merge(df, winners2, by = "Player")
total3 <- subset(total2, select = -c(Pos.x, Year.x, Team.x, RoY.x, Pos.y, Year.y, Team.y, Round.y, Pick.y, RoY.y))
total3$RoundPick <- c(3, 22, 6, 11, 5, 1, 19, 7, 29, 3, 22, 1, 10, 3)
plot(total3$Round.x,
     total3$RoundPick,
     main = "Scatterplot of when Rookie of the Years were Drafted",
     xlab = "Round",
     ylab = "Pick")
text(total3$Round.x, total3$RoundPick, labels = total3$Player)

position_summary<- total[c(2, 7)]
position_summary<- na.omit(position_summary)
qplot(Pos,
      RoY,
      data = position_summary,
      geom = "col",
      xlab = "Pos",
      ylab = "RoY")
