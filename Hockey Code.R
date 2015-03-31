library(XML)
library(RCurl)
library(lubridate)
library(dplyr)

url <- "http://www.hockey-reference.com/leagues/NHL_2014_games.html"

html <- htmlTreeParse(url, useInternal=TRUE)
tables <- readHTMLTable(html)

season<- tables$games
playoffs <- tables$games_playoffs

#Row rename and variable transformation
names(season)[5:6] <- c("G.1", "OT")
season <- transform(season, Notes = as.character(Notes), Date = as.character(Date), G= as.character(G), G.1= as.character(G.1))
season <- transform(season, Date = as.Date(Date))

#Get's rid of postponed games
season <- subset(season, G!= "")
rows <- nrow(season)

#puts in W/L factor for both Visitor and Home.  
i<-1
while (i <= rows){
  
  if(season$G[i]<season$G.1[i]){
    season$V.W[i] <- "L"
    season$H.W[i] <- "W"
  } else {
    season$V.W[i] <- "W"
    season$H.W[i] <- "L"}
  
  i<-i+1
}

#Creates a dataframe with all games based on visiting team
visitorgames <- season[,c(1,2,3,6,8,7)]
visitorgames$Home_Away <- "A"
names(visitorgames)[2] <- "Team"
names(visitorgames)[5] <- "W"

#Creates a dataframe with all games based on home team
homegames <- season[,c(1,4,5,6,9,7)]
homegames$Home_Away <- "H"
names(homegames)[2] <- "Team"
names(homegames)[3] <- "G"
names(homegames)[5] <- "W"

#Combines visiter based games and Home based games into one dataframe
allgames <- rbind(homegames, visitorgames)

allgames <- arrange(allgames, Team, Date)

daysoff <- data.frame(do.call("cbind", by(allgames, allgames$Team, function(x) as.numeric(x$Date-lag(x$Date)))))

daysoffvector <- unlist(daysoff)

allgames <- cbind(allgames, daysoffvector)

suball <- data.frame(cbind(allgames$W, allgames$daysoffvector))

#table is used to count the amount of wins and losses

table<- data.frame(table(suball))
backtoback <- table$Freq[2]/(table$Freq[2] +table$Freq[1])
onedayoff <- table$Freq[10]/(table$Freq[10] +table$Freq[9])
twodaysoff <- table$Freq[16]/(table$Freq[16] +table$Freq[15])

backtoback
onedayoff
twodaysoff


#Setting up for chi square test
data <- matrix(c(table[1,3],table[2,3], table[15,3],table[16,3]), nrow=2)
table2 <- as.table(data)

chisq.test(table2)
