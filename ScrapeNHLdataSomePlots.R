###  Sam Ventura
###  R Scraping Demo
###  18 September 2013
### Scraps the NFL for info on players. Then some plots of the data.
# rm(list = ls())




library(ggplot2)
library(plyr)
library(lubridate)
library(stringr)
library(reshape2)
library(devtools)
install_github("hadley/rvest")

setwd("C:/Users/jamesthompsonc/Documents/Programing Projects/Web Scrape")


team.abbr = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
              "DET", "GB", "HOU", "IND", "JAC", "KC", "MIA", "MIN", "NE", "NO", "NYG", 
              "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB", "TEN", "WAS")

team.loc = c("arizona", "atlanta", "baltimore", "buffalo", "carolina", "chicago", 
             "cincinati", "cleveland", "dallas", "denver", "detroit", "greenbay", 
             "houston", "indianapolis", "jacksonville", "kansascity", "miami", 
             "minnesota", "newengland", "neworleans", "newyork", "newyork", "oakland", 
             "philadelphia", "pittsburgh", "sandiego", "sanfrancisco", "seattle", 
             "stlouis", "tampabay", "tennessee", "washington")

team.name = c("cardinals", "falcons", "ravens", "bills", "panthers", "bears", "bengals", 
              "browns", "cowboys", "broncos", "lions", "packers", "texans", "colts", 
              "jaguars", "chiefs", "dolphins", "vikings", "patriots", "saints", "giants", 
              "jets", "raiders", "eagles", "steelers", "chargers", "49ers", "seahawks", 
              "rams", "buccaneers", "titans", "redskins")

team.full = paste(team.loc, team.name, sep = "")
team.full

all.players = NULL

team.ID = 1:32

teamInfo <- cbind(team.ID, team.abbr, team.loc, team.name, team.full)

ii = 25 ## steelers


for(ii in team.ID){
  
  print(team.full[ii])
  
  
  
  
  team.link = paste("http://www.nfl.com/teams/", team.full[ii], 
                    "/roster?team=", team.abbr[ii], sep = "")
  team.link
  txt = try(readLines(URLencode(team.link)))
  txt
  
  
  
  for(jj in 1:length(txt)){
    if(txt[jj] == '<tr class="even">' | txt[jj] == '<tr class="odd">'){
      
      ##  Get Player ID and Full Name
      str1 = txt[jj+2]
      str1 = strsplit(str1, "/player/")[[1]][2]
      str1 = strsplit(str1, '/profile\">')[[1]]
      str2 = str1[2]
      str1 = strsplit(str1[1], "/")[[1]]
      player.ID = str1[2]
      full.name = str1[1]
      
      ##  Get Player First and Last Name
      str2 = strsplit(str2, "</a>")[[1]][1]
      str2 = strsplit(str2, ", ")[[1]]
      last.name = str2[1]
      first.name = str2[2]
      
      ##  Get remaining player info:  jerseyNumber, position, status, height, weight, DOB, experience, college
      info = NULL
      for(kk in c(1, 3:9)){
        str1 = txt[jj+kk]
        str1 = strsplit(str1, "<td>")[[1]][2]
        info = c(info, strsplit(str1, "</td>")[[1]][1])
      }
      
      ##  Put it all together and add to all.players data frame, including team ID (ii)
      player.data = c(player.ID, full.name, first.name, last.name, info, ii)
      all.players = rbind(all.players, player.data)
    }
  }
}

##  Formatting, saving
dim(all.players)
rownames(all.players) = NULL
head(all.players)
colnames(all.players) = c("player.ID", "full.name", "first.name", "last.name", "jerseyNumber", "position", "status", "height", "weight", "DOB", "experience", "college", "team.ID")
all.players = as.data.frame(all.players)
head(all.players)


# add a col with the data accessed. Just in case the data changes through out the year.
all.players$assecced <- format(Sys.Date(), "%Y-%m-%d")
write.csv(all.players, file = paste("Data/all.players",format(Sys.Date(), "%Y-%m-%d"), ".csv"), row.names = FALSE)

     ##  Format and save team.data file

team.data = cbind(team.ID, team.abbr, team.loc, team.name, team.full)
team.data = as.data.frame(team.data)
head(team.data)
# save todays data.



#load("Data/all.players.RData")
#load("Data/team.data.RData")
# more permanent
write.csv(all.players, file = paste0("Data/all.players.",format(Sys.Date(), "%Y-%m-%d"),".csv"))
write.csv(team.data, file = paste0("Data/team.data",format(Sys.Date(), "%Y-%m-%d"),".RData"))

names(all.players)
str(all.players)


# Change data type so that the histogram plots well.
# weight to numeric
levels(all.players$weight)
all.players$weight <- as.numeric(as.character(all.players$weight))
#height to ordered factor
levels(all.players$height)
hlevels <- c(" 5'5\"",  " 5'6\"",  " 5'7\"",  " 5'8\"",  " 5'9\"", " 5'10\"", " 5'11\"", " 6'0\"",  " 6'1\"",  " 6'2\"", " 6'3\"",  " 6'4\"",  " 6'5\"",  " 6'6\"",  " 6'7\"",  " 6'8\"",  " 6'9\"") 
all.players$height <- ordered(all.players$height, levels=hlevels)
# experience to numeric
all.players$experience <- ordered(as.numeric(as.character(all.players$experience)))

# data of birth to date

all.players$DOB <- as.Date(all.players$DOB,format='%m/%d/%Y')

names(all.players)
summary(all.players)


#### PLOTS
# jerseyNumber
i=5
names(all.players)[i]
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))  + coord_flip()

str(all.players)
theTable <- all.players
sortbycol <- names(all.players)[i]
theTable[,sortbycol] <- factor(theTable[, sortbycol],
                               levels = names(sort(table(theTable[sortbycol]), decreasing=TRUE)))
head(theTable)
## plot
ggplot(theTable,aes_string(x=sortbycol))+geom_bar(binwidth=1, , color=I("white")) + coord_flip()

# position
i=6
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"), main = "How many players per position?")

### For when the x is categorical and I want to order
theTable <- all.players
sortbycol <- names(all.players)[i]
theTable[,sortbycol] <- factor(theTable[, sortbycol],
                               levels = names(sort(table(theTable[sortbycol]), decreasing=TRUE)))
head(theTable)
## plot
ggplot(theTable,aes_string(x=sortbycol))+geom_bar(binwidth=1, , color=I("white"), main = "How many players per position?") + coord_flip()
ggsave("nPlayersPerPosition.pdf")

# status
i=7
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))

# height
i=8
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))

qplot(height ,data=all.players, color=I("white"), main = paste0("heightByPos in the NFL ",format(Sys.Date(), "%Y-%m-%d")))+ facet_grid(position~.)

ggsave(paste0("heightByPos", format(Sys.Date(), "%Y-%m-%d"), ".pdf"))

qplot(height ,data=all.players, color=I("white")) + facet_wrap(~position, ncol = 2, scales = "free_y")
ggsave(paste0("heightByPosVer2", format(Sys.Date(), "%Y-%m-%d"), ".pdf"))

qplot(height,data=all.players, xlab=names(all.players)[i], color=I("white"))  + facet_grid(team.ID~.)

theTable <- all.players
sortbycol <- names(all.players)[i]
theTable[,sortbycol] <- factor(theTable[, sortbycol],
                               levels = names(sort(table(theTable[sortbycol]), decreasing=TRUE)))
head(theTable)
## plot
ggplot(theTable,aes_string(x=sortbycol))+geom_bar(binwidth=1, , color=I("white")) + coord_flip()


# weight
i=9
names(all.players)
names(all.players)[i]
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))
qplot(all.players[,i],data=all.players, xlab=names(all.players)[i], color=I("white"), fill=position)

qplot(weight,data=all.players, xlab=names(all.players)[i], color=I("white"), main = "Weight by Position in the NFL
      ") + facet_grid(position~.)

#weight distribution by position
qplot(weight,data=all.players, xlab=names(all.players)[i], color=I("white"),  main = paste0(
      "Weght by Position in the NFL:", format(Sys.Date(), "%Y-%m-%d"))) + facet_grid(position~.)
ggsave(paste0("positionbyWeight", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
)



## Weight Distribution by Team
qplot(weight,data=all.players, xlab=names(all.players)[i], color=I("white")) + stat_bin(binwidth=10) + facet_grid(team.ID~.)

ggplot(data=all.players, aes(x=team.ID, y=weight)) + geom_violin()
ggplot(data=all.players, aes(x=team.ID, y=weight)) + geom_boxplot()

#qplot(all.players[,i],data=all.players, xlab=names(all.players)[i], color=I("white"), binwidth=10) + facet_grid(team.ID~position)


# data of birth

i=10
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))

# year experience
names(all.players)
table(all.players$status)

i=11
qplot(all.players[,i], xlab=names(all.players)[i], color=I("white"))
table(year(all.players$DOB), all.players$experience)

heatmap(table(year(all.players$DOB), all.players$experience),Rowv=NA, Colv=NA, scale="none")

i=12
all.players.active <- subset(all.players, status == "ACT")
names(all.players.active)[i]
qplot(college, data=all.players.active, color=I("white"))
theTable <- all.players.active
sortbycol <- names(all.players.active)[i]
theTable[,sortbycol] <- factor(theTable[, sortbycol],
                               levels = names(sort(table(theTable[sortbycol]), decreasing=TRUE)))
head(theTable)
## plot
head(theTable)
head(names(sort(table(theTable[sortbycol]), decreasing=TRUE)), 30)

tmp <- arrange(count(theTable, sortbycol), freq)
tail(tmp)
qplot(college, freq, data=tail(tmp, 50), geom="bar", stat="identity", color=I("white"), main="Num of Active Players per College\n Top 50" , ylab="Count") + coord_flip()
ggsave(paste0("activePlayersByCollege", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
)
ggplot(data=theTable,aes_string(x=sortbycol))+geom_bar(binwidth=1) + coord_flip()

i="team.name"
allPlayers <- merge(all.players, team.data) 
head(merge(allPlayers, team.data))
qplot(allPlayers[,i], xlab=i, color=I("white")) + coord_flip()

#median weigh
str(allPlayers)
count(allPlayers$position, all.players$weight)
names(a)
wTeam <- ddply(allPlayers, .(team.name), summarize, median(weight))
names(wTeam) <- c('team','medWgt')
arrange(wTeam, medWgt)

# height
ggplot(allPlayers, aes(x=height, y=weight)) + geom_boxplot()
ggplot(allPlayers, aes(x=position, y=weight)) + geom_boxplot()
ggplot(allPlayers, aes(x=team.abbr, y=weight)) + geom_violin()+ coord_flip()

str(allPlayers)

# Look at the frequency of first names
arrange(data.frame(table(allPlayers$first.name)), Freq)


# Convert height to numbers
allPlayers$height
heightF <- sapply(str_split(allPlayers$height, "'"), function(v) v[1])
heightF <- as.numeric(str_trim(heightF))
heightF

heightIn <- sapply(str_split(allPlayers$height, "'"), function(v) v[2])
heightIn
heightIn <- as.numeric(str_extract(heightIn, "[0-9]*"))
allPlayers$heightNum <- heightF + heightIn/12

ggplot(allPlayers, aes(x=heightNum, y = weight)) + geom_jitter(aes(color=position))
ggplot(allPlayers, aes(x=heightNum, y = weight)) + geom_jitter(aes(color=as.numeric(experience)))

ggplot(allPlayers, aes(x=team.abbr, y=as.numeric(experience))) + geom_boxplot() + coord_flip()

ggplot(allPlayers, aes(x=team.abbr, y=DOB)) + geom_boxplot() + coord_flip()

myDataFrame <- allPlayers
myDataFrame$TYPE <- with(myDataFrame, reorder(team.abbr, DOB, median))
myDataFrame$TYPE

boxplot( DOB~TYPE, data=myDataFrame)

## set the levels in order we want
ordered_spec = with(allPlayers, reorder(team.abbr, DOB, median, na.rm=TRUE))
ordered_spec

ggplot(allPlayers, aes(x=ordered_spec, y=DOB)) + geom_boxplot() + xlab(label="Team Abbr.") + coord_flip()

str(allPlayers)
ordered_spec = with(allPlayers, reorder(team.abbr, as.numeric(experience), median, na.rm=TRUE))
ordered_spec

str(allPlayers)
ggplot(allPlayers, aes(x=ordered_spec, y=as.numeric(experience))) + geom_boxplot() + xlab(label="Team Abbr.") + coord_flip()
#ggplot(allPlayers, aes(x=experience)) + geom_histogram() + facet_grid(as.factor(team.abbr, levels=ordered_spec)~.)  #+ coord_flip()


qplot(weight,data=all.players,  color=I("white")) + 
  facet_grid(height~.)

names(allPlayers)
qplot(weight, data = allPlayers) + facet_wrap(~team.abbr, ncol = 2)

getwd(
      )
      
      
