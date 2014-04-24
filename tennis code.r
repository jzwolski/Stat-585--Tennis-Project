###Libraries###
library(XML)
library(plyr)
library(stringr)
library(scrapeR)
library(ggplot2)
library(reshape)
library(lubridate)

###Create table of rankings for the last 20 years###

#Indices:
#14(2013), 60(2012), 107(2011), 153(2010), 205(2009)
#257(2008), 309(2007), 362(2006), 414(2005), 466(2004)
#518(2003), 570(2002), 622(2001), 675(2000), 726(1999)
#778(1998), 830(1997), 882(1996), 935(1995), 987(1994)
#These must be incremented by 1 on the Monday of each week
#as the rankings are updated weekly.

url <- "http://www.atpworldtour.com/Rankings/Singles.aspx"
doc <- htmlParse(url)
root <- xmlRoot(doc)
years <- getNodeSet(root, "//option[@value]")
result <- ldply(years, function(y) xmlAttrs(y)["value"]) [[1]] [c(14,60,107,153,205,257,
                                                                  309,362,414,466,518,570,
                                                                  622,675,726,778,830,882,
                                                                  935,987)]

base <- "http://www.atpworldtour.com/Rankings/Singles.aspx?d="
urls <- paste0(base, rev(result))

rankings <- ldply(as.list(result), function(y) {
  result2 <- as.data.frame(readHTMLTable(paste0(base, y)) [[1]])
  result2$year <- y
  result2
})

names(rankings)[4] <- "Tourn Played"
names(rankings)[3] <- "Week Change"
names(rankings)[2] <- "Points"
names(rankings)[1] <- "Name"

rankings <- subset(rankings, Points != "Points")

rankings$Name <- gsub('[0-9]{1,}\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\t', '', rankings$Name)

rankings$LastName <- gsub(',.*', '', rankings$Name)

rankings$FirstName <- gsub('.*,', '', rankings$Name)
rankings$FirstName <- gsub("\\(.*", '', rankings$FirstName)
rankings$FirstName <- substr(rankings$FirstName, 2, nchar(rankings$FirstName) - 1)

rankings$Country <- gsub(".*\\(", '', rankings$Name)
rankings$Country <- gsub("\\)", '', rankings$Country)

rankings <- rankings[c(-1)]

rankings$PlayerID <- paste(rankings$FirstName, rankings$LastName, sep=", ")
rankings$PlayerID <- gsub(", ", " ", rankings$PlayerID)

rankings <- rankings[c(8, 6, 5, 7, 4, 1, 2, 3)]

names(rankings)[5] <- "Date"

###Get links for the players in the rankings table###
getLinks = function() { 
  links = character() 
  list(a = function(node, ...) { 
    links <<- c(links, xmlGetAttr(node, "href"))
    node 
  }, 
  links = function()links)
}

ranklinks <- c("http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.2013&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2012&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.2011&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.2010&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=28.12.2009&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.2008&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2007&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.2006&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.2005&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.2004&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.2003&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.2002&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2001&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.2000&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.1999&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=28.12.1998&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.1997&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.1996&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.1995&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.1994&r=1&c=#")

stats = NULL
for (j in 1:20)
{
  #Find all links on the rankings page#
  h1 <- getLinks()
  htmlTreeParse(file = ranklinks[j],
                handlers = h1, useInternalNodes = TRUE)
  #h1$links()
  
  #Subset the links to only links for the players#
  files.1<-h1$links()[str_detect(h1$links(), "/Tennis/Players")]
  #head(files.1)
  
  #Add an "a" to remove multiple links for the same player#
  files.2 <- paste(files.1,"a",sep="")
  #head(files.2)
  files.3 <- files.2[str_detect(files.2, ".aspxa")]
  #head(files.3)
  
  files.4<-str_replace_all(files.3, ".aspxa", ".aspx")
  #head(files.4)
  
  #Add the beginning of the url to get the full website address#
  files <- paste("http://www.atpworldtour.com",files.4,sep="")
  #files
  
  #Scrape the website for all players in top 100 for 30.12.2013#
  fulltable = NULL
  for (i in 1:100)
  {
    url <- files[i]
    doc <- scrape(url)[[1]]
    
    #Specify the path to the table to get the labels of the stats
    label.1 <-  getNodeSet(doc, path='//*[@id="playerBioInfoList"]/li/span/text()')
    #label.1
    
    label.2 <- as.character(sapply(label.1,xmlValue))
    #label.2
    
    #Remove the " : "
    stat.name <- gsub(":","",label.2)
    #stat.name
    
    #Specify the path to get the values of the stats
    value.1 <- getNodeSet(doc, path='//*[@id="playerBioInfoList"]/li/text()')
    #value.1
    
    value <- as.character(sapply(value.1,xmlValue))
    #value
    
    #Create a player name column
    player.name.1 <-  getNodeSet(doc, path='//*[@id="playerBioInfoCardHeader"]/h1/text()')
    #player.name.1
    
    player.name <- as.character(sapply(player.name.1,xmlValue))
    #player.name
    
    #Combine the labels and stats for the player
    table <- data.frame(player.name,stat.name,value,j)
    
    if(i == 1){
      fulltable = table
    } else {
      fulltable = rbind(fulltable, table)
    }
  }
  if(j == 1){
    stats = fulltable
  } else {
    stats = rbind(stats, fulltable)
  }
}

###Subset and prepare statistics table for combining with rankings table###
total.stats <- subset(stats, stats$stat.name=="Height"|
                      stats$stat.name=="Weight"|
                      stats$stat.name=="Age")

total.stats$NameYear <- paste(total.stats$player.name, total.stats$j, sep="-")

total.stats <- total.stats[-c(1,4)]

StatTable <- reshape(total.stats, idvar="NameYear", timevar="stat.name", direction="wide")

names(StatTable) <- c("NameYear","Age","Height","Weight")

###Merge rankings and StatsTable###

TennisData <- cbind(rankings, StatTable)

TennisData <- TennisData[c(-9)]

###Fix variables in TennisData###
#Fixing Height
TennisData$Height <- as.numeric(substring(gsub(".*\\(", '', as.character(TennisData$Height)), 1, 3))/2.54

#Fixing Weight
TennisData$Weight <- as.numeric(substr(TennisData$Weight, 2, 4))

#Fixing Date
TennisData$Date <- as.Date(TennisData$Date, "%d.%m.%Y")

#Fixing Age
TennisData$Age <- substr(TennisData$Age, 6, 15)
TennisData$Age <- as.Date(TennisData$Age, "%d.%m.%Y")
TennisData$Age <- as.numeric(TennisData$Date - TennisData$Age)
TennisData$Age <- floor(TennisData$Age / 365)

###Write TennisData to a csv###
write.csv(TennisData, "TennisData.csv", row.names=FALSE)

###Calculating average height, weight, and age###

AvgHeight <- tapply(TennisData$Height, TennisData$Date, mean)

AvgWeight <- tapply(TennisData$Weight, TennisData$Date, mean)

AvgAge <- tapply(TennisData$Age, TennisData$Date, mean, na.rm=T)

Year <- rev(unique(TennisData$Date))

###Plots###
#Average height by year
qplot(data=NULL, Year, AvgHeight) + geom_smooth(method="loess") + 
  labs(title = "Average Height By Year") + ylab("Avg Height (in.)")

#Average weight by year
qplot(data=NULL, Year, AvgWeight) + geom_smooth(method="loess") + 
  labs(title = "Average Weight By Year") + ylab("Avg Weight (lbs.)")

#Average age by year
qplot(data=NULL, Year, AvgAge) + geom_smooth(method="loess") + 
  labs(title = "Average Age By Year") + ylab("Avg Age")

#More on fundamentals and the mental aspect of the game, and less about physicality
#Back then, younger players could "get away" with weaker fundamentals because they were
#faster and better physically than older players.