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

Final = NULL
for (j in 1:20)
{
  #Find all links on the rankings page#
  h1<-getLinks()
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
  df = NULL
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
    table <- data.frame(player.name,stat.name, value)
    
    
    #table <- table[which(table$stat.name=="Height" & table$stat.name=="Age"),]
    
    #table <- subset(table, c(table$stat.name=="Age", table$stat.name=="Height"))
    
    #if(nrow(table[which(table$stat.name=="Age"),])==0){
    #  table <- table[which(table$stat.name=="Birthplace"),]
    #}
    
    #table.1 <- with(table,data.frame(table$player.name[1],
    #                      table[stat.name=="Age",]$value,
    #                      table[stat.name=="Height",]$value,
    #                      table[stat.name=="Weight",]$value))
    #table1 <- data.frame(table$player.name[1], table[table$stat.name=="Height",]$value)
    
    #names(table.1) = c("Name", "Age", "Height", "Weight")
    #names(table1) = c("Name", "Height")
    
    #table
    #table <- table[which(table$stat.name=="Height"),]
    
    #table2 <- reshape(table, idvar="player.name", timevar="stat.name", direction="wide")
    #table$Height = as.numeric(substring(gsub(".*\\(", '', as.character(table$value)), 1, 3))/2.54
    
    if(i == 1){
      df = table
      #df = table[,-c(2:3)]
    } else {
      df = rbind(df, table)
      #df = rbind(df, table[,-c(2:3)])
    }
  }
  if(j == 1){
    Final = df
  } else {
    Final = rbind(Final, df)
  }
}


reshape(Final, idvar="player.name", timevar="stat.name", direction="wide")