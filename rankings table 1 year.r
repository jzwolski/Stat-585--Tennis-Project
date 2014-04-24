###Create a table of rankings for 1 year###
rank <- "http://www.atpworldtour.com/Rankings/Singles.aspx?d=17.03.2014&r=1&c=#"
rank
tables <- readHTMLTable(rank)
ldply(tables, dim)
lastrank <- tables[[1]]
head(lastrank)
tail(lastrank)
dim(lastrank)
names(lastrank)[4] <- "Tourn Played"
names(lastrank)[3] <- "Week Change"
names(lastrank)[2] <- "Points"
names(lastrank)[1] <- "Name"

#Regular Expression Hints:
#gsub("[^aeiouy]{2,}", '', x)
#gsub("[^0-9]([^a-z]{1,}, [a-z]{1,})$", '\\1', lastrank$Name)

#Long Method:
gsub(".\r", '', lastrank$Name)
gsub(".\t", '', lastrank$Name)
gsub('\r\t\r', '', lastrank$Name)
gsub('\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t', '', lastrank$Name)
#Remove numbers
gsub('[0-9]', '', lastrank$Name)
gsub("\\t", "", lastrank$Name)


#Shortened Version:
lastrank$Name <- gsub('[0-9]{1,}\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\t', '', lastrank$Name)

lastrank$Name
lastrank$Points

lastrank <- lastrank[2:101,]

#Create last name column:
lastrank$LastName <- gsub(',.*', '', lastrank$Name)

#Create first name column:
lastrank$FirstName <- gsub('.*,', '', lastrank$Name)
lastrank$FirstName <- gsub("\\(.*", '', lastrank$FirstName)
lastrank$FirstName <- substr(lastrank$FirstName, 2, nchar(lastrank$FirstName)-1)

lastrank$FirstName

#Create country column:
lastrank$Country <- gsub(".*\\(", '', lastrank$Name)
lastrank$Country <- gsub("\\)", '', lastrank$Country)
lastrank$Country


#Write to a dataset:
write.csv(lastrank, "temp.csv", row.names=FALSE)
lastrank2 <- read.csv("temp.csv")

lastrank2$Name[2]

summary(lastrank2$Name)
class(lastrank$Name)
as.character(lastrank$Name)