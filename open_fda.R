require(jsonlite)
require(httr)

drug = "Metformin"

# Adverse Events endpoint; 338322 total
event_call <- GET(paste("https://api.fda.gov/drug/event.json?search=", drug, sep=""))
multiple_event_call <- GET(paste("https://api.fda.gov/drug/event.json?search=", drug, "&limit=10", "&serious=1", sep=""))
event_json <- fromJSON(content(event_call, "text"), flatten = TRUE)

View(event_json)



# trying to get ae reports/counts by year



library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
AE="ABDOMINAL HERNIA"

event_call <- GET(paste0('https://api.fda.gov/drug/event.json?search=patient.reaction.reactionmeddrapt:"', gsub(" ", "+", AE), '"&count=receiptdate'))
event_json <- fromJSON(content(event_call, "text"), flatten = TRUE)
View(event_json)


str <- event_json$results$time[1]
d <- paste(substr(str, 1, 4), substr(str, 5, 6), substr(str, 7, 8), sep="-")
dates=c()
count=c()
sysdate <- Sys.Date()
for(i in 1:length(event_json$results$time)){
  str <- event_json$results$time[i]
  d <- paste(substr(str, 1, 4), substr(str, 5, 6), substr(str, 7, 8), sep="-")
  if(d<sysdate){
    dates=c(dates, d)
    count=c(count, event_json$results$count[i])
  }
}
df <- data.frame("Date"=as.Date(dates), "Count"=count)
df
p <- df %>%
  ggplot( aes(x=Date, y=Count)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab(paste(AE, "Count")) +
  theme_ipsum()

ggplotly(p)


# drug data

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

substance="abacavir" #does captilization matter? no

event_call <- GET(paste0('https://api.fda.gov/drug/event.json?search=patient.drug.medicinalproduct:', gsub(" ", "+", substance), '&count=receiptdate'))
event_json <- fromJSON(content(event_call, "text"), flatten = TRUE)
View(event_call)
View(event_json)

dates=c()
count=c()
sysdate <- Sys.Date()
for(i in 1:length(event_json$results$time)){
  str <- event_json$results$time[i]
  d <- paste(substr(str, 1, 4), substr(str, 5, 6), substr(str, 7, 8), sep="-")
  if(d<sysdate){
    dates=c(dates, d)
    count=c(count, event_json$results$count[i])
  }
}
df <- data.frame("Date"=as.Date(dates), "Count"=count)
df
p <- df %>%
  ggplot( aes(x=Date, y=Count)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab(paste(substance, "Count")) +
  theme_ipsum()

ggplotly(p)

