setwd("/Users/David/Desktop/Coursera")
source("ipak.R")
packages <- c("plyr", "lattice", "data.table", "httr", "ggplot2")
ipak(packages)

dossier <- "/repdata_data_activity.zip"
donnes <- "donnes"
if(!file.exists(donnes)){
  dir.create(donnes)
} 
graph <- "graph" 
if(!file.exists(graph)){
  dir.create(graph)
}
zip <- paste(getwd(), "/repdata_data_activity.zip", sep = "")
if(!file.exists(zip)){
  download.file(dossier, zip, method="curl", mode="wb")
}
archives <- paste(getwd(), "/donnes/activity.csv", sep = "")
if(!file.exists(archives)){
  unzip(zip, list = FALSE, overwrite = FALSE, exdir = donnes)
}

activity <- read.table(file = archives, header = TRUE, sep = ",")

## Partie 2
activity$dateheure <- as.POSIXct(with(activity, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
                                  format="%Y-%m-%d %H:%M",tz="")

nbpas <- setNames(aggregate(steps~as.Date(date), activity, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(nbpas), by = 6)

echelle <- list(x = list(rot = 45, cex = 1.0, labels = format(nbpas$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = nbpas, main = "steps per day", ylab = "steps", xlab = "date", scales = echelle, horizontal = F)

paste("mean:", mean(nbpas$steps))
paste("median:", median(nbpas$steps))

## Partie 3

activitequot <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(activitequot, type = "l", axes = F, xlab = "Time of the day", 
     ylab = "Average across all days provided a time", main = "Average number of steps taken", 
     col = "red")
axis(1,at=c(seq(0,2400,100),835), label = paste(c(seq(0,24,1),8),c(rep(":00",25),":40"),sep=""), pos = 0)
axis(2, at=c(seq(0,210,30),206.2), label = c(seq(0,210,30),206.2), pos = 0)
max <- which.max(activitequot$steps)
segments(832, 0, 832, 206.2, col = "blue", lty = "dashed")
text(835,200, "max average of steps: (832,206.2)", col = "blue", adj = c(-.1, -.1))
segments(0, 206.2, 832, 206.2, col = "blue", lty = "dashed")
activitequot [max, ]

paste(835, "équivalent à 8.667 heures, le max est atteind à 08:40 ")

## Partie 4

paste("missing observations:", sum(is.na(activity$steps)))


horaires <- activity
horaires[is.na(activity$steps), ]$steps <- mean(activity$steps)

horaires$dateheure <- as.POSIXct(with(horaires, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
                                     format="%Y-%m-%d %H:%M",tz="")

nb2pas <- setNames(aggregate(steps~as.Date(date), horaires, sum, na.rm = TRUE), c("date","steps"))

xaxe <- seq(1, nrow(nb2pas), by = 6)

echelle2 <- list(x = list(rot = 45, cex = 1.0, labels = format(nb2pas$date, "%d-%b-%Y")[xaxe], at = xaxe))

barchart(date ~ steps, data = nb2pas, main = "steps per day", ylab = "steps", xlab = "date", scales = echelle2, horizontal = F)

paste("mean:", mean(nb2pas$steps))
paste("median:", median(nb2pas$steps))

paste("difference mean:", mean(nb2pas$steps)-mean(nbpas$steps))
paste("difference median:", median(nb2pas$steps)-median(nbpas$steps))


## partie 5

str(horaires)
horaires$date <- as.Date(horaires$date, "%Y-%m-%d")
horaires$day <- weekdays(horaires$date)
horaires$jourchoisi <- c("weekday")
for (i in 1:nrow(horaires)){
  if (horaires$day[i] == "Saturday" || horaires$day[i] == "Sunday"){
    horaires$jourchoisi[i] <- "weekend"
  }
}
horaires$jourchoisi <- as.factor(horaires$jourchoisi)
weekend <- aggregate(steps ~ interval+jourchoisi, horaires, mean)
qplot(interval, steps, data=weekend, geom=c("line"), xlab="5-min intervals", 
      ylab="steps mean", main="") + facet_wrap(~ jourchoisi, ncol=1)