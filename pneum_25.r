#----------------------------------------------------------------------------------------------------------------------------------------------------

setwd("/home/jwafula/combined.hospitals/data")
require(data.table)

## Age variable: convert to months (After getting and cleaning data):

data.copy[age_days< 0 | age_days > 30 | age_days%in% c("empty",-1,"Empty","" ),age_days := as.numeric(NA)]
data.copy[age_years< 0 | age_years > 18 | age_years%in% c("empty",-1,"Empty","" ),age_years := as.numeric(NA)]
data.copy[age_mths< 0 | age_mths > 60 | age_mths%in% c("empty",-1,"Empty","" ),age_mths := as.numeric(NA)]

data.copy[,ages.in.months:=apply(.SD, 1, function(x){
  return(sum(c(x["age_days"]/30,
               x["age_mths"],
               x["age_years"]*12), na.rm=T))
}),.SDcols=c('age_days','age_mths','age_years')]

#----------------------------------------------------------------------------------------------------------------------------------------------------

data_1 <- copy(data.copy)

#### detach(data_1) #####
#### attach(data_1) ####

par(mfrow=c(2,3))

#=========================================
CleanData <- data_1[which(ages.in.months > 1 & ages.in.months <= 59 & (!is.na(dx1_pneum) | !is.na(dx2_pneum)))]

CleanData <- data.table(CleanData)

#2.

require(zoo)

dates = as.yearmon(seq(as.Date("2014-03-01"), as.Date("2016-03-31"), by="month"))

date <- seq(as.Date("2014-03-01"), as.Date("2016-03-31"), by="month")

d <- data.frame(date)

start.month <- as.Date("2014-03-01")

end.start.month <- as.Date("2014-04-01")

startPeriod   = seq(start.month, length = 25, by = "month")

endPeriod     = seq(end.start.month, length = 25, by = "month")

reporting_period_starts = startPeriod[-26]

reporting_period_ends   = endPeriod

counts.period <- sapply(dates, function(x){gsub(" ","\\_",x)})


#Subsetting all recods

monthly.counts.all.Data <- paste0(counts.period,"all = CleanData[which((!is.na(as.Date(date_today)) & as.Date(date_today) >= as.Date(","\"",reporting_period_starts,"\"",",",'\'',"%Y-%m-%d",'\'',")",")", "&",
                                  
                                  "(!is.na(as.Date(date_today)) & as.Date(date_today) < as.Date(","\"",reporting_period_ends,"\"",",",'\'',"%Y-%m-%d",'\'',")",")",")","]")

eval(parse(text = monthly.counts.all.Data))

#Summing up

monthly.sum.all.counts <- paste0(counts.period, "records <- nrow(",counts.period,"all)")

eval(parse(text=monthly.sum.all.counts))


# y <- paste0(counts.period, "records")
# y <- noquote(y)
s <- noquote(c(paste0(counts.period, "records",collapse = ",")))
#data_copy_all
data.all.counts <- data.table(rbind(Mar_2014records,Apr_2014records,May_2014records,Jun_2014records,Jul_2014records,Aug_2014records,Sep_2014records,Oct_2014records,Nov_2014records,Dec_2014records,Jan_2015records,Feb_2015records,Mar_2015records,Apr_2015records,May_2015records,Jun_2015records,Jul_2015records,Aug_2015records,Sep_2015records,Oct_2015records,Nov_2015records,Dec_2015records,Jan_2016records,Feb_2016records,Mar_2016records))
eval(parse(text=data.all.counts))

#===================================
CleanData_deaths <- data_1[which(ages.in.months > 1 & ages.in.months <= 59 & (!is.na(dx1_pneum) | !is.na(dx2_pneum)) & (is.na(outcome)==F & as.factor(outcome)=="Died"))]

CleanData_deaths <- data.table(CleanData_deaths)

monthly.counts.deaths.Data <- paste0(counts.period,"all = CleanData_deaths[which((!is.na(as.Date(date_today)) & as.Date(date_today) >= as.Date(","\"",reporting_period_starts,"\"",",",'\'',"%Y-%m-%d",'\'',")",")", "&",
                                     
                                     "(!is.na(as.Date(date_today)) & as.Date(date_today) < as.Date(","\"",reporting_period_ends,"\"",",",'\'',"%Y-%m-%d",'\'',")",")",")","]")

eval(parse(text = monthly.counts.deaths.Data))

#Summing up

monthly.sum.deaths.counts <- paste0(counts.period, "deaths <- nrow(",counts.period,"all)")

eval(parse(text=monthly.sum.deaths.counts))

data.deaths.counts <- data.table(rbind(Mar_2014deaths,Apr_2014deaths,May_2014deaths,Jun_2014deaths,Jul_2014deaths,Aug_2014deaths,Sep_2014deaths,Oct_2014deaths,Nov_2014deaths,Dec_2014deaths,Jan_2015deaths,Feb_2015deaths,Mar_2015deaths,Apr_2015deaths,May_2015deaths,Jun_2015deaths,Jul_2015deaths,Aug_2015deaths,Sep_2015deaths,Oct_2015deaths,Nov_2015deaths,Dec_2015deaths,Jan_2016deaths,Feb_2016deaths,Mar_2016deaths))

eval(parse(text=data.deaths.counts))
#===================================

dataPlot <- c(data.all.counts[1],data.all.counts[2],data.all.counts[3],data.all.counts[4],data.all.counts[5],data.all.counts[6],data.all.counts[7],data.all.counts[8],data.all.counts[9],data.all.counts[10],data.all.counts[11],data.all.counts[12],data.all.counts[13],data.all.counts[14],data.all.counts[15],data.all.counts[16],data.all.counts[17],data.all.counts[18],data.all.counts[19],data.all.counts[20],data.all.counts[21],data.all.counts[22],data.all.counts[23],data.all.counts[24],data.all.counts[25])

dataPlotDeaths <- c(data.deaths.counts[1],data.deaths.counts[2],data.deaths.counts[3],data.deaths.counts[4],data.deaths.counts[5],data.deaths.counts[6],data.deaths.counts[7],data.deaths.counts[8],data.deaths.counts[9],data.deaths.counts[10],data.deaths.counts[11],data.deaths.counts[12],data.deaths.counts[13],data.deaths.counts[14],data.deaths.counts[15],data.deaths.counts[16],data.deaths.counts[17],data.deaths.counts[18],data.deaths.counts[19],data.deaths.counts[20],data.deaths.counts[21],data.deaths.counts[22],data.deaths.counts[23],data.deaths.counts[24],data.deaths.counts[25])

foo = ts(dataPlot, frequency = 25, start = 2014)
zoo = ts(dataPlotDeaths, frequency = 25, start = 2014)
plot(foo, type="line", col = "blue", main = "Pneumonia Adm (blue) & Deaths (red) - all CIN Hospitals", xlab = "Month", ylab = "Cases (age: 1 - 59 Mnths)",xaxt = "n", ylim = c(0,1350))
#grid(500, 500, lwd = .25)
#par(bg = 'blue')
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
lines(zoo, col = "red")
tsp = attributes(foo)$tsp
datess = seq(as.Date("2014-03-01"), by = "month", along = foo)
axis(1, at = seq(tsp[1], tsp[2], along = foo), labels = format(datess, "%Y-%m"))
legend("topright", c("Adm.","Deaths"),lwd=c(1.5,1.5), bty='n', cex =.75,  col=c("blue","red"))
#=========================================


for (hosp_name in c("hospX1 DH","hospX2 DH")){
CleanData <- data_1[which(ages.in.months > 1 & ages.in.months <= 59 & (!is.na(dx1_pneum) | !is.na(dx2_pneum)) & hosp_id==hosp_name)]

CleanData <- data.table(CleanData)

#2.

require(zoo)

dates = as.yearmon(seq(as.Date("2014-03-01"), as.Date("2016-03-31"), by="month"))

date <- seq(as.Date("2014-03-01"), as.Date("2016-03-31"), by="month")

d <- data.frame(date)

start.month <- as.Date("2014-03-01")

end.start.month <- as.Date("2014-04-01")

startPeriod   = seq(start.month, length = 25, by = "month")

endPeriod     = seq(end.start.month, length = 25, by = "month")

reporting_period_starts = startPeriod[-26]

reporting_period_ends   = endPeriod

counts.period <- sapply(dates, function(x){gsub(" ","\\_",x)})


#Subsetting all recods

monthly.counts.all.Data <- paste0(counts.period,"all = CleanData[which((!is.na(as.Date(date_today)) & as.Date(date_today) >= as.Date(","\"",reporting_period_starts,"\"",",",'\'',"%Y-%m-%d",'\'',")",")", "&",
                                  
                                  "(!is.na(as.Date(date_today)) & as.Date(date_today) < as.Date(","\"",reporting_period_ends,"\"",",",'\'',"%Y-%m-%d",'\'',")",")",")","]")

eval(parse(text = monthly.counts.all.Data))

#Summing up

monthly.sum.all.counts <- paste0(counts.period, "records <- nrow(",counts.period,"all)")

eval(parse(text=monthly.sum.all.counts))


# y <- paste0(counts.period, "records")
# y <- noquote(y)
s <- noquote(c(paste0(counts.period, "records",collapse = ",")))
#data_copy_all
data.all.counts <- data.table(rbind(Mar_2014records,Apr_2014records,May_2014records,Jun_2014records,Jul_2014records,Aug_2014records,Sep_2014records,Oct_2014records,Nov_2014records,Dec_2014records,Jan_2015records,Feb_2015records,Mar_2015records,Apr_2015records,May_2015records,Jun_2015records,Jul_2015records,Aug_2015records,Sep_2015records,Oct_2015records,Nov_2015records,Dec_2015records,Jan_2016records,Feb_2016records,Mar_2016records))
eval(parse(text=data.all.counts))

#===================================
CleanData_deaths <- data_1[which(ages.in.months > 1 & ages.in.months <= 59 & (!is.na(dx1_pneum) | !is.na(dx2_pneum)) & (is.na(outcome)==F & as.factor(outcome)=="Died") & hosp_id==hosp_name)]

CleanData_deaths <- data.table(CleanData_deaths)

monthly.counts.deaths.Data <- paste0(counts.period,"all = CleanData_deaths[which((!is.na(as.Date(date_today)) & as.Date(date_today) >= as.Date(","\"",reporting_period_starts,"\"",",",'\'',"%Y-%m-%d",'\'',")",")", "&",
                                     
                                     "(!is.na(as.Date(date_today)) & as.Date(date_today) < as.Date(","\"",reporting_period_ends,"\"",",",'\'',"%Y-%m-%d",'\'',")",")",")","]")

eval(parse(text = monthly.counts.deaths.Data))

#Summing up

monthly.sum.deaths.counts <- paste0(counts.period, "deaths <- nrow(",counts.period,"all)")

eval(parse(text=monthly.sum.deaths.counts))

data.deaths.counts <- data.table(rbind(Mar_2014deaths,Apr_2014deaths,May_2014deaths,Jun_2014deaths,Jul_2014deaths,Aug_2014deaths,Sep_2014deaths,Oct_2014deaths,Nov_2014deaths,Dec_2014deaths,Jan_2015deaths,Feb_2015deaths,Mar_2015deaths,Apr_2015deaths,May_2015deaths,Jun_2015deaths,Jul_2015deaths,Aug_2015deaths,Sep_2015deaths,Oct_2015deaths,Nov_2015deaths,Dec_2015deaths,Jan_2016deaths,Feb_2016deaths,Mar_2016deaths))

eval(parse(text=data.deaths.counts))
#===================================

dataPlot <- c(data.all.counts[1],data.all.counts[2],data.all.counts[3],data.all.counts[4],data.all.counts[5],data.all.counts[6],data.all.counts[7],data.all.counts[8],data.all.counts[9],data.all.counts[10],data.all.counts[11],data.all.counts[12],data.all.counts[13],data.all.counts[14],data.all.counts[15],data.all.counts[16],data.all.counts[17],data.all.counts[18],data.all.counts[19],data.all.counts[20],data.all.counts[21],data.all.counts[22],data.all.counts[23],data.all.counts[24],data.all.counts[25])

dataPlotDeaths <- c(data.deaths.counts[1],data.deaths.counts[2],data.deaths.counts[3],data.deaths.counts[4],data.deaths.counts[5],data.deaths.counts[6],data.deaths.counts[7],data.deaths.counts[8],data.deaths.counts[9],data.deaths.counts[10],data.deaths.counts[11],data.deaths.counts[12],data.deaths.counts[13],data.deaths.counts[14],data.deaths.counts[15],data.deaths.counts[16],data.deaths.counts[17],data.deaths.counts[18],data.deaths.counts[19],data.deaths.counts[20],data.deaths.counts[21],data.deaths.counts[22],data.deaths.counts[23],data.deaths.counts[24],data.deaths.counts[25])

foo = ts(dataPlot, frequency = 25, start = 2014)
zoo = ts(dataPlotDeaths, frequency = 25, start = 2014)
plot(foo, type="line", col = "blue", main = paste("Pneumonia Adm (blue) & Deaths (red)", hosp_name, collapse=" ", sep=" - "), xlab = "Month", ylab = "Cases (age: 1 - 59 Mnths)",xaxt = "n", ylim = c(0,200))
#grid(500, 500, lwd = .25)
#par(bg = 'blue')
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
lines(zoo, col = "red")
tsp = attributes(foo)$tsp
datess = seq(as.Date("2014-03-01"), by = "month", along = foo)
axis(1, at = seq(tsp[1], tsp[2], along = foo), labels = format(datess, "%Y-%m"))
legend("topright", c("Adm.","Deaths"),lwd=c(1.5,1.5), bty='n', cex =.75,  col=c("blue","red"))
}
