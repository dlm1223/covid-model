#run this to get updated data. then run 1-organize-data.R, then 2-model-fitting.

library(data.table)
library(plyr)
library(dplyr)
library(reshape2)
wd<-""

##READING IN DATA FROM JHU GITHUB AND RESHAPE WIDE TO LONG

recoveries_time_series<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recoveries_time_series<-melt(recoveries_time_series, id.vars=c("Province.State", "Country.Region", "Lat", "Long"), variable.name = "Date", value.name  ="Recoveries")
recoveries_time_series$Date<-as.Date(recoveries_time_series$Date, format="X%m.%d.%y")
recoveries_time_series<-recoveries_time_series[, !colnames(recoveries_time_series)%in% c("Lat", "Long")]
tail(recoveries_time_series)
str(recoveries_time_series)
write.csv(recoveries_time_series, file=paste0(wd, "externaljhu/recoveries.csv"), row.names = F)

recoveries_time_series[recoveries_time_series$Country.Region=='US',]


confirmed.US<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed.US<-melt(confirmed.US, id.vars=colnames(confirmed.US)[!grepl("20", colnames(confirmed.US))], variable.name = "Date", value.name="ConfirmedCases")
colnames(confirmed.US)<-gsub("_", ".", colnames(confirmed.US))

confirmed.US$Date<-as.Date(confirmed.US$Date, format="X%m.%d.%y")
write.csv(confirmed.US, file=paste0(wd, "externaljhu/cases_us_county.csv"), row.names = F)


confirmed.US<-data.table(confirmed.US)[, list(ConfirmedCases=sum(ConfirmedCases)),by=c("Province.State", "Country.Region", "Date")]
tail(confirmed.US)
write.csv(confirmed.US, file=paste0(wd, "externaljhu/cases_us.csv"), row.names = F)



deaths.US<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths.US<-melt(deaths.US, id.vars=colnames(deaths.US)[!grepl("20", colnames(deaths.US))], variable.name = "Date", value.name="Fatalities")
colnames(deaths.US)<-gsub("_", ".", colnames(deaths.US))

deaths.US$Date<-as.Date(deaths.US$Date, format="X%m.%d.%y")
write.csv(deaths.US, file=paste0(wd, "externaljhu/deaths_us_county.csv"), row.names = F)
head(deaths.US)

deaths.US<-data.table(deaths.US)[, list(Fatalities=sum(Fatalities)),by=c("Province.State", "Country.Region", "Date")]
write.csv(deaths.US, file=paste0(wd, "externaljhu/deaths_us.csv"), row.names = F)
#deaths.US[deaths.US$Province.State=="New York",]


confirmed.global<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed.global<-melt(confirmed.global, id.vars=colnames(confirmed.global)[!grepl("20", colnames(confirmed.global))], variable.name = "Date", value.name="ConfirmedCases")
colnames(confirmed.global)<-gsub("_", ".", colnames(confirmed.global))
confirmed.global<-confirmed.global[, !colnames(confirmed.global)%in% c("Lat", "Long")]
confirmed.global$Date<-as.Date(confirmed.global$Date, format="X%m.%d.%y")

tail(confirmed.global)

write.csv(confirmed.global, file=paste0(wd, "externaljhu/cases.csv"), row.names = F)




deaths.global<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths.global<-melt(deaths.global, id.vars=colnames(deaths.global)[!grepl("20", colnames(deaths.global))], variable.name = "Date", value.name="Fatalities")
colnames(deaths.global)<-gsub("_", ".", colnames(deaths.global))
deaths.global<-deaths.global[, !colnames(deaths.global)%in% c("Lat", "Long")]
deaths.global$Date<-as.Date(deaths.global$Date, format="X%m.%d.%y")

tail(deaths.global)

write.csv(deaths.global, file=paste0(wd, "externaljhu/deaths.csv"), row.names = F)
