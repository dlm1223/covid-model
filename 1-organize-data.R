rm(list=ls())
library(data.table)
library(plyr)
library(dplyr)
library(geosphere)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(tseries)
library(forecast)
library(lightgbm)
options(stringsAsFactors=FALSE)
options(scipen=999)


###READ IN DATA#######

wd<-""

train<-Reduce(function(x, y) merge(x, y, by=c("Province.State", "Country.Region", "Date"), all=TRUE),
              list(read.csv(paste0(wd, "externaljhu/deaths.csv")), 
                   read.csv(paste0(wd, "externaljhu/cases.csv")), 
                   read.csv(paste0(wd, "externaljhu/recoveries.csv") )
              ))
#state-data:
# train2<-merge(read.csv(paste0(wd, "externaljhu/deaths_US.csv")), read.csv(paste0(wd, "externaljhu/cases_US.csv")), by=c("Province.State", "Country.Region", "Date"))
# train<-rbind.fill(train, train2)



coordName<-function(x){
  x<-gsub("^\\s+", "", x)
  abbreviations<-read.csv(paste0(wd, "external/state_abbreviations.csv"))
  x[x%in% abbreviations$Abbrev]<-abbreviations$State[match(x[x%in% abbreviations$Abbrev],abbreviations$Abbrev )]
  x[grepl("Kitts", x)]<-"Saint Kitts and Nevis"
  x[grepl("Venezuela ", x)]<-'Venezuela'
  x[grepl("Bolivia ", x)]<-'Bolivia'
  x[grepl("Brunei ", x)]<-'Brunei'
  x[grepl("Tanzania", x)]<-"Tanzania"
  x[grepl("Republic of Moldova", x)]<-'Moldova'
  x[x=='Burma']<-'Myanmar'
  x[x=='Syrian Arab Republic']<-"Syria"
  x[x=="U.A.E."]<-"United Arab Emirates"
  x[grepl("Palestine", x)]<-"West Bank and Gaza"
  
  x[x=='United States of America'|x=='U.S.']<-'US'
  x[x=='Russian Federation']<-'Russia'
  x[x=="Viet Nam"]<-"Vietnam"
  x[x=="GU"]<-"Guam"
  x[x=="VI"|x=="U.S. Virgin Islands"]<-"Virgin Islands"
  x[x=="PR"]<-"Puerto Rico"
  #x[x=="Myanmar"]<-"Laos"
  x[x=='Ivory Coast'|grepl("Ivoire", x)]<-"Cote d'Ivoire"
  x[x=="Macao"]<-"Macau"
  x[x=="Saint Martin"]<-"St Martin"
  x[x=="Faeroe Islands"]<-"Faroe Islands"
  x[x=="Réunion"]<-"Reunion"
  x[x=="St. Barth" ]<-"Saint Barthelemy"
  x[x=='UAE']<-"United Arab Emirates"
  x[x== "St. Vincent Grenadines"]<-"Saint Vincent and the Grenadines"
  x[x=='CAR']<-"Central African Republic"
  x[x=='United States']<-"US"
  x[x=='USA']<-"US"
  x[x=="Cape Verde"]<-"Cabo Verde"
  x[x=="Timor"]<-"Timor-Leste"
  x[x=="Swaziland"| grepl("Swaziland", x)]<-"Eswatini"
  x[x=="Curaçao"  ]<-"Curacao"
  x[x=="Gambia"]<-"The Gambia"
  x[x=="Congo"]<-'Republic of the Congo'
  x[x=='South Korea'|x=='Republic of Korea'|x=="S. Korea"]<-"Korea, South"
  x[grepl("Iran ", x)]<-"Iran"
  x[x=="Mainland China"]<-"China"
  x[x=="UK"]<-"United Kingdom"
  x[x==""]
  x[grepl("Taiwan", x)]<-"Taiwan"
  x[x=='Bahamas']<-'The Bahamas'
  x[x=='Czech Republic']<-"Czechia"
  x[x=='Macedonia']<-'North Macedonia'
  x
}

#rmsle just root of mean-squarred-log error
rmsle<-function(y, y_pred, na.rm=T, add=1){
  #add 1 to generalize so can apply to 0 values
  n<-sum(!is.na(y_pred))
  sqrt(1/n*sum((log(y_pred+add)-log(y+add))^2, na.rm=na.rm))
}


#EXTERNAL DATA: STATE AND COUNTRY LEVEL DATASETS######

#from https://www.kaggle.com/koryto/countryinfo
country.info<-read.csv(paste0(wd, "external/covid19countryinfo.csv" ))
country.info$region[country.info$region==country.info$country]<-''
country.info$country<-coordName(country.info$country)


# outside.data<-read.csv(paste0(wd,"external/Data Join - RELEASE.csv"), na.strings = c("#N/A", "NA"))
# colnames(outside.data)<-gsub("_", ".", colnames(outside.data))
# outside.data$Province.State[outside.data$Country.Region==outside.data$Province.State]<-""
# outside.data$Country.Region<-coordName(outside.data$Country.Region)
# outside.data[outside.data$Country.Region=='Botswana',]


#region metadata: Lat, Lng, Population, Area. From: https://www.kaggle.com/rohanrao/covid19-forecasting-metadata
region.info<-read.csv(paste0(wd, "external/region_metadata.csv"))
colnames(region.info)<-c("Country.Region", "Province.State", "Lat", "Long", "Continent", "Population", "Area")
region.info[, c("Population", "Area")]<-sapply(region.info[, c("Population", "Area")], function(x) as.numeric(gsub("[,]", "", x) ))
region.info$Density<-region.info$Population/(1+region.info$Area)
region.info$Province.State[region.info$Country.Region==region.info$Province.State]<-""
region.info$Country.Region<-coordName(region.info$Country.Region)
region.info[1:3,]

#ages data:
ages<-read.csv(paste0(wd, "external/Age-structure-2.csv"))
ages$Country<-coordName(ages$Country)
ages[, 2:4]<-sapply(ages[, 2:4], function(x) as.numeric(gsub("%", "", x)))
colnames(ages)[2:4]<-c("age1.14", "age15.65", "age66")


ages2<-read.csv(paste0(wd, "external/Age-structure-1.csv"))
ages2$Location<-coordName(ages2$Location)
setdiff(train$Country.Region, ages2$Location)
ages2<-data.table(ages2)[,list(age60.male=100*PopMale[AgeGrp=="60+"]/sum(PopTotal), 
                               age60.fem=100*PopFemale[AgeGrp=="60+"]/sum(PopTotal),
                               age60=100*PopTotal[AgeGrp=="60+"]/sum(PopTotal)
) , by="Location"]



list.files(paste0(wd, "external/"))

#from Kaggle: https://www.kaggle.com/jcyzag/covid19-lockdown-dates-by-country
restrictions<-read.csv(paste0(wd, "external/countryLockdowndates.csv"), na.strings = "")
restrictions$Date<-as.Date(restrictions$Date, format="%d/%m/%y")
colnames(restrictions)[2:3]<-c("Province.State","quarantine")
restrictions[, c("Country.Region", "Province.State")]<-sapply(restrictions[, c("Country.Region", "Province.State")], coordName)
restrictions$Province.State[restrictions$Country.Region==restrictions$Province.State| is.na(restrictions$Province.State)]<-''
restrictions<-rbind(restrictions, data.frame(Country.Region="US", Province.State="", quarantine=as.Date("2020-03-25"), Type="Full", Reference="idk"))

restrictions<-restrictions[restrictions$Type=='Full', ]
restrictions<-restrictions[!duplicated(restrictions[, c("Country.Region", "Province.State")]), ]


#outside data ourworldindata.org

list.files(paste0(wd,"externalourworldindata/"))
read.file<-function(file){
  print(file)
  #file<-"comparing-the-share-of-men-and-women-who-are-smoking.csv"
  dataset<-read.csv(paste0(wd, paste0("externalourworldindata/", file)))
  dataset<-dataset[order(dataset$Year,decreasing = F),]
  if(file=="comparing-the-share-of-men-and-women-who-are-smoking.csv"){
    dataset<-dataset[!is.na(dataset[,4]),]
  }
  dataset<-dataset[!duplicated(dataset$Entity, fromLast = T),]
  dataset<-dataset[, !colnames(dataset)%in% c("Code"),]
  colnames(dataset)[!colnames(dataset)%in% c("Entity", "Year")]<-paste0(colnames(dataset)[!colnames(dataset)%in% c("Entity", "Year")], max(dataset$Year))
  colnames(dataset)[colnames(dataset)=="Entity"]<-"Country"
  dataset$Year<-NULL
  
  # a few error analysis
  if(file=="comparing-the-share-of-men-and-women-who-are-smoking.csv"){
    dataset$X2016<-NULL
    colnames(dataset)<-c("Country", "Share.of.women.smoking.2019", "Share.of.men.smoking.2019")
  } else if (file=="international-tourism-number-of-arrivals.csv"){
    colnames(dataset)<-c("Country", "Tourism.Arrivals.2016")
  }
  colnames(dataset)<-gsub("[.][.]|[.][.][.]|[.][.][.][.]|[.][.][.][.][.]", ".", colnames(dataset))
  
  dataset
}
world.in.data<-list.files(paste0(wd, "externalourworldindata/"))
world.in.data<-lapply(world.in.data, read.file)
world.in.data<- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Country", all = TRUE),
                       world.in.data)

colnames(world.in.data)

list.files(paste0(wd, "externalworldbank/"))

#world bank data

read.file<-function(file){
  print(file)
  df<-read.csv(paste0(wd, "externalworldbank/", file))
  df<-df[, !colnames(df)=='Value.Footnotes' & !grepl("can[.]be[.]trusted", colnames(df))]
  colnames(df)[1]<-'Country'
  df$Country<-coordName(df$Country)
  if("Year"%in% colnames(df)){
    df<-df[df$Year==max(df$Year),]
    df$Year<-NULL
  }
  df<-df[ , !(sapply(df, function(x) sum(is.na(x)))>100)]
  if(colnames(df)[ncol(df)]=='Value'){
    colnames(df)[ncol(df)]<-gsub("[.]csv", "", file)
  }
  df
}
world.bank<-list.files(paste0(wd, "externalworldbank/"))
world.bank<-lapply(world.bank, read.file)

world.bank<- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Country", all = TRUE),
                    world.bank)
world.bank<-world.bank[ , !(sapply(world.bank, function(x) sum(is.na(x)))>150)]
world.bank[1,]
colnames(world.bank)


# files<-gsub("//", "/", list.files(paste0(wd, "externalworldometer/"), pattern='csv', full.names = T))
# readFile<-function(file){
#   data<-read.csv(file)
#   if(sum(grepl("Critical", colnames(data)))>0){
#     country.col<-colnames(data)[grepl("Country", colnames(data))]
#     critical.col<-colnames(data)[grepl("Critical", colnames(data))]
#     data<-data[, c(country.col, critical.col, "Source", "Date")]
#     colnames(data)[1:2]<-c("Country.Region", "CriticalCases")
#   } else{
#     data<-data.frame()
#   }
#   data
# }
# worldometer<-lapply(files, readFile)
# worldometer<-rbindlist(worldometer)%>% data.frame()
# worldometer$Country.Region<-coordName(worldometer$Country.Region)
# worldometer$CriticalCases[worldometer$CriticalCases==""]<-NA
# worldometer$CriticalCases<-as.numeric(gsub(",", "", worldometer$CriticalCases))
# worldometer<-worldometer[!is.na(worldometer$CriticalCases),]
# worldometer<-worldometer[!worldometer$Country.Region%in%c("South America", "World", "Europe", "North America", "Africa", "Oceania", "", 'Asia', 'Total:'), ]
# worldometer$Province.State<-""
# worldometer$Province.State[worldometer$Country.Region=='Hong Kong']<-"Hong Kong"
# worldometer$Country.Region[worldometer$Country.Region=='Hong Kong']<-"China"






testing<-read.csv(paste0(wd, "external/Tests.csv"))
testing$date<-as.Date(testing$date)
testing$country<-coordName(testing$country)
testing<-testing[!duplicated(testing[, c("country", "date")]),]
testing<-testing[, c("country", "date", "tests_cumulative")]
colnames(testing)<-c("Country.Region", "Date", "Tests.Country")
testing<-testing[order(testing$Date, decreasing = F),]
testing<-data.table(testing)[, `:=`(Tests.Country.lag=lag(Tests.Country, 1), min.Date=min(Date)), by=c("Country.Region")]
testing$Tests.Country[testing$Tests.Country==testing$Tests.Country.lag]<-NA
testing$Province.State<-""
testing$Tests.Country[(testing$Country.Region%in% c("France", "Germany", "Philippines")& testing$Date==as.Date("2020-03-25"))]<-NA
testing$Tests.Country[(testing$Country.Region%in% c("Spain", 'Brazil')& testing$Date==as.Date("2020-03-18"))]<-NA

#testing data from https://www.kaggle.com/koryto/countryinfo
testing2<-read.csv(paste0(wd, "external/covid19tests.csv"))
testing2<-testing2[, c("region", "country", "tests",  'date')]
testing2$date<-as.Date(testing2$date, format='%d-%B')
testing2$country<-coordName(testing2$country)
testing2<-testing2[!testing2$country=="US",]
colnames(testing2)<-c("Province.State", 'Country.Region', "Tests.Country", 'Date')
testing2$Tests.Country<-as.numeric(gsub(",", "", testing2$Tests.Country))
testing2$Tests.Country[testing2$Country.Region=='Philippines'& testing2$Date==as.Date("2020-03-31")]<-NA

#teting data from wikipedia:
testing3<-read.csv(paste0(wd, "external/Tests-Wiki.csv"))
testing3[, c("Country.Region", "Province.State")]<-sapply(testing3[, c("Country.Region", "Province.State")], coordName)
#write.csv(testing3, file=paste0(wd, "external/Tests-Wiki.csv"), row.names=F)

testing3$Date<-as.Date(testing3$Date, format="%m/%d/%y")
testing3<-testing3[!is.na(testing3$Date),]
testing3<-testing3[!duplicated(testing3[, c("Country.Region", "Province.State", 'Date')]),]
testing3$Tests<-as.numeric(gsub("[,]", "", testing3$Tests))
colnames(testing3)<-gsub("Tests", "Tests.Country", colnames(testing3))
testing3<-testing3[order(testing3$Date, decreasing = F),]
testing3<-testing3[!duplicated(testing3[, c("Country.Region", "Province.State", "Tests.Country")]), ]

#testing dataset... from ourworldindata.com
testing4<-read.csv(paste0(wd, "external/full-list-total-tests-for-covid-19.csv"))
colnames(testing4)<-c("Country.Region", "Code", "Date", "Tests.Country")
testing4$Province.State<-""
testing4$Date<-as.Date(testing4$Date, format="%b %d, %Y")
testing4$Country.Region<-coordName(testing4$Country.Region)
testing4$Code<-NULL

testing<-rbind.fill( testing, testing2)
testing<-rbind.fill(testing, testing3)
testing<-rbind.fill(testing, testing4)

testing<-testing[!duplicated(testing[, c("Country.Region","Province.State", "Date" )], fromLast = T),]
testing<-testing[order(testing$Date, decreasing = F),]

testing[testing$Country.Region=='Tunisia'& testing$Province.State=="",]

#mobility data:
apple.mobility<-read.csv(paste0(wd,"external/applemobilitytrends-2020-04-23.csv"))
apple.mobility<-melt(apple.mobility, id.vars=c("geo_type", "region", "transportation_type"), variable.name = "Date", value.name = "Mobility")
apple.mobility$Province.State<-ifelse(apple.mobility$region=="New York City", "New York", "")
apple.mobility$Country.Region<-ifelse(apple.mobility$region=="New York City", "US", apple.mobility$region)
apple.mobility$Country.Region<-coordName(apple.mobility$Country.Region)
apple.mobility<-apple.mobility[apple.mobility$geo_type=="country/region"| apple.mobility$Province.State=='New York', -c(1:2)]
apple.mobility<-reshape(apple.mobility,timevar="transportation_type",idvar=setdiff(colnames(apple.mobility), c("transportation_type", "Mobility")),direction="wide")
apple.mobility$Date<-as.Date(apple.mobility$Date, format="X%Y.%m.%d")
apple.mobility[is.na(apple.mobility)]<-100  #100 means no change..
apple.mobility$Province.State[apple.mobility$Country.Region=='Hong Kong']<-c( "Hong Kong")
apple.mobility$Country.Region[apple.mobility$Country.Region=='Hong Kong']<-c( "China")
apple.mobility$Province.State[apple.mobility$Country.Region=='Macau']<-c( "Macau")
apple.mobility$Country.Region[apple.mobility$Country.Region=='Macau']<-c( "China")



###MERGE/ORGANIZE DATA####

train$Date<-as.Date(train$Date)

# drop some inaccurate data
train<-train[!train$Province.State%in% c("Diamond Princess", "Grand Princess", "Recovered"),] 

#canada data is messed up..fill missing values
canada<-data.table(train)[train$Province.State!=''& train$Country.Region=='Canada', 
                          list(ConfirmedCases=sum(ConfirmedCases),Fatalities=sum(Fatalities)), by=c("Date", "Country.Region")]
train$ConfirmedCases[train$Country.Region=="Canada"& train$Province.State=='']<-canada$ConfirmedCases
train$Fatalities[train$Country.Region=="Canada"& train$Province.State=='']<-canada$Fatalities



train[, c("ConfirmedCases", "Fatalities", "Recoveries")][train[, c("ConfirmedCases", "Fatalities", "Recoveries")]<0]<-0
summary(train)

#cases and fatalities shouldn't decrease ever, so use cummax..
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, `:=`(ConfirmedCases=cummax(ConfirmedCases),
                                Fatalities=cummax(Fatalities), 
                                Recoveries=cummax(Recoveries)
), by=c("Country.Region", "Province.State")]
train<-data.frame(train)

# recoveries..fill missing
summary(lm(Recoveries~ConfirmedCases+Fatalities+0,data=train))
train$Recoveries[is.na(train$Recoveries)& !is.na(train$ConfirmedCases)]<-
  0.079*train$ConfirmedCases[is.na(train$Recoveries)& !is.na(train$ConfirmedCases)]+1.298036*train$Fatalities[is.na(train$Recoveries)& !is.na(train$ConfirmedCases)]
train$Recoveries<-ifelse(train$Recoveries<0, 0,
                         ifelse(train$Recoveries>(train$ConfirmedCases-train$Fatalities), train$ConfirmedCases-train$Fatalities,
                                train$Recoveries))

#add extra dates
test<-merge(unique(train[, c("Province.State", "Country.Region")]), data.frame(Date=seq(as.Date(max(train$Date)+1), as.Date("2020-05-04"), 1)  ))
train<-rbind.fill(train, test)
train$Country.Region<-coordName(train$Country.Region)
train$Province.State[train$Province.State==train$Country.Region]<-''


#Merge testing data.
testing$Province.State[testing$Province.State==testing$Country.Region]<-""
train<-merge(train, testing[, c("Country.Region", 'Province.State', "Tests.Country","Date")],by=c("Country.Region", 'Province.State', "Date"), all.x=T)


#loess impute for misssing testing data....
impute.loess<-function(y, Date, min.Date){
  #y<-state_testing$NegativeCases[state_testing$Province.State=='Michigan']
  #Date<-state_testing$Date[state_testing$Province.State=="Michigan"];
  # min.Date<-state_testing$min.Date[state_testing$Province.State=="Michigan"]
  
  # loess.fit<-tryCatch(splinefun(y=y, x=I(as.numeric(Date-min.Date)),method='hyman'), error=function(e) c())
  loess.fit<-tryCatch(approx(y=y, x=as.numeric(Date-min.Date), xout=as.numeric(Date-min.Date)), error=function(e) c()) #linear interpolation
  if(length(loess.fit)>0){
    first.date<-min(Date[!is.na(y)])
    last.date<-max(Date[!is.na(y)])
    
    # y[is.na(y)]<-loess.fit(as.numeric(Date-min.Date))[is.na(y)]
    y[is.na(y)]<-loess.fit$y[is.na(y)]
    y[Date<first.date]<-NA
    y[Date>last.date]<-NA
    
  }
  y
}
train<-data.table(train)[, `:=`(min.Date=min(Date)), by=c("Province.State", "Country.Region")]
train<-data.table(train)[, `:=`(Tests.Country=impute.loess(Tests.Country, Date, min.Date)), by=c("Province.State", "Country.Region")]
train<-data.frame(train)
train$Tests<-train$Tests.Country

##testing data..impute missing values
train$Percent.Positive<-train$ConfirmedCases/train$Tests
summary(train$Percent.Positive)

max.date<-max(train$Date[!is.na(train$ConfirmedCases)])

train$Tests[which(train$Percent.Positive>=.4)]<-train$ConfirmedCases[which(train$Percent.Positive>=.4)]/.4
train$Percent.Positive[which(train$Percent.Positive>=.4)]<-.4
train$Percent.Positive[(train$ConfirmedCases<=75& !is.na(train$Percent.Positive)& train$Date<=max.date)]<-.125
train$ConfirmedNegative<-train$Tests-train$ConfirmedCases


train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Percent.Positive.lag', 1:45) := shift(Percent.Positive, 1:45), by= c("Country.Region", "Province.State")]
train<-data.frame(train)

#use last available percent.positive rate to extrapolate missing tests
train$LastPercent.Positive<-NA
for(i in 1:30){
  train$LastPercent.Positive[is.na(train$LastPercent.Positive)]<-train[is.na(train$LastPercent.Positive), paste0("Percent.Positive.lag", i)]
}
train$Percent.Positive[train$Date<=max.date& is.na(train$Percent.Positive) ]<-
  train$LastPercent.Positive[train$Date<=max.date&  is.na(train$Percent.Positive) ]
train$Percent.Positive[is.na(train$Percent.Positive)& train$Date<=max.date]<-.125
train$Tests[train$Date<=max.date&  is.na(train$Tests)]<- train$ConfirmedCases[train$Date<=max.date& is.na(train$Tests)]/train$Percent.Positive[train$Date<=max.date& is.na(train$Tests)]
train$ConfirmedNegative<-train$Tests-train$ConfirmedCases

#what percent of confirmed cases are active (haven't recovered or died)
train$Percent.Active<-(train$ConfirmedCases-train$Fatalities-train$Recoveries)/train$ConfirmedCases
train$Percent.Active[train$ConfirmedCases==0]<-.8

#check here for errors...
train[which(train$Percent.Positive>=.4& train$Province.State==""), c("ConfirmedCases", "Country.Region", "Tests", "Date", "Percent.Positive", "Percent.Positive.lag1","LastPercent.Positive" , "ConfirmedNegative")]
# train[which(train$Country.Region=='Brazil'& train$Province.State==""), c("ConfirmedCases", "Country.Region", "Tests", "Date", "Percent.Positive", "Percent.Positive.lag1","LastPercent.Positive" , "ConfirmedNegative")]


#critical cases--from state_testing and worldometer
# critical<- worldometer[, c("Country.Region", "Province.State", "CriticalCases", "Date")]
# critical<-critical[!duplicated(critical[, c("Country.Region", 'Province.State', 'Date')]),]
# train<-merge(train, critical, by=c("Country.Region", "Province.State", "Date"), all.x=T)
# train$CriticalCases[is.na(train$CriticalCases)]<-.04*train$ConfirmedCases[is.na(train$CriticalCases)]
# train$CriticalCases[which(train$CriticalCases>train$ConfirmedCases)]<-train$ConfirmedCases[which(train$CriticalCases>train$ConfirmedCases)]
# hist(train$CriticalCases/train$ConfirmedCases)


#if there's anything missing here, add it to region_metadata.csv
train<-merge(train, region.info, by=c("Province.State", "Country.Region"), all.x=T)
unique(train[is.na(train$Population)| is.na(train$Lat) , c("Country.Region", "Province.State")])

country.info<-country.info[, !colnames(country.info)%in% c("tests", "testpop", 'schools', 'recovered', "quarantine","pop", "density" )]
country.info<-country.info[, !grepl("X[.]", colnames(country.info))& !colnames(country.info)=='X']
country.info[duplicated(country.info$country),]
train<-merge(train, country.info,  by.x=c("Country.Region", "Province.State"), by.y=c("country", "region"), all.x=T)


#restrictions dataset..
train<-merge(train, restrictions[, c("Country.Region", "Province.State","quarantine" )], by=c("Country.Region", "Province.State"), all.x=T)
train$Days.Since.quarantine<-as.numeric(train$Date-(train$quarantine))
train$Days.Since.quarantine[train$Days.Since.quarantine<0| is.na(train$Days.Since.quarantine)]<-0

world.in.data$Country<-coordName(world.in.data$Country)
world.in.data<-world.in.data[, !grepl("Diet",colnames(world.in.data))]
world.in.data<-world.in.data[!duplicated(world.in.data$Country),]
train<-merge(train, world.in.data, by.x="Country.Region", by.y="Country",all.x=T)
setdiff(train$Country.Region, world.bank$Country)


world.bank<-world.bank[!duplicated(world.bank$Country),]
train<-merge(train, world.bank, by.x="Country.Region", by.y="Country",all.x=T)
setdiff(train$Country.Region, world.bank$Country)


train<-merge(train, ages, by.x="Country.Region", by.y="Country", all.x=T)
train<-merge(train, ages2, by.x="Country.Region", by.y="Location", all.x=T)
setdiff(train$Country.Region, ages2$Location)



apple.mobility.1<-apple.mobility[apple.mobility$Province.State=='',]
train<-data.frame(train)
train<-merge(train[, !grepl("Mobility", colnames(train))], apple.mobility.1[, !colnames(apple.mobility.1)%in% c("Province.State")], by=c("Country.Region", 'Date'), all.x=T)
#some provinces available
for(i in c("New York", 'Hong Kong', "Macau")){
  apple.mobility.2<-apple.mobility[apple.mobility$Province.State==i,]
  train$Mobility.driving[train$Province.State==i]<- apple.mobility.2$Mobility.driving[match(train$Date[train$Province.State==i], apple.mobility.2$Date)]
  train$Mobility.walking[train$Province.State==i]<- apple.mobility.2$Mobility.walking[match(train$Date[train$Province.State==i], apple.mobility.2$Date)]
  train$Mobility.transit[train$Province.State==i]<- apple.mobility.2$Mobility.transit[match(train$Date[train$Province.State==i], apple.mobility.2$Date)]
}
# train[train$Province.State=='New York' , c("Date", 'Mobility.driving',)]
train<-train[order(train$Date, decreasing = F),]



###CREATE FEATURES#######

#apple mobility
train<-data.table(train)[, paste0('Mobility.walking.lag', 1:45) := shift(Mobility.walking, 1:45), by= c("Country.Region", "Province.State")]
train<-data.table(train)[, paste0('Mobility.transit.lag', 1:45) := shift(Mobility.transit, 1:45), by= c("Country.Region", "Province.State")]
#mean impute by day
cols<-colnames(train)[grepl("Mobility", colnames(train))]
train<-data.table(train)[, (cols):=lapply(.SD, function(x) {y<-x;y[is.na(y)]<-mean(x, na.rm=T);y  }), by=c("Date", "Continent"), .SDcols=cols]


train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, `:=`(
  min.Date=min(Date),
  Days=as.numeric(Date-min(Date)), 
  Days.Since.Case1=as.numeric(Date-min(Date[which(ConfirmedCases>=1)])),
  Days.Since.Case5=as.numeric(Date-min(Date[which(ConfirmedCases>=5)])),
  Days.Since.Case10=as.numeric(Date-min(Date[which(ConfirmedCases>=10)])),
  Days.Since.Case50=as.numeric(Date-min(Date[which(ConfirmedCases>=50)])),
  Days.Since.Case100=as.numeric(Date-min(Date[which(ConfirmedCases>=100)])),
  Days.Since.Case500=as.numeric(Date-min(Date[which(ConfirmedCases>=500)])),
  Days.Since.Case1000=as.numeric(Date-min(Date[which(ConfirmedCases>=1000)])),
  Days.Since.Case5000=as.numeric(Date-min(Date[which(ConfirmedCases>=5000)])),
  
  Days.Since.Fatality1=as.numeric(Date-min(Date[which(Fatalities>=1)])),
  Days.Since.Fatality5=as.numeric(Date-min(Date[which(Fatalities>=5)])),
  Days.Since.Fatality10=as.numeric(Date-min(Date[which(Fatalities>=10)])),
  Days.Since.Fatality50=as.numeric(Date-min(Date[which(Fatalities>=50)])),
  Days.Since.Fatality100=as.numeric(Date-min(Date[which(Fatalities>=100)])),
  Days.Since.Fatality500=as.numeric(Date-min(Date[which(Fatalities>=500)])),
  Days.Since.Fatality1000=as.numeric(Date-min(Date[which(Fatalities>=1000)])),
  Days.Since.Fatality5000=as.numeric(Date-min(Date[which(Fatalities>=5000)])),
  
  Days.Since.Recoveries10=as.numeric(Date-min(Date[which(Recoveries>=10)])),
  Days.Since.Recoveries100=as.numeric(Date-min(Date[which(Recoveries>=100)])),
  Days.Since.Recoveries1000=as.numeric(Date-min(Date[which(Recoveries>=1000)])),
  Days.Since.Recoveries5000=as.numeric(Date-min(Date[which(Recoveries>=5000)]))
), by=c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Cases.lag', 1:45) := shift(ConfirmedCases, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Fatalities.lag', 1:45) := shift(Fatalities, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Tests.lag', 1:45) := shift(Tests, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Recoveries.lag', 1:45) := shift(Recoveries, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('ConfirmedNegative.lag', 1:45) := shift(ConfirmedNegative, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Percent.Positive.lag', 1:45) := shift(Percent.Positive, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Percent.Active.lag', 1:45) := shift(Percent.Active, 1:45), by= c("Country.Region", "Province.State")]
# train<-train[order(train$Date, decreasing = F),]
# train<-data.table(train)[, paste0('Critical.lag', 1:45) := shift(CriticalCases, 1:45), by= c("Country.Region", "Province.State")]


#plot(train$Tests[train$Province.State=='New York']~train$Days[train$Province.State=='New York'])
train[train$Country.Region=='US'& train$Date<=Sys.Date(), c("Tests", "ConfirmedCases", 'Date', "Percent.Positive.lag1", "ConfirmedNegative")]


##country level stats###

#country stats: note: Canada has an extra row for it's country level 
country.stats<-data.table(train)[!(train$Country.Region%in% c("Canada", "US")& train$Province.State!=""),
                                 list(ConfirmedCases=sum(ConfirmedCases),
                                      Population=sum(Population, na.rm=T),
                                      Lat=median(Lat),
                                      Long=median(Long),
                                      Fatalities=sum(Fatalities)), by=c("Country.Region","Date")]
country.stats<-data.table(country.stats)[, `:=`(
  Days.Since.Case1=as.numeric(Date-min(Date[which(ConfirmedCases>=1)])),
  Days.Since.Case5=as.numeric(Date-min(Date[which(ConfirmedCases>=5)])),
  Days.Since.Case10=as.numeric(Date-min(Date[which(ConfirmedCases>=10)])),
  Days.Since.Case50=as.numeric(Date-min(Date[which(ConfirmedCases>=50)])),
  Days.Since.Case100=as.numeric(Date-min(Date[which(ConfirmedCases>=100)])),
  Days.Since.Case500=as.numeric(Date-min(Date[which(ConfirmedCases>=500)])),
  Days.Since.Case1000=as.numeric(Date-min(Date[which(ConfirmedCases>=1000)])),
  Days.Since.Case5000=as.numeric(Date-min(Date[which(ConfirmedCases>=5000)])),
  
  Days.Since.Fatality1=as.numeric(Date-min(Date[which(Fatalities>=1)])),
  Days.Since.Fatality5=as.numeric(Date-min(Date[which(Fatalities>=5)])),
  Days.Since.Fatality10=as.numeric(Date-min(Date[which(Fatalities>=10)])),
  Days.Since.Fatality50=as.numeric(Date-min(Date[which(Fatalities>=50)])),
  Days.Since.Fatality100=as.numeric(Date-min(Date[which(Fatalities>=100)])),
  Days.Since.Fatality500=as.numeric(Date-min(Date[which(Fatalities>=500)])),
  Days.Since.Fatality1000=as.numeric(Date-min(Date[which(Fatalities>=1000)])),
  Days.Since.Fatality5000=as.numeric(Date-min(Date[which(Fatalities>=5000)]))
), by=c("Country.Region")]
country.stats<-country.stats[order(country.stats$Date, decreasing = F),]
country.stats<-data.table(country.stats)[, paste0('Fatalities.lag', 1:45) := shift(Fatalities, 1:45), by= c("Country.Region")]
country.stats<-country.stats[order(country.stats$Date, decreasing = F),]
country.stats<-data.table(country.stats)[, paste0('Cases.lag', 1:45) := shift(ConfirmedCases, 1:45), by= c("Country.Region")]


colnames(country.stats)[!colnames(country.stats)%in% c("Country.Region", "Date")]<-paste0(colnames(country.stats)[!colnames(country.stats)%in% c("Country.Region", "Date")], ".Country")
country.stats<-data.frame(country.stats)
country.stats$Tests.Country<-NULL

train<-data.frame(train)
train<-merge(train, country.stats, by=c("Date", "Country.Region"), all.x=T)
train$Dist.From.Country.Center<-distHaversine(train[, c("Long", "Lat")], train[, c("Long.Country", "Lat.Country")])/1000
train$Population.Country[train$Province.State=='']<-train$Population[train$Province.State=='']

#hist(train$Dist.From.Country.Center)

#negative implies it happened in future
train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ][train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ]<0]<-0


##world and continent level stats###

#can filter by neighbor distance here or just leave it as whole world
world.stats<-data.table(train)[!(train$Country.Region%in% c("Canada", "US")& train$Province.State!=""),  list(ConfirmedCases.World=sum(ConfirmedCases, na.rm=T), 
                               Fatalities.World=sum(Fatalities, na.rm=T)),by="Date"]
train<-merge(train, world.stats, by=c("Date"))
cont.stats<-data.table(train)[!(train$Country.Region%in% c("Canada", "US")& train$Province.State!=""),list(ConfirmedCases.Continent=sum(ConfirmedCases, na.rm=T), 
                               Fatalities.Continent=sum(Fatalities, na.rm=T) ),by=c("Continent", "Date")]
train<-merge(train, cont.stats, by=c("Date", "Continent"))

train$ConfirmedCases.World<-train$ConfirmedCases.World-train$ConfirmedCases
train$Fatalities.World<-train$Fatalities.World-train$Fatalities
train$ConfirmedCases.Continent<-train$ConfirmedCases.Continent-train$ConfirmedCases
train$Fatalities.Continent<-train$Fatalities.Continent-train$Fatalities

train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Cases.World.lag', 1:45) := shift(ConfirmedCases.World, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Fatalities.World.lag', 1:45) := shift(Fatalities.World, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Cases.Continent.lag', 1:45) := shift(ConfirmedCases.Continent, 1:45), by= c("Country.Region", "Province.State")]
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[, paste0('Fatalities.Continent.lag', 1:45) := shift(Fatalities.Continent, 1:45), by= c("Country.Region", "Province.State")]
train<-data.frame(train)


##MISSING DATA IMPUTE:

#rate-stats: impute median for missing
cols<-c(
  #countryinfo-kaggle
  "avghumidity","lung", "malelung", 
  "sexratio", "sex65plus", "sex54","smokers", "urbanpop","hospibed","medianage",
  "Density","gini.of.household.income.reported.in.Gallup..by.wp5.year",
  
  #ourworldindata
  "Share.of.adults.that.are.overweight.2016", "Share.of.adults.who.are.obese.2016", 
  "GDP.per.capita.int..constant.2011.international.2017",
  "Estimated.prevalence.2012","Deaths.Smoking.Sex.Both.Age.Age.standardized.Rate.deaths.per.100.000.2017",
  "Deaths.Lower.respiratory.infections.Sex.Both.Age.Age.standardized.Rate.Rate.2017",
  "Smoking.prevalence.total.ages.15..of.adults.2016",
  "Urban.population.of.total.of.total.2017", 
  "X70.years.old.deaths.per.100.000.2017",
  "Share.of.men.smoking.2019", "Share.of.men.who.are.obese.2016",
  
  
  
  #age stats:
  "age1.14", "age15.65", "age66","age60.male","age60.fem",
  
  #world bank stats:
  "Human.development.index..HDI..2018","Life.expectancy.at.birth.2018","Social.support", "Mean.years.of.schooling.2018","Healthy.life.expectancy.at.birth",
  "Standard.deviation.of.ladder.by.country.year","Negative.affect"
  
  
)

for(i in cols){
  train[ , i][is.na(train[, i])]<-median(train[ , i], na.rm=T)
}

#population based-stats: use median-rate to impute missing
cols<-c("Air_Transport_Passengers_Carried", "Estimated.number.of.malaria.cases",
        "Smoking.deaths.2017", "Obesity.deaths.2017", "No.access.to.handwashing.facility.deaths.2017",
        'Tourism.Arrivals.2016',"Poor.sanitation.deaths.2017", "High.blood.pressure.deaths.2017")
for(i in cols){
  median.rate<-median(train[, i]/train$Population.Country, na.rm=T)
  train[is.na(train[, i]) , i]<-median.rate*train[is.na(train[, i]), "Population.Country"]
}


#more features:

train$LogConfirmed = log(train$ConfirmedCases+1)
train$LogConfirmedDelta = log(train$ConfirmedCases+1) - log(train$Cases.lag1+1)
train$ConfirmedDelta = train$ConfirmedCases - train$Cases.lag1
train$LogFatalities = log(train$Fatalities+1) 
train$LogFatalitiesDelta = log(train$Fatalities+1) - log(train$Fatalities.lag1+1)
train$FatalitiesDelta = train$Fatalities - train$Fatalities.lag1
train$LogRecoveries = log(train$Recoveries+1) 
train$LogRecoveriesDelta = log(train$Recoveries+1) - log(train$Recoveries.lag1+1)
train$RecoveriesDelta = train$Recoveries - train$Recoveries.lag1

# train$Percent.Confirmed<-train$ConfirmedCases/train$Population



###CASES: GEOMETRIC SERIES MODEL######

#Model future Cases as geometric series using cases number only. 
#Each country get's it's own rate of increase based on last 3 days of worldwide, continent, and country-level rate of increase
#decide parameters for deciding weights based on cv optimization
#based on this notebook except for deciding weights with CV optimization..https://www.kaggle.com/gaborfodor/covid-19-a-few-charts-and-a-simple-baseline/notebook


#CV to optimize params for decay, delta, etc..

getPred<-function(train.end){
  #train.end<-max(train$Date[!is.na(train$ConfirmedCases)])
  
  
  DECAY =  0.9; day.param<-3;count.param<-4
  test.end<-max(train$Date)
  
  #calculate state-level "DELTA"
  
  deltas<-train[train$Date>=as.Date("2020-03-12") & train$Date<=train.end  & !is.na(train$LogConfirmedDelta),]
  confirmed_deltas<-data.table(deltas)[, `:=`(
    avg.continent.3=mean(LogConfirmedDelta[LogConfirmed>2& Date>=train.end-day.param & !Country.Region=='China']),
    avg.continent=mean(LogConfirmedDelta[LogConfirmed>2 & !Country.Region=='China'], na.rm=T)
  ), by=c("Continent")]
  confirmed_deltas<-data.table(confirmed_deltas)[, `:=`(
    avg.global.3=mean(LogConfirmedDelta[LogConfirmed>2& Date>=train.end-day.param & !Country.Region=='China']),
    avg.global=mean(LogConfirmedDelta[LogConfirmed>2 & !Country.Region=='China'], na.rm=T)
  ), by=c("")]
  confirmed_deltas<-data.table(confirmed_deltas)[, list(avg=mean(LogConfirmedDelta[LogConfirmed>2], na.rm=T), 
                                                        avg.3=mean(LogConfirmedDelta[LogConfirmed>2 & Date>=train.end-day.param], na.rm=T), 
                                                        cnt=sum(!is.na(LogConfirmedDelta[LogConfirmed>2])), 
                                                        avg.global.3=avg.global.3[1],
                                                        avg.continent.3=avg.continent.3[1],
                                                        avg.global=avg.global[1],
                                                        avg.continent=avg.continent[1]
  ), by=c("Country.Region", "Province.State")]
  confirmed_deltas$avg[is.na(confirmed_deltas$avg)]<-confirmed_deltas$avg.continent[is.na(confirmed_deltas$avg)]
  confirmed_deltas$avg.3[is.na(confirmed_deltas$avg.3)]<-confirmed_deltas$avg.continent.3[is.na(confirmed_deltas$avg.3)]
  confirmed_deltas[order(avg.3, decreasing = T),]
  
  #delta is a weighted sum. decide weight from CV
  confirmed_deltas$DELTA = (confirmed_deltas$avg.global.3*0+3*confirmed_deltas$avg.continent.3+
                              (confirmed_deltas$avg.3*confirmed_deltas$cnt/count.param))/(3+confirmed_deltas$cnt/count.param)
  confirmed_deltas[order(confirmed_deltas$DELTA),]
  summary(confirmed_deltas)
  #hist(confirmed_deltas$DELTA)
  
  #calculate estimated cases
  
  daily_log_confirmed<-unique(train[, c("Province.State", "Country.Region", "Date", 'LogConfirmed')])
  daily_log_confirmed$Prediction<-daily_log_confirmed$Date>train.end
  
  for(i in 1:nrow(confirmed_deltas)){
    confirmed_delta<-confirmed_deltas$DELTA[i]
    
    bool<-daily_log_confirmed$Country.Region==confirmed_deltas$Country.Region[i]&
      daily_log_confirmed$Province.State==confirmed_deltas$Province.State[i]
    
    #geometric series...x2=x1+delta*decay^1. x3=x1+delta*decay^1+delta*deacay^2. sum of 1st n elements=(1-r^n)/(1-r)
    last.day<-daily_log_confirmed$LogConfirmed[daily_log_confirmed$Date==train.end & bool]
    daily_log_confirmed$n<-as.numeric(daily_log_confirmed$Date-train.end+1)
    daily_log_confirmed$LogConfirmed[daily_log_confirmed$Date>train.end& bool]<-last.day-confirmed_delta+
      confirmed_delta*(1-DECAY^daily_log_confirmed$n[daily_log_confirmed$Date>train.end& bool])/(1-DECAY)
    
  }
  daily_log_confirmed$estCases <- exp(daily_log_confirmed$LogConfirmed)-1
  
  # daily_log_confirmed<-merge(daily_log_confirmed, 
  #                            train[, c("Province.State", 'Country.Region', "Date" ,"ConfirmedCases")],
  #                            by=c("Province.State", 'Country.Region', "Date"), all.x = T)
  daily_log_confirmed<-merge(daily_log_confirmed, 
                             confirmed_deltas[, c("Country.Region", "Province.State", "DELTA"), with=F],
                             by=c("Country.Region", "Province.State"), all.x = T)
  daily_log_confirmed$train.end<-train.end
  daily_log_confirmed[daily_log_confirmed$Prediction & daily_log_confirmed$n<=41, ]
}
preds<-lapply(seq(as.Date("2020-03-15"),max(train$Date[!is.na(train$ConfirmedCases)]), 1), getPred )
preds<-rbindlist(preds)%>% data.frame()
preds$n<-preds$n-1 #

preds[, paste0("estCases.", 1:40)]<-NA
for(i in 1:40){
  preds[which(preds$n==i), paste0("estCases.",i)]<-preds$estCases[which(preds$n==i)]
}
preds[1,]
preds<-data.table(preds)[, lapply(.SD, mean, na.rm=T), 
                         by=c("Country.Region", 'Province.State', "Date"), .SDcols=paste0("estCases.", 1:40)]
preds[1,]
train<-data.frame(train)
train<-merge(train[, !grepl("estCases[.]", colnames(train))], preds, by.x=c("Country.Region", "Province.State", "Date"), 
             by.y=c("Country.Region", 'Province.State', "Date"), all.x=T)



