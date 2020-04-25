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

#run 1-organize-data.R first


######FATALITIES: GEOMETRIC SERIES MODEL######
getPred<-function(train.end){
  #train.end<-max(train$Date[!is.na(train$ConfirmedCases)])
  
  DECAY =  0.85; day.param<-3;count.param<-1.5
  test.end<-max(train$Date)
  
  deltas<-train[train$Date>=as.Date("2020-03-12") & train$Date<=train.end & !is.na(train$LogFatalitiesDelta),]
  death_deltas<-data.table(deltas)[, `:=`(
    avg.continent.3=mean(LogFatalitiesDelta[LogFatalities>1& Date>=train.end-day.param&!Country.Region=='China']),
    avg.continent=mean(LogFatalitiesDelta[LogFatalities>1&!Country.Region=='China'], na.rm=T)
  ), by=c("Continent")]
  death_deltas<-data.table(death_deltas)[, `:=`(
    avg.global.3=mean(LogFatalitiesDelta[LogFatalities>1& Date>=train.end-day.param&!Country.Region=='China']),
    avg.global=mean(LogFatalitiesDelta[LogFatalities>1&!Country.Region=='China'], na.rm=T)
  ), by=c("")]
  death_deltas<-data.table(death_deltas)[, list(avg=mean(LogFatalitiesDelta[LogFatalities>1], na.rm=T), 
                                                avg.3=mean(LogFatalitiesDelta[LogFatalities>1 & Date>=train.end-day.param], na.rm=T), 
                                                cnt=sum(!is.na(LogFatalitiesDelta[LogFatalities>1])), 
                                                avg.global.3=avg.global.3[1],
                                                avg.continent.3=avg.continent.3[1],
                                                avg.global=avg.global[1],
                                                avg.continent=avg.continent[1]
  ), by=c("Country.Region", "Province.State")]
  death_deltas$avg[is.na(death_deltas$avg)]<-death_deltas$avg.continent[is.na(death_deltas$avg)]
  death_deltas$avg.3[is.na(death_deltas$avg.3)]<-death_deltas$avg.continent.3[is.na(death_deltas$avg.3)]
  death_deltas[order(avg.3, decreasing = T),]
  
  death_deltas$DELTA = (death_deltas$avg.global.3*0+3*death_deltas$avg.continent.3+
                          (death_deltas$avg.3*death_deltas$cnt/count.param))/(3+death_deltas$cnt/count.param)
  death_deltas[order(death_deltas$DELTA),]
  summary(death_deltas)
  #hist(death_deltas$DELTA)
  
  
  daily_log_deaths<-unique(train[, c("Province.State", "Country.Region", "Date", 'LogFatalities')])
  daily_log_deaths$Prediction<-daily_log_deaths$Date>train.end
  
  for(i in 1:nrow(death_deltas)){
    death_delta<-death_deltas$DELTA[i]
    bool<-daily_log_deaths$Country.Region==death_deltas$Country.Region[i]&
      daily_log_deaths$Province.State==death_deltas$Province.State[i]
    
    #geometric series...x2=x1+delta*decay^1. x3=x1+delta*decay^1+delta*deacay^2
    
    last.day<-daily_log_deaths$LogFatalities[daily_log_deaths$Date==train.end & bool ]
    
    daily_log_deaths$n<-as.numeric(daily_log_deaths$Date-train.end+1)
    daily_log_deaths$LogFatalities[daily_log_deaths$Date>train.end& bool]<- last.day-death_delta+
      death_delta*(1-DECAY^daily_log_deaths$n[daily_log_deaths$Date>train.end& bool])/(1-DECAY)
    
  }
  daily_log_deaths$estFatalities <- exp(daily_log_deaths$LogFatalities)-1
  
  # daily_log_deaths<-merge(daily_log_deaths, 
  #                         train[, c("Province.State", 'Country.Region', "Date" ,"Fatalities")],
  #                         by=c("Province.State", 'Country.Region', "Date"), all.x = T)
  daily_log_deaths<-merge(daily_log_deaths, 
                          death_deltas[, c("Country.Region", "Province.State", "DELTA"), with=F],
                          by=c("Country.Region", "Province.State"), all.x = T)
  
  daily_log_deaths$train.end<-train.end
  daily_log_deaths[daily_log_deaths$Prediction & daily_log_deaths$n<=41, ]
}
preds<-lapply(seq(as.Date("2020-03-15"),max(train$Date[!is.na(train$ConfirmedCases)]), 1), getPred )
preds<-rbindlist(preds)%>% data.frame()
preds$n<-preds$n-1 #

preds[, paste0("estFatalities.", 1:40)]<-NA
for(i in 1:40){
  preds[which(preds$n==i), paste0("estFatalities.",i)]<-preds$estFatalities[which(preds$n==i)]
}
preds[1,]
preds<-data.table(preds)[, lapply(.SD, mean, na.rm=T), 
                         by=c("Country.Region", 'Province.State', "Date"), .SDcols=paste0("estFatalities.", 1:40)]
unique(preds$train.end)
train<-merge(train[, !grepl("estFatalities[.]", colnames(train))], preds,
             by.x=c("Country.Region", "Province.State", "Date"), 
             by.y=c("Country.Region", 'Province.State', "Date"), all.x=T)



######RECOVERIES: GEOMETRIC SERIES MODEL######
getPred<-function(train.end){
  #train.end<-max(train$Date[!is.na(train$ConfirmedCases)])
  
  DECAY =  0.88; day.param<-3;count.param<-15
  test.end<-max(train$Date)
  
  deltas<-train[train$Date>=as.Date("2020-03-12") & train$Date<=train.end & !is.na(train$LogRecoveriesDelta),]
  recover_deltas<-data.table(deltas)[, `:=`(
    avg.continent.3=mean(LogRecoveriesDelta[LogRecoveries>2& Date>=train.end-day.param&!Country.Region=='China']),
    avg.continent=mean(LogRecoveriesDelta[LogRecoveries>2&!Country.Region=='China'], na.rm=T)
  ), by=c("Continent")]
  recover_deltas<-data.table(recover_deltas)[, `:=`(
    avg.country.3=mean(LogRecoveriesDelta[LogRecoveries>2& Date>=train.end-day.param&!Country.Region=='China']),
    cnt.country=sum(!is.na(LogRecoveriesDelta[LogRecoveries>2])), 
    avg.country=mean(LogRecoveriesDelta[LogRecoveries>2&!Country.Region=='China'], na.rm=T)
  ), by=c("")]
  recover_deltas<-data.table(recover_deltas)[, list(avg=mean(LogRecoveriesDelta[LogRecoveries>2], na.rm=T), 
                                                    avg.3=mean(LogRecoveriesDelta[LogRecoveries>2 & Date>=train.end-day.param], na.rm=T), 
                                                    cnt=sum(!is.na(LogRecoveriesDelta[LogRecoveries>2])), 
                                                    cnt.country=cnt.country[1],
                                                    avg.country.3=avg.country.3[1],
                                                    avg.continent.3=avg.continent.3[1],
                                                    avg.country=avg.country[1],
                                                    avg.continent=avg.continent[1]
  ), by=c("Country.Region", "Province.State")]
  recover_deltas$avg[is.na(recover_deltas$avg)]<-recover_deltas$avg.continent[is.na(recover_deltas$avg)]
  recover_deltas$avg.3[is.na(recover_deltas$avg.3)]<-recover_deltas$avg.continent.3[is.na(recover_deltas$avg.3)]
  recover_deltas$avg.country.3[is.na(recover_deltas$avg.country.3)]<-recover_deltas$avg.continent.3[is.na(recover_deltas$avg.country.3)]
  recover_deltas[order(avg.3, decreasing = T),]
  
  recover_deltas$DELTA = (recover_deltas$avg.country.3*0+recover_deltas$avg.continent.3*3+
                            (recover_deltas$avg.3*recover_deltas$cnt/count.param))/(3+recover_deltas$cnt/count.param)
  recover_deltas[order(recover_deltas$DELTA),]
  summary(recover_deltas)
  hist(recover_deltas$DELTA)
  
  
  daily_log_recovers<-unique(train[, c("Province.State", "Country.Region", "Date", 'LogRecoveries')])
  daily_log_recovers$Prediction<-daily_log_recovers$Date>train.end
  
  for(i in 1:nrow(recover_deltas)){
    recover_delta<-recover_deltas$DELTA[i]
    bool<-daily_log_recovers$Country.Region==recover_deltas$Country.Region[i]&
      daily_log_recovers$Province.State==recover_deltas$Province.State[i]
    
    #geometric series...x2=x1+delta*decay^1. x3=x1+delta*decay^1+delta*deacay^2
    
    last.day<-daily_log_recovers$LogRecoveries[daily_log_recovers$Date==train.end & bool ]
    
    daily_log_recovers$n<-as.numeric(daily_log_recovers$Date-train.end+1)
    daily_log_recovers$LogRecoveries[daily_log_recovers$Date>train.end& bool]<- last.day-recover_delta+
      recover_delta*(1-DECAY^daily_log_recovers$n[daily_log_recovers$Date>train.end& bool])/(1-DECAY)
    
  }
  daily_log_recovers$estRecoveries <- exp(daily_log_recovers$LogRecoveries)-1
  
  daily_log_recovers<-merge(daily_log_recovers, 
                            recover_deltas[, c("Country.Region", "Province.State", "DELTA"), with=F],
                            by=c("Country.Region", "Province.State"), all.x = T)
  
  daily_log_recovers$train.end<-train.end
  daily_log_recovers[daily_log_recovers$Prediction & daily_log_recovers$n<=41, ]
}
preds<-lapply(seq(as.Date("2020-03-15"),max(train$Date[!is.na(train$ConfirmedCases)]), 1), getPred )
preds<-rbindlist(preds)%>% data.frame()
preds$n<-preds$n-1 #

preds[, paste0("estRecoveries.", 1:40)]<-NA
for(i in 1:40){
  preds[which(preds$n==i), paste0("estRecoveries.",i)]<-preds$estRecoveries[which(preds$n==i)]
}
preds<-data.table(preds)[, lapply(.SD, mean, na.rm=T), 
                         by=c("Country.Region", 'Province.State', "Date"), .SDcols=paste0("estRecoveries.", 1:40)]
train<-merge(train[, !grepl("estRecoveries[.]", colnames(train))], preds,
             by.x=c("Country.Region", "Province.State", "Date"), 
             by.y=c("Country.Region", 'Province.State', "Date"), all.x=T)

###ARIMA MODEL#####



max.train.date<-as.Date("2020-04-06")
#max.train.date<-max(train$Date[!is.na(train$ConfirmedCases)])
max.cutoff<-20




#Fit an arima model for each country/state combo
countries<-unique(train[train$Date<=max.train.date ,c("Country.Region", "Province.State")])
train[, paste0("arimaCases.",1:max.cutoff)]<-NA
train[, paste0("arimaFatalities.",1:max.cutoff)]<-NA
train[, paste0("arimaRecoveries.",1:max.cutoff)]<-NA

train<-train[order(train$Province.State, train$Date, decreasing = F),]

#max.train.date is cutoff date for fitting the arima model---run for all dates to get backtested stats--takes a while though

#for(max.train.date in seq(as.Date("2020-03-30"), max(train$Date[!is.na(train$ConfirmedCases)]), 1)){
for(max.train.date in max.train.date){
  max.train.date<-as.Date( max.train.date, origin='1970-01-01')
  print(max.train.date)
  # max.train.date<-max(train$Date[!is.na(train$ConfirmedCases)])
  dates <-seq(as.Date(max.train.date)+1,as.Date(max(train$Date)),1)
  days<-length(dates) #number of dates to forecast
  days<-min(days, max.cutoff)
  
  for(i in 1:nrow(countries)){
    df <- train[train$Country.Region==countries$Country.Region[i]& train$Province.State==countries$Province.State[i]& train$Date<=max.train.date,]
    #tail(df[, 1:10], 10)
    fit_arima = auto.arima(y=df$ConfirmedCases ) #stepwise = F, seasonal = F, method="ML" )
    forecasts<-as.vector(forecast(fit_arima, h=days,level = 80)$mean)
    forecasts<-pmax(forecasts, max(df$ConfirmedCases))
    forecasts<-cummax(forecasts)
    
    fit_arima_fatal = auto.arima(y=df$Fatalities )  
    forecasts_fatal<-as.vector(forecast(fit_arima_fatal, h=days,level = 80)$mean)
    forecasts_fatal<-pmax(forecasts_fatal, max(df$Fatalities))
    forecasts_fatal<-cummax(forecasts_fatal)
    
    fit_arima_recover = auto.arima(y=df$Recoveries )  
    forecasts_recover<-as.vector(forecast(fit_arima_recover, h=days,level = 80)$mean)
    forecasts_recover<-pmax(forecasts_recover, max(df$Recoveries))
    forecasts_recover<-cummax(forecasts_recover)
    
    for(j in 1:days){
      bool<-(train$Date==max.train.date+j)& train$Country.Region==df$Country.Region[1]& train$Province.State==df$Province.State[1]
      train[bool, paste0("arimaCases.",j)]<-forecasts[j]
      train[bool, paste0("arimaFatalities.",j)]<-forecasts_fatal[j]
      train[bool, paste0("arimaRecoveries.",j)]<-forecasts_recover[j]
    }
  }
}


#save(train, file="train.Rda")
#load("train.Rda")
####LIGHTGBM MODEL########

observation.min<-1
eps<-.5


for(cutoff in 1:max.cutoff){
  print(cutoff)
  
  #cutoff<-7
  
  train[, paste0("cases.var", 1:7)]<-train[, paste0("Cases.lag",seq(cutoff, cutoff+6, 1))] 
  train[, paste0("fatalities.var", 1:7)]<-train[, paste0("Fatalities.lag",seq(cutoff, cutoff+6,1))]
  train[, paste0("negative.var", c(1, 4, 7))]<-train[, paste0("ConfirmedNegative.lag",seq(cutoff, cutoff+6, 3))]  #1, 4, 7
  train[, paste0("tests.var", c(1, 4, 7))]<-train[, paste0("Tests.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("cases.world.var", c(1, 4, 7))]<-train[, paste0("Cases.World.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("recoveries.var",c(1, 4, 7))]<-train[, paste0("Recoveries.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.world.var", c(1, 4, 7))]<-train[, paste0("Fatalities.World.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("percent.var", c(1, 4, 7))]<-train[, paste0("Percent.Positive.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("percent.active.var", c(1, 4, 7))]<-train[, paste0("Percent.Active.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("cases.continent.var", c(1, 4, 7))]<-train[, paste0("Cases.Continent.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.continent.var", c(1, 4, 7))]<-train[, paste0("Fatalities.Continent.lag",seq(cutoff, cutoff+6,3))]
  
  train[, "mobility.walking.var.mean"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff, cutoff+2, 1))], na.rm=T)
  train[, "mobility.walking.var.mean2"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+6, cutoff+8, 1))], na.rm=T)
  
  
  train$log.Cases<-log(eps+train$ConfirmedCases)
  train$log.Fatalities<-log(eps+train$Fatalities)
  train$log.Recoveries<-log(eps+train$Recoveries)
  
  
  train$abs.Lat<- abs(train$Last)
  train$Poor.sanitation.deaths.2017.scaled<-train$Poor.sanitation.deaths.2017*train$Population/train$Population.Country
  train$log.cases.var1.negative.affect<-log(eps+train$Negative.affect*train$cases.var1)
  train$log.negative.var1<-log(eps+train$negative.var1)/train$Population
  train$log.tests.var1<-log(eps+train$tests.var1)
  train$log.cases.diff<-log(eps+train$cases.var1-train$cases.var2)
  train$log.cases.diff2<-log(eps+train$cases.var1-train$cases.var3)
  train$log.active.var1<-log(eps+train$cases.var1-train$fatalities.var1-train$recoveries.var1)
  train$log.cases.world.var7<- log(eps+train$cases.world.var7)
  train$log.cases.world.var1<- log(eps+train$cases.world.var1)
  train$log.cases.var7<- log(eps+train$cases.var7)
  train$log.cases.var4<- log(eps+train$cases.var4)
  train$log.cases.var1<- log(eps+train$cases.var1)
  train$log.fatalities.var7<- log(eps+train$fatalities.var7)
  train$log.fatalities.var4<- log(eps+train$fatalities.var4)
  train$log.fatalities.var1<- log(eps+train$fatalities.var1)
  train$log.recoveries.var7<- log(eps+train$recoveries.var7)
  train$log.recoveries.var4<- log(eps+train$recoveries.var4)
  train$log.recoveries.var1<- log(eps+train$recoveries.var1)
  train$log.fatalities.continent.var4<-log(eps+train$fatalities.continent.var4+train$fatalities.var4)
  train$log.fatalities.continent.var1<-log(eps+train$fatalities.continent.var1+train$fatalities.var1)
  train$log.quarantine.days<-log(eps+train$Days.Since.quarantine)
  train$log.days<-log(eps+train$Days)
  
  
  all.vars<-c(
    "abs.Lat", "Poor.sanitation.deaths.2017.scaled","log.cases.var1.negative.affect","log.negative.var1",
    "log.tests.var1","log.cases.diff","log.cases.diff2","log.active.var1","log.cases.world.var7","log.cases.world.var1",
    "log.cases.var7", "log.cases.var4", "log.cases.var1","log.fatalities.continent.var4","log.fatalities.continent.var1",
    "log.quarantine.days", "log.days",
    
    
    "mobility.walking.var.mean2","mobility.walking.var.mean",
    "age66","High.blood.pressure.deaths.2017",
    "medianage", "Population", "Population.Country","Negative.affect",
    "percent.var1", "percent.active.var1", "lung", "urbanpop"
  )
  
  train$log.cases.var4.obese<-log(eps+train$cases.var4*train$Obesity.deaths.2017/(10^6)*(train$Population/train$Population.Country))
  train$log.cases.var1.obese<-log(eps+train$cases.var1*train$Obesity.deaths.2017/(10^6)*(train$Population/train$Population.Country))
  train$log.cases.var1.obese2<-log(eps+train$cases.var1*train$Share.of.adults.who.are.obese.2016/100)
  train$log.cases.var7.age60.male<- log(eps+train$cases.var7*train$age60.male/100)
  train$log.cases.var4.age66<- log(eps+train$cases.var4*train$age66/100)
  train$log.cases.var4.age70<- log(eps+train$cases.var4*train$X70.years.old.deaths.per.100.000.2017/(10^4))
  
  train$log.fatalities.var1.negative.affect<- log(eps+train$fatalities.var1*train$Negative.affect)
  train$log.fatalities.var1.age66<- log(eps+train$fatalities.var1*train$age66/100)
  train$Population.age66<-train$Population*train$age66/100
  train$Population.age15<-train$Population*train$age15.65/100
  train$log.active.cases.age66<-log(eps+(train$cases.var1-train$recoveries.var1-train$fatalities.var1)*train$age66/100)
  
  setdiff(all.vars, colnames(train))
  
  ##Cases GBM##
  params = list(objective = "regression",
                metric = "rmse",
                num_leaves = 30,
                learning_rate = 0.013,
                lambda = 0.5,
                alpha=.5,
                num_threads=4,
                feature_fraction=0.8,
                bagging_fraction = 0.8
  )
  
  #create lgb dataset and fit model
  bool<-!is.na(train$percent.var7)& !is.na(train[, "Fatalities"])& train$cases.var1>=observation.min  & train$Date<=max.train.date
  y<-train$log.Cases[bool]
  train.lgb<- data.matrix(train[bool, all.vars])
  train.lgb <- lgb.Dataset(train.lgb, label =y )
  
  fit <- lgb.train(params, train.lgb, num_boost_round = 4000,  verbose=0,early_stopping_rounds = 200, valids = list(train.lgb=train.lgb))
  # fit <- lgb.cv(params, train.lgb, num_boost_round = 4000,  verbose=0, nfold = 3, early_stopping_rounds = 200)
  train[, paste0("lgbCases.", cutoff)]<- exp(predict(fit, data.matrix(train[,all.vars])))-eps
  train[,  paste0("lgbCases.", cutoff)]<-pmax(train[,  paste0("lgbCases.", cutoff)], train$cases.var1)
  
  # Importance
  #imp <- lgb.importance(fit);lgb.plot.importance(imp, top_n = Inf)
  # summary(train[, paste0("lgbCases.", cutoff)])
  # summary(train$cases.var1)
  
  ##Fatalities GBM##
  
  train$cases.var30<-rowMeans(train[, c(paste0("estCases.", cutoff), paste0("estCases.", cutoff), paste0("estCases.", cutoff), paste0("lgbCases.", cutoff))],na.rm=T)
  
  all.vars2<-c(
    "cases.var30",
    
    "log.cases.var4.obese","log.cases.var1.obese",
    "log.cases.var1.obese2","log.cases.var7.age60.male","log.cases.var4.age66","log.cases.var4.age70",
    "log.fatalities.var1.negative.affect","log.fatalities.var1.age66","Population.age66","Population.age15", "log.active.cases.age66",
    
    "Share.of.adults.who.are.obese.2016", "Obesity.deaths.2017","X70.years.old.deaths.per.100.000.2017",
    "Population", "Population.Country","percent.var1", "percent.active.var1",
    
    "age66", "age60.male","age15.65","log.fatalities.var1","log.fatalities.var4", "log.fatalities.var7",
    "log.recoveries.var1", "log.recoveries.var4", "log.recoveries.var7",
    "log.cases.var1","log.cases.var4", "log.cases.var7"
  )  
  
  
  params = list(objective = "regression",
                metric = "rmse",
                num_leaves = 50,
                learning_rate = 0.013,
                alpha=1,
                lambda = 4,
                num_threads=4,
                feature_fraction=0.8,
                bagging_fraction = 0.8
  )
  
  
  bool<-!is.na(train$percent.var7)& !is.na(train[, "Fatalities"])& train$cases.var1>=observation.min  & train$Date<=max.train.date
  y<-train$log.Fatalities[bool]
  train.lgb<- data.matrix(train[bool, all.vars2])
  train.lgb <- lgb.Dataset(train.lgb, label =y )
  
  fit <- lgb.train(params, train.lgb, num_boost_round = 4000,  verbose=0,early_stopping_rounds = 200, valids = list(train.lgb=train.lgb))
  train[, paste0("lgbFatalities.", cutoff)]<- exp(predict(fit, data.matrix(train[,all.vars2])))-eps
  train[,  paste0("lgbFatalities.", cutoff)]<-pmax(train[,  paste0("lgbFatalities.", cutoff)], train$fatalities.var1)
  rmsle(train$Fatalities[bool], train[bool, paste0("lgbFatalities.", cutoff)])
  
  
  # Importance
  #imp <- lgb.importance(fit);lgb.plot.importance(imp, top_n = Inf)
  
  
  ##Recoveries GBM##
  
  params = list(objective = "regression",
                metric = "rmse",
                num_leaves = 30,
                learning_rate = 0.013,
                lambda = 1,
                alpha=1,
                num_threads=4,
                feature_fraction=0.8,
                bagging_fraction = 0.8
  )
  
  train$log.recoveries.var1.obese<-log(eps+train$recoveries.var1*train$Obesity.deaths.2017/(10^6)*(train$Population/train$Population.Country))
  train$log.recoveries.var1.obese2<-log(eps+train$recoveries.var1*train$Share.of.adults.who.are.obese.2016/100)
  train$log.recoveries.var1.age66<- log(eps+train$recoveries.var1*train$age66/100)
  train$log.recoveries.var1.negative.affect<- log(eps+train$recoveries.var1*train$Negative.affect)
  train$log.active.cases.var7<-log(eps+(train$cases.var7-train$fatalities.var7))
  train$log.active.cases.var4<-log(eps+(train$cases.var4-train$fatalities.var4))
  train$log.active.cases.var1<-log(eps+(train$cases.var1-train$fatalities.var1))
  
  all.vars3<-c("log.recoveries.var1.obese","log.recoveries.var1.obese2","log.recoveries.var1.age66","log.recoveries.var1.negative.affect",
               "log.active.cases.var7","log.active.cases.var4","log.active.cases.var1", "log.cases.world.var7", "log.cases.world.var1",
               all.vars2)
  all.vars3<-unique(all.vars3)
  
  bool<-!is.na(train$percent.var7)& !is.na(train[, "Fatalities"])& train$cases.var1>=observation.min  & train$Date<=max.train.date
  y<-train$log.Recoveries[bool]
  train.lgb<- data.matrix(train[bool, all.vars3])
  train.lgb <- lgb.Dataset(train.lgb, label =y )
  
  fit <- lgb.train(params, train.lgb, num_boost_round = 4000,  verbose=0,early_stopping_rounds = 200, valids = list(train.lgb=train.lgb))
  train[, paste0("lgbRecoveries.", cutoff)]<- exp(predict(fit, data.matrix(train[,all.vars3])))-eps
  train[,  paste0("lgbRecoveries.", cutoff)]<-pmax(train[,  paste0("lgbRecoveries.", cutoff)], train$recoveries.var1)
  
  #imp <- lgb.importance(fit);lgb.plot.importance(imp, top_n = Inf)
  gc(verbose = FALSE)
}





### CASES- LINEAR MODEL####
# load("train.Rda")
#INDIVIDUAL LINEAR MODELS FOR PROJECTING 1, 2, 3, 4, etc DAYS IN THE FUTURE
#based on lagged variables like lagged cases, fatalities variables and demogaphic variables like population, air transportation

observation.min<-1
eps<-.5

###LINEAR MODELS#####

#max.train.date<-max(train$Date[!is.na(train$ConfirmedCases)])
for(cutoff in 1:max.cutoff){
  #print(cutoff)
  #cutoff<-7
  
  
  #if it happened in last 5 days and I am using 7 day model need to impute it as 0
  train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ][train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ]<cutoff]<-0
  
  train[, paste0("cases.var", 1:7)]<-train[, paste0("Cases.lag",seq(cutoff, cutoff+6, 1))] 
  train[, paste0("fatalities.var", 1:7)]<-train[, paste0("Fatalities.lag",seq(cutoff, cutoff+6,1))]
  train[, paste0("negative.var", c(1, 4, 7))]<-train[, paste0("ConfirmedNegative.lag",seq(cutoff, cutoff+6, 3))]  #1, 4, 7
  train[, paste0("tests.var", c(1, 4, 7))]<-train[, paste0("Tests.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("cases.world.var", c(1, 4, 7))]<-train[, paste0("Cases.World.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("recoveries.var",c(1, 4, 7))]<-train[, paste0("Recoveries.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.world.var", c(1, 4, 7))]<-train[, paste0("Fatalities.World.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("percent.var", c(1, 4, 7))]<-train[, paste0("Percent.Positive.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("percent.active.var", c(1, 4, 7))]<-train[, paste0("Percent.Active.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("cases.continent.var", c(1, 4, 7))]<-train[, paste0("Cases.Continent.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.continent.var", c(1, 4, 7))]<-train[, paste0("Fatalities.Continent.lag",seq(cutoff, cutoff+6,3))]
  
  #how much were people walking recently
  train[, "mobility.walking.var.mean"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff, cutoff+2, 1))], na.rm=T)
  train[, "mobility.walking.var.mean2"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+6, cutoff+8, 1))], na.rm=T)
  train[, "mobility.walking.var.mean3"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+12, cutoff+14, 1))], na.rm=T)
  
  fit<-lm(log(eps+ConfirmedCases)~
            abs(Lat)+
            #I(Air_Transport_Passengers_Carried*Population/Population.Country)+
            
            I(Negative.affect*log(eps+cases.var1))+
            Population+
            Population.Country+
            I(Population/Population.Country)+
            I(medianage^2)+
            #Dist.From.Country.Center+
            urbanpop+
            
            #Deaths.Lower.respiratory.infections.Sex.Both.Age.Age.standardized.Rate.Rate.2017+
            lung+
            I(Poor.sanitation.deaths.2017*Population/Population.Country)+
            High.blood.pressure.deaths.2017+
            
            #tests stats
            I(log(eps+negative.var1)/Population)+
            log(eps+tests.var1)+
            percent.var1+
            #I(var10/Population*age66/100)+
            
            #cases stats
            percent.active.var1+
            log(eps+(cases.var1-cases.var2))+
            log(eps+(cases.var1-cases.var3))+
            
            log(eps+cases.var1-fatalities.var1-recoveries.var1)+
            log(eps+cases.world.var7)+
            log(eps+cases.world.var1)+
            log(eps+cases.var7)+
            log(eps+cases.var4)+
            log(eps+cases.var1)+
            log(eps+fatalities.continent.var4+fatalities.var4)+
            log(eps+fatalities.continent.var1+fatalities.var1)+
            log(eps+cases.var1*age66/100)+
            mobility.walking.var.mean+
            mobility.walking.var.mean2+
            # mobility.walking.var.mean3+
            log(eps+Days.Since.quarantine)+
            log(eps+Days), 
          # method="glmnet",
          # trControl=trainControl( method = "none") ,
          # tuneGrid=expand.grid(alpha=0, lambda=c(.0001)),
          data=train[!is.na(train$percent.var7)& !is.na(train[, "Fatalities"])& train$cases.var1>=observation.min  & train$Date<=max.train.date, ]);summary(fit)
  
  y.var<-paste0("predCases.", cutoff)
  train[, y.var]<-NA
  
  train[,y.var]<-exp(predict(fit, newdata=train))-1
  train[which(train[, y.var]<0), y.var]<-0
  train[which(train[, y.var]<train[, "cases.var1"]), y.var]<-train[which(train[, y.var]<train[, "cases.var1"]), "cases.var1"]  #number of cases can't go down
  train[which(train[, y.var]>train$Population*.2667), y.var]<-train$Population[which(train[, y.var]>train$Population*.2667)]*.2667
  
}

#predictions sometimes get weird for 10+ days out, so impute large vals
train$predCases.7.2<-ifelse(train$predCases.7>=2*train$estCases.7, train$estCases.7*2, train$predCases.7)
bool<-!is.na(train$predCases.7)& !is.na(train$estCases.7)  & !is.na(train$ConfirmedCases) & train$Date>as.Date("2020-04-06")
rmsle( train$ConfirmedCases[bool], train$predCases.7[bool])
rmsle( train$ConfirmedCases[bool], train$lgbCases.7[bool])
rmsle( train$ConfirmedCases[bool], train$estCases.7[bool])
rmsle( train$ConfirmedCases[bool], train$arimaCases.7[bool])
rmsle( train$ConfirmedCases[bool], train$predCases.7.2[bool])
rmsle( train$ConfirmedCases[bool],  rowMeans(train[bool, c("lgbCases.7", 'arimaCases.7', 'arimaCases.7',"estCases.7" , "estCases.7")]))
# rmsle( train$ConfirmedCases[bool],  rowMeans(train[bool, c("lgbCases.7", "estCases.7",  "estCases.7", "estCases.7")]))
sum(bool)

# 
for(i in 7:max.cutoff){
  train[which(train[,paste0("predCases.", i)]>train[,  paste0("estCases.", i)]*2), paste0("predCases.", i)]<-
    train[which(train[,paste0("predCases.", i)]>train[,  paste0("estCases.", i)]*2), paste0("estCases.", i)]*2
}
# 




### FATALITIES- LINEAR MODEL####

for(cutoff in 1:max.cutoff){
  #print(cutoff)
  #cutoff<-7
  
  
  train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ][train[, grepl("Days", colnames(train))& !colnames(train)=="Days" ]<cutoff]<-0
  
  
  train[, paste0("cases.var", 1:7)]<-train[, paste0("Cases.lag",seq(cutoff, cutoff+6, 1))] #1, 4, 7
  train[, paste0("fatalities.var", 1:7)]<-train[, paste0("Fatalities.lag",seq(cutoff, cutoff+6, 1))]
  train[, paste0("negative.var", c(1, 4, 7))]<-train[, paste0("ConfirmedNegative.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("tests.var", c(1, 4, 7))]<-train[, paste0("Tests.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("cases.world.var", c(1, 4, 7))]<-train[, paste0("Cases.World.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("recoveries.var",c(1, 4, 7))]<-train[, paste0("Recoveries.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.world.var", c(1, 4, 7))]<-train[, paste0("Fatalities.World.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("percent.var", c(1, 4, 7))]<-train[, paste0("Percent.Positive.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.continent.var", c(1,4,7))]<-train[, paste0("Fatalities.Continent.lag",seq(cutoff, cutoff+6, 3))]
  # train[, paste0("critical.var", c(1,4, 7))]<-train[, paste0("Critical.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("percent.active.var", c(1, 4, 7))]<-train[, paste0("Percent.Active.lag",seq(cutoff, cutoff+6, 3))]
  
  #how much were people walking recently
  train[, "mobility.walking.var.mean"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff, cutoff+2, 1))], na.rm=T)
  train[, "mobility.walking.var.mean2"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+6, cutoff+8, 1))], na.rm=T)
  train[, "mobility.walking.var.mean3"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+12, cutoff+14, 1))], na.rm=T)
  
  train$var30<-rowMeans(train[, c(paste0("estCases.", cutoff), paste0("estCases.", cutoff), paste0("estCases.", cutoff), paste0("predCases.", cutoff))],na.rm=T)
  
  
  fit<-lm(log(eps+Fatalities)~
            log(eps+cases.var4*Obesity.deaths.2017/(10^6)*(Population/Population.Country))+
            
            
            #I(Negative.affect*age66*Population)+
            # log(eps+cases.var1*age66/100)+
            log(eps+cases.var1*Negative.affect)+
            log(eps+cases.var1*Share.of.adults.who.are.obese.2016/100)+
            log(eps+cases.var1*Obesity.deaths.2017/(10^6)*(Population/Population.Country))+
            
            log(eps+cases.var7*age60.male/100)+
            log(eps+cases.var4*age66/100)+
            log(eps+cases.var4*X70.years.old.deaths.per.100.000.2017/(10^4))+
            
            log(eps+fatalities.var1*Negative.affect)+
            log(eps+fatalities.var1*age66/100)+
            
            # log(eps+var33)+
            log(eps+var30)+
            
            #demographic variable
            I(Population*age66/100)+
            I(Population*age15.65/100)+
            #Dist.From.Country.Center+
            # hospibed+           
            #Estimated.prevalence.2012+
            #Estimated.prevalence.2012+
            #Deaths.Lower.respiratory.infections.Sex.Both.Age.Age.standardized.Rate.Rate.2017+
            
            #cases stats
            percent.active.var1+
            
            #number of cases variables
            # log(Days.Since.Case10.Country+1)+
            log(eps+recoveries.var7)+
            log(eps+recoveries.var4)+
            log(eps+recoveries.var1)+
            
            log(eps+(cases.var1-recoveries.var1-fatalities.var1)*age66/100)+
            log(eps+fatalities.var7)+
            log(eps+fatalities.var4)+
            log(eps+fatalities.var1)+
            + log(eps+cases.var7)+
            + log(eps+cases.var4)+
            log(eps+cases.var1) 
          # method="glmnet",
          # trControl=trainControl( method = "none") ,
          # tuneGrid=expand.grid(alpha=0, lambda=c(.0001)),
          , data=train[!is.na(train$percent.var7)& !is.na(train[, "Fatalities"]) & train$cases.var1>=observation.min  & train$Date<=max.train.date, ]);summary(fit)
  
  y.var<-paste0("predFatalities.", cutoff)
  train[,y.var]<-exp(predict(fit, newdata=train))-1
  train[which(train[, y.var]<train[, "fatalities.var1"]), y.var]<-train[which(train[, y.var]<train[, "fatalities.var1"]), "fatalities.var1"] #fatalities can't go down
  rmsle(y=train$Fatalities, y_pred=train[, y.var])
}


summary(train$Fatalities[train$Fatalities>20]/train$ConfirmedCases[train$Fatalities>20])
train$predFatalities.7.2<-ifelse(train$predFatalities.7>rowMeans(train[, c("predCases.7", "estCases.7","estCases.7", "estCases.7")], na.rm = T)*.14,
                                 rowMeans(train[, c("predCases.7", "estCases.7","estCases.7","estCases.7")], na.rm = T)*.14, train$predFatalities.7)
train$lgbFatalities.7.2<-ifelse(train$lgbFatalities.7>rowMeans(train[, c("predCases.7", "estCases.7","estCases.7", "estCases.7")], na.rm = T)*.14,
                                rowMeans(train[, c("predCases.7", "estCases.7","estCases.7","estCases.7")], na.rm = T)*.14, train$lgbFatalities.7)
bool<-!is.na(train$predFatalities.7)& 
  !is.na(train$predFatalities.7.2) & train$Date>as.Date("2020-04-06")& !is.na(train$Fatalities)& !is.na(train$estFatalities.7) #& !is.na(train$lgbFatalities.7) 
rmsle( train$Fatalities[bool], train$estFatalities.7[bool])
rmsle( train$Fatalities[bool], train$predFatalities.7[bool])
rmsle( train$Fatalities[bool], train$predFatalities.7.2[bool])
rmsle( train$Fatalities[bool], train$lgbFatalities.7.2[bool])
rmsle( train$Fatalities[bool], train$arimaFatalities.7[bool])
rmsle( train$Fatalities[bool], train$estFatalities.7[bool]*1/3+train$predFatalities.7.2[bool]*.5)
rmsle( train$Fatalities[bool], rowMeans(train[bool, c("predFatalities.7.2", "estFatalities.7", "arimaFatalities.7", "lgbFatalities.7")]))
# sum(bool)


#linear model predictions get weird sometimes for predicting far in future.. impute weird values
for(i in 7:max.cutoff){
  bool<-which(train[,paste0("predFatalities.", i)]>
                rowMeans(train[, c(paste0("estCases.", i), paste0("estCases.", i), paste0("estCases.", i), paste0("predCases.", i))],na.rm=T)*.14)
  train[bool, paste0("predFatalities.", i)]<-
    rowMeans(train[bool, c(paste0("estCases.", i), paste0("estCases.", i), paste0("estCases.", i), paste0("predCases.", i))],na.rm=T)*.14
}



for(cutoff in 1:max.cutoff){
  
  
  train[, paste0("cases.var", 1:7)]<-train[, paste0("Cases.lag",seq(cutoff, cutoff+6, 1))] #1, 4, 7
  train[, paste0("fatalities.var", 1:7)]<-train[, paste0("Fatalities.lag",seq(cutoff, cutoff+6, 1))]
  train[, paste0("recoveries.var",c(1:7))]<-train[, paste0("Recoveries.lag",seq(cutoff, cutoff+6, 1))]
  
  train[, paste0("negative.var", c(1, 4, 7))]<-train[, paste0("ConfirmedNegative.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("tests.var", c(1, 4, 7))]<-train[, paste0("Tests.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("cases.world.var", c(1, 4, 7))]<-train[, paste0("Cases.World.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("fatalities.world.var", c(1, 4, 7))]<-train[, paste0("Fatalities.World.lag",seq(cutoff, cutoff+6,3))]
  train[, paste0("fatalities.continent.var", c(1,4,7))]<-train[, paste0("Fatalities.Continent.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("percent.var", c(1, 4, 7))]<-train[, paste0("Percent.Positive.lag",seq(cutoff, cutoff+6, 3))]
  # train[, paste0("critical.var", c(1,4, 7))]<-train[, paste0("Critical.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("cases.continent.var", c(1,4,7))]<-train[, paste0("Cases.Continent.lag",seq(cutoff, cutoff+6, 3))]
  train[, paste0("percent.active.var", c(1, 4, 7))]<-train[, paste0("Percent.Active.lag",seq(cutoff, cutoff+6, 3))]
  
  #how much were people walking recently
  train[, "mobility.walking.var.mean"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff, cutoff+2, 1))], na.rm=T)
  train[, "mobility.walking.var.mean2"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+6, cutoff+8, 1))], na.rm=T)
  train[, "mobility.walking.var.mean3"]<-rowMeans(train[, paste0("Mobility.walking.lag",seq(cutoff+12, cutoff+14, 1))], na.rm=T)
  
  
  
  fit<-lm(log(eps+Recoveries)~
            log(eps+recoveries.var1*Negative.affect)+
            log(eps+recoveries.var1*Share.of.adults.who.are.obese.2016/100)+
            log(eps+recoveries.var1*Obesity.deaths.2017/(10^6)*(Population/Population.Country))+
            log(eps+recoveries.var1*age66/100)+
            
            log(eps+cases.var1*Negative.affect)+
            log(eps+cases.var1*Share.of.adults.who.are.obese.2016/100)+
            log(eps+cases.var1*Obesity.deaths.2017/(10^6)*(Population/Population.Country))+
            
            log(eps+cases.var4*age66/100)+
            log(eps+cases.var4*X70.years.old.deaths.per.100.000.2017/(10^4))+
            log(eps+cases.var4*Obesity.deaths.2017/(10^6)*(Population/Population.Country))+
            
            log(eps+fatalities.var1*Negative.affect)+
            log(eps+fatalities.var1*age66/100)+
            
            percent.active.var1+
            
            log(eps+cases.world.var4)+
            log(eps+cases.world.var1)+
            #number of cases variables
            log(eps+recoveries.var7)+
            log(eps+recoveries.var4)+
            log(eps+recoveries.var1)+
            
            log(eps+cases.var7-fatalities.var7)+
            log(eps+cases.var4-fatalities.var4)+
            log(eps+cases.var1-fatalities.var1)
          # method="glmnet",
          # trControl=trainControl( method = "none") ,
          # tuneGrid=expand.grid(alpha=0, lambda=c(.0001)),
          , data=train[!is.na(train$percent.var7)& !is.na(train[, "Recoveries"])  & train$cases.var1>=observation.min  &  train$Date<=max.train.date, ]);summary(fit)
  y.var<-paste0("predRecoveries.", cutoff)
  train[,y.var]<-exp(predict(fit, newdata=train))-1
  train[which(train[, y.var]<train[, "recoveries.var1"]), y.var]<-train[which(train[, y.var]<train[, "recoveries.var1"]), "recoveries.var1"] #recoveries can't go down
  rmsle(y=train$Fatalities, y_pred=train[, y.var])
}
train$predRecoveries.7.2<-ifelse(train$predRecoveries.7>=2*train$estRecoveries.7, train$estRecoveries.7*2, train$predRecoveries.7)
train$predRecoveries.7.2<-ifelse(train$predRecoveries.7.2<train$estCases.7*.03, train$estCases.7*.03, train$predRecoveries.7.2)

bool<-!is.na(train$predRecoveries.7)& !is.na(train$predRecoveries.7.2) & !is.na(train$Recoveries) & train$Date>as.Date("2020-04-06") 
rmsle( train$Recoveries[bool], train$arimaRecoveries.7[bool])
rmsle( train$Recoveries[bool], train$predRecoveries.7[bool])
rmsle( train$Recoveries[bool], train$estRecoveries.7[bool])
rmsle( train$Recoveries[bool], train$lgbRecoveries.7[bool])
rmsle( train$Recoveries[bool], train$predRecoveries.7.2[bool])
rmsle( train$Recoveries[bool],  rowMeans(train[bool, c( "arimaRecoveries.7","predRecoveries.7", "lgbRecoveries.7")]))
sum(bool)




###PREDICTIONS: CLEAN, FORCE MONOTONIC, AND ENSEMBLE######

#max.train.date =2020-04-12 means 2020-04-13 gets predcases.1, 2020-04-13 gets predcaes.2,  etc

train$predCases<-NA
train$predFatalities<-NA
train$predRecoveries<-NA

train$estCases<-NA
train$estFatalities<-NA
train$estRecoveries<-NA

train$lgbCases<-NA
train$lgbFatalities<-NA
train$lgbRecoveries<-NA

train$arimaCases<-NA
train$arimaFatalities<-NA
train$arimaRecoveries<-NA

for(i in 1:max.cutoff){
  train$predCases[train$Date<=max.train.date]<-train$ConfirmedCases[train$Date<=max.train.date]
  train$predCases[is.na(train$predCases)& train$Date<=max.train.date+i]<-train[is.na(train$predCases)& train$Date<=max.train.date+i, paste0("predCases.", i)]
  train$predFatalities[train$Date<=max.train.date]<-train$Fatalities[train$Date<=max.train.date]
  train$predFatalities[is.na(train$predFatalities)& train$Date<=max.train.date+i]<-train[is.na(train$predFatalities)& train$Date<=max.train.date+i, paste0("predFatalities.", i)]
  train$predRecoveries[train$Date<=max.train.date]<-train$Recoveries[train$Date<=max.train.date]
  train$predRecoveries[is.na(train$predRecoveries)& train$Date<=max.train.date+i]<-train[is.na(train$predRecoveries)& train$Date<=max.train.date+i, paste0("predRecoveries.", i)]
  
  train$arimaCases[train$Date<=max.train.date]<-train$ConfirmedCases[train$Date<=max.train.date]
  train$arimaCases[is.na(train$arimaCases)& train$Date<=max.train.date+i]<-train[is.na(train$arimaCases)& train$Date<=max.train.date+i, paste0("arimaCases.", i)]
  train$arimaFatalities[train$Date<=max.train.date]<-train$Fatalities[train$Date<=max.train.date]
  train$arimaFatalities[is.na(train$arimaFatalities)& train$Date<=max.train.date+i]<-train[is.na(train$arimaFatalities)& train$Date<=max.train.date+i, paste0("arimaFatalities.", i)]
  train$arimaRecoveries[train$Date<=max.train.date]<-train$Recoveries[train$Date<=max.train.date]
  train$arimaRecoveries[is.na(train$arimaRecoveries)& train$Date<=max.train.date+i]<-train[is.na(train$arimaRecoveries)& train$Date<=max.train.date+i, paste0("arimaRecoveries.", i)]
  
  
  train$lgbCases[train$Date<=max.train.date]<-train$ConfirmedCases[train$Date<=max.train.date]
  train$lgbCases[is.na(train$lgbCases)& train$Date<=max.train.date+i]<-train[is.na(train$lgbCases)& train$Date<=max.train.date+i, paste0("lgbCases.", i)]
  train$lgbFatalities[train$Date<=max.train.date]<-train$Fatalities[train$Date<=max.train.date]
  train$lgbFatalities[is.na(train$lgbFatalities)& train$Date<=max.train.date+i]<-train[is.na(train$lgbFatalities)& train$Date<=max.train.date+i, paste0("lgbFatalities.", i)]
  train$lgbRecoveries[train$Date<=max.train.date]<-train$Recoveries[train$Date<=max.train.date]
  train$lgbRecoveries[is.na(train$lgbRecoveries)& train$Date<=max.train.date+i]<-train[is.na(train$lgbRecoveries)& train$Date<=max.train.date+i, paste0("lgbRecoveries.", i)]
  
  train$estCases[train$Date<=max.train.date]<-train$ConfirmedCases[train$Date<=max.train.date]
  train$estCases[is.na(train$estCases)& train$Date<=max.train.date+i]<-train[is.na(train$estCases)& train$Date<=max.train.date+i, paste0("estCases.", i)]
  train$estFatalities[train$Date<=max.train.date]<-train$Fatalities[train$Date<=max.train.date]
  train$estFatalities[is.na(train$estFatalities)& train$Date<=max.train.date+i]<-train[is.na(train$estFatalities)& train$Date<=max.train.date+i, paste0("estFatalities.", i)]
  train$estRecoveries[train$Date<=max.train.date]<-train$Recoveries[train$Date<=max.train.date]
  train$estRecoveries[is.na(train$estRecoveries)& train$Date<=max.train.date+i]<-train[is.na(train$estRecoveries)& train$Date<=max.train.date+i, paste0("estRecoveries.", i)]
  
}



##ensemble##
#weight to optimize predictive accuracy
train$predCases.agg<-rowMeans(train[, c("estCases","estCases","arimaCases", "arimaCases", "lgbCases")], na.rm=T)
train$predFatalities.agg<-rowMeans(train[, c("predFatalities",'lgbFatalities', "estFatalities", "arimaFatalities")], na.rm=T)
train$predRecoveries.agg<- rowMeans(train[, c("predRecoveries",  "lgbRecoveries",  "arimaRecoveries")], na.rm=T)


#force monotonic with cummmax() : 
cols<-c("predCases.agg","predFatalities.agg", "predRecoveries.agg")
#cols<-c("predCases", "predFatalities", "predRecoveries", "lgbCases", "lgbFatalities", 'lgbRecoveries', "lgbCases")
train<-train[order(train$Date, decreasing = F),]
train<-data.table(train)[,   (cols):=lapply(.SD, function(x) {y<-cummax(x[!is.na(x)]);x[!is.na(x)]<-y;x}  ) ,
                         by=c("Country.Region", "Province.State"), .SDcols=cols]
train<-data.frame(train)


#if out of sample, can check accuracy.
bool<-train$Date>as.Date("2020-04-06") & !(train$Country.Region=="Canada" & train$Province.State=="") & train$Date!=as.Date("2020-04-23")

rmsle(train$ConfirmedCases[ bool], train$predCases.agg[bool])
rmsle(train$Fatalities[bool], train$predFatalities.agg[bool])
rmsle(train$Recoveries[bool], train$predRecoveries.agg[bool])



###EXAMINE RESULTS######

#optimize metric:
bool<-train$Date>as.Date("2020-04-06") & train$Cases.lag7>=100 & !is.na(train$Fatalities)

#sqrt(mean((log(actual+add)-log(pred+add))^2))
rmsle(y=c(train$Fatalities[bool], train$ConfirmedCases[bool], train$Recoveries[bool]),   
      y_pred=c(train$predFatalities.1[bool],train$estCases.1[bool], train$predRecoveries.1[bool]), add=1)
sum(!is.na(c(train$predFatalities.1[bool],train$estCases.1[bool], train$predRecoveries.1[bool])))

rmsle(y=c(train$Fatalities[bool], train$ConfirmedCases[bool],train$Recoveries[bool]),
      y_pred=c(train$predFatalities.7[bool],train$estCases.7[bool], train$predRecoveries.7[bool]), add=1)
sum(!is.na(c(train$predFatalities.7[bool],train$estCases.7[bool], train$predRecoveries.7[bool])))

rmsle(y=c(train$Fatalities[bool], train$ConfirmedCases[bool],train$Recoveries[bool]),
      y_pred=c(train$predFatalities.20[bool],train$estCases.20[bool], train$predRecoveries.20[bool]), add=1)
sum(!is.na(c(train$predFatalities.20[bool],train$estCases.20[bool], train$predRecoveries.20[bool])))


# train[train$Country.Region=='Italy' & train$Province.State==""& train$Date>=max.train.date-5,c("predCases.agg","predCases", "estCases", "arimaCases", "lgbCases", "ConfirmedCases", "Tests","Date", "Percent.Positive", "Recoveries")]
# 
#train[train$Country.Region=='US' & train$Province.State==""& train$Date>=max.train.date-5,c("predFatalities.agg", "predFatalities", "estFatalities", "lgbFatalities", "Fatalities","Date","Percent.Positive", "quarantine", "ConfirmedCases", 'Tests')]
# train[train$Country.Region=='US'& train$Province.State==''& train$Date>=max.train.date-5,c("predFatalities.agg", "estFatalities", "Fatalities","Date","Percent.Positive", "quarantine", "ConfirmedCases", 'Tests')]

# train[train$Country.Region=='Germany'& train$Date>=max.train.date-5,c("predRecoveries.agg", "estRecoveries", 'arimaRecoveries', "lgbRecoveries", "Recoveries","Date")]


###EXAMINE RESULTS--PLOTS###


countries<-data.table(train)[,list(estimated.fatalities=sum(predFatalities.agg, na.rm=T)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.fatalities, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
head(countries[countries$Date==max(countries$Date),])

ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.fatalities,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.fatalities[1]), colour="black", angle=0)+
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Regions- Fatalities")




countries<-data.table(train)[,list(estimated.cases=sum(predCases.agg, na.rm=T)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.cases, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
countries$label<-ifelse(countries$Date==max(countries$Date), as.character(countries$Country.Region), "")
head(countries[countries$Date==max(countries$Date),])

ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.cases,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.cases[1]), colour="black", angle=0)+
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Regions- Cases ")


countries<-data.table(train)[,list(estimated.recoveries=sum(predRecoveries.agg, na.rm=T)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.recoveries, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
countries$label<-ifelse(countries$Date==max(countries$Date), as.character(countries$Country.Region), "")
head(countries[countries$Date==max(countries$Date),])

ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.recoveries,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.recoveries[1]), colour="black", angle=0)+
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Regions- Recoveries ")

countries<-data.table(train)[train$Population>=10000,list(estimated.fatalities.percent=sum(predFatalities.agg, na.rm=T)/sum(Population)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.fatalities.percent, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
head(countries[countries$Date==max(countries$Date),])
ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.fatalities.percent,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.fatalities.percent[1]), colour="black", angle=0)+
  scale_y_continuous(labels = scales::percent_format(accuracy = .01))+ 
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Countries/States- Fatalities as % of Population", 
          subtitle = c("<10K population omitted"))

countries<-data.table(train)[train$Population>10000,list(estimated.cases.percent=sum(predCases.agg, na.rm=T)/sum(Population)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.cases.percent, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
head(countries[countries$Date==max(countries$Date),])

ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.cases.percent,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.cases.percent[1]), colour="black", angle=0)+
  scale_y_continuous(labels = scales::percent_format(accuracy = .01))+ 
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Countries/States- Cases as % of Population", 
          subtitle = c("<10K population omitted"))

countries<-data.table(train)[train$Population>10000,list(estimated.recoveries.percent=sum(predRecoveries.agg, na.rm=T)/sum(Population)), 
                             by=c("Date", "Country.Region", "Province.State")]
countries$Country.Region<-ifelse(countries$Province.State=="", countries$Country.Region,
                                 paste(countries$Country.Region,countries$Province.State ,sep="-"))
countries<-countries[order(countries$estimated.recoveries.percent, decreasing = T),]
countries$Country.Region <- factor(countries$Country.Region,levels = countries$Country.Region[countries$Date==max(countries$Date)],ordered = TRUE)
head(countries[countries$Date==max(countries$Date),])

ggplot(countries[countries$Country.Region%in% levels(countries$Country.Region)[1:15] ,],
       aes(x=Date,y=estimated.recoveries.percent,colour=Country.Region,group=Country.Region)) + 
  geom_line()+
  geom_vline(xintercept = max.train.date, linetype="dotted", 
             color = "black", size=1) +
  geom_text(aes(x=max.train.date - 2.6, label="<-Actual  \n Projected->", y=estimated.recoveries.percent[1]), colour="black", angle=0)+
  scale_y_continuous(labels = scales::percent_format(accuracy = .01))+ 
  scale_colour_discrete() +  #guide = 'none'
  ggtitle("Top 15 Projected Countries/States- Recoveries as % of Population", 
          subtitle = c("<10K population omitted"))




###WRITE/FORMAT SUBMISSION FILES####


recoveries_time_series<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recoveries_time_series$Country.Region2<-coordName(recoveries_time_series$Country.Region)
recoveries_time_series<-merge(recoveries_time_series, train[train$Date==as.Date("2020-05-01"), c("predRecoveries.agg", 'Country.Region', 'Province.State')], 
                              by.x=c("Country.Region2", 'Province.State'), by.y=c("Country.Region", "Province.State"), all.x=T, sort = F)
recoveries_time_series$Country.Region2<-NULL

recoveries_time_series$predRecoveries.agg<-round(recoveries_time_series$predRecoveries.agg, 0)
colnames(recoveries_time_series)[colnames(recoveries_time_series)=="predRecoveries.agg"]<-"X5.1.20"
colnames(recoveries_time_series)<-gsub("X", "", colnames(recoveries_time_series))
colnames(recoveries_time_series)<-gsub("[.]", "/", colnames(recoveries_time_series))
recoveries_time_series[1,]
write.csv(recoveries_time_series, file="time_series_covid19_recovered_global_submission.csv", row.names = F)



fatalities_time_series<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
fatalities_time_series$Country.Region2<-coordName(fatalities_time_series$Country.Region)
fatalities_time_series<-merge(fatalities_time_series, train[train$Date==as.Date("2020-05-01"), c("predFatalities.agg", 'Country.Region', 'Province.State')], 
                              by.x=c("Country.Region2", 'Province.State'), by.y=c("Country.Region", "Province.State"), all.x=T, sort = F)
fatalities_time_series$Country.Region2<-NULL

fatalities_time_series$predFatalities.agg<-round(fatalities_time_series$predFatalities.agg, 0)
colnames(fatalities_time_series)[colnames(fatalities_time_series)=="predFatalities.agg"]<-"X5.1.20"
colnames(fatalities_time_series)<-gsub("X", "", colnames(fatalities_time_series))
colnames(fatalities_time_series)<-gsub("[.]", "/", colnames(fatalities_time_series))
fatalities_time_series[is.na(fatalities_time_series$`5/1/20`),]
fatalities_time_series[is.na(fatalities_time_series)]<-0
summary(fatalities_time_series)
write.csv(fatalities_time_series, file="time_series_covid19_deaths_global_submission.csv", row.names = F)


cases_time_series<-read.csv("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
cases_time_series$Country.Region2<-coordName(cases_time_series$Country.Region)
cases_time_series<-merge(cases_time_series, train[train$Date==as.Date("2020-05-01"), c("predCases.agg", 'Country.Region', 'Province.State')], 
                         by.x=c("Country.Region2", 'Province.State'), by.y=c("Country.Region", "Province.State"), all.x=T, sort = F)
cases_time_series$Country.Region2<-NULL

cases_time_series$predCases.agg<-round(cases_time_series$predCases.agg, 0)
colnames(cases_time_series)[colnames(cases_time_series)=="predCases.agg"]<-"X5.1.20"
colnames(cases_time_series)<-gsub("X", "", colnames(cases_time_series))
colnames(cases_time_series)<-gsub("[.]", "/", colnames(cases_time_series))
cases_time_series[is.na(cases_time_series$`5/1/20`),]
cases_time_series[is.na(cases_time_series)]<-0
summary(cases_time_series)
write.csv(cases_time_series, file="time_series_covid19_confirmed_global_submission.csv", row.names = F)
