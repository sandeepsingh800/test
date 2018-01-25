##The R-Code has been simplified for the individual product categories.
##Let's refer to this during our meeting.
##~~~~~~~~~~~~~~~~~~~~~~~~~


require(forecast)
require(xts)
require(lubridate)
require(RODBC)

#Historical Input (Train)
db <- odbcConnect("SHC EDW PROD1", uid = "dsankar", pwd = "dheeraj88")
DF <- sqlQuery(db, "select a.*,    case when Ny=1 then 1
               when Md=1 then 1
               when Id=1 then 1
               when Ld=1 then 1
               when Chd=1 then 1
               when Thd=1 then 1
               else 0 end as hol  from   hs_perm_tbls.lv_ih_dcp_history_daily_mix a where prodcat1='COOK'  ")
odbcClose(db)

#Forecast Info Input
db <- odbcConnect("SHC EDW PROD1", uid = "dsankar", pwd = "dheeraj88")
DFxreg <- sqlQuery(db, "select a.*,
                   case when Ny=1 then 1
                   when Md=1 then 1
                   when Id=1 then 1
                   when Ld=1 then 1
                   when Chd=1 then 1
                   when Thd=1 then 1
                   else 0 end as hol  from   hs_perm_tbls.lv_ih_dcp_xreg_daily_fcst_mix a where prodcat1='COOK'  ")
odbcClose(db)

#Segments to model
DF$bycat<-paste(DF$pay_met2,DF$prodcat1,DF$region,DF$dru_id,DF$iru_id,DF$fru_id,DF$sub_cat, sep="-")
DFxreg$bycat<-paste(DFxreg$pay_met2,DFxreg$prodcat1,DFxreg$region,DFxreg$dru_id,DFxreg$iru_id,DFxreg$fru_id,DFxreg$sub_cat,sep="-")

##Forecast Function
mymain <- function (mydata,mydata1, strColMeasure, strColForecastBy, strColDate, iDaysToForecast, iConfidenceLevel)
{
  ##   Count how many time series need to be forecasted
  strTimeSeriesNames = unique(mydata[, strColForecastBy])
  iTimeSeriesCount <- length(strTimeSeriesNames)
  mysubset <- NULL
  mysubset1 <- NULL
  
  myforecast <- NULL
  myforecast1 <- NULL
  mytempfutureset <- NULL
  myfuturesetcollection <- NULL
  hd<-NULL
  hd2<-NULL
  xreg<-NULL
  xreg1<-NULL
  h1<-NULL
  
  for (i in 1:iTimeSeriesCount)
  {
    ## Create an individual and sorted dataset for the current time series
    mysubset <- mydata [mydata[, strColForecastBy]==strTimeSeriesNames[i],]
    mysubset <- mysubset[order(mysubset[, strColDate]),]
    
    mysubset1 <- mydata1 [mydata1[, strColForecastBy]==strTimeSeriesNames[i],]
    mysubset1 <- mysubset1[order(mysubset1[, strColDate]),]
    
    ## HOLIDAYS
    hd <- cbind(wday=model.matrix(~as.factor(mysubset$hol)))
    hd <- hd[,-1]
    xreg <- cbind(hd)
    hd2 <- cbind(wday=model.matrix(~as.factor(mysubset1$hol)))
    hd <- hd2[,-1]
    xreg1 <- cbind(hd)
    h1 <- length(unique(mysubset1$acctg_yr_wk))
    
    # Rob Hyndman daily forecasting guide
    # Training
    y <- ts(mysubset$completed, frequency=365.25/7) # using diff freq
    bestfit <- list(aicc=Inf)
    for(tt in 1:25)
    {
      fit <- auto.arima(y, xreg=cbind(xreg, fourier(y, K=tt)), seasonal=FALSE)
      if(fit$aicc < bestfit$aicc)
        bestfit <- fit
      else break;
    }
    
    K_<-tt-1
    print(K_)
    
    ## Carry out the forecast
    myforecast <- try(forecast(bestfit, xreg=cbind(xreg1,fourier(y, K=K_, h=h1)), h=h1,level=iConfidenceLevel))
    if (class(myforecast) == 'try-error')next;
    
    ## Plot chart if within maxiumum of previously defined number of charts
    myheader <- paste(strTimeSeriesNames[i],  '\n', ' ', i, '/', iTimeSeriesCount, ' ',sep='')
    plot(myforecast,main=myheader)
    print(myheader)
    
    
    ## Bring forecast into format for union with actuals
    mytempfutureset <- data.frame(Date=paste(mysubset1$acctg_yr_wk,sep=""), ForecastBy=strTimeSeriesNames[i], Measure=as.numeric(myforecast$mean),  Type='Forecast', PILower=as.numeric(myforecast$lower), PIUpper=as.numeric(myforecast$upper))
    
    ## Union current forecast with previously forecasted time series
    myfuturesetcollection <- rbind(mytempfutureset, myfuturesetcollection)
  }
  
  ## Union actuals and forecasts for all time series
  output <- rbind(myfuturesetcollection)
  ## Return the actuals and forecasts
  return(list(out=output))
}

#Input for function
asd<-mymain(DF,DFxreg,"completed","bycat","acctg_yr_wk",78,.95)

#Output Treatment
asd12<-data.frame(asd)
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
asd12$pay_met2 <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 1)
asd12$prodcat1 <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 2)
asd12$region <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 3)
asd12$dru_id <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 4)
asd12$iru_id <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 5)
asd12$fru_id <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 6)
asd12$sub_cat <- lapply(strsplit(as.character(asd12$out.ForecastBy), "-"), "[", 7)
asd12$predict <- specify_decimal(asd12$out.Measure,4)

a <- data.frame(lapply(asd12, as.character), stringsAsFactors=FALSE)
head(a)
a<-subset(a,select=c("out.Date","pay_met2","out.Type","region","prodcat1","dru_id","iru_id","fru_id","sub_cat","out.Measure"))

#Write Call
fcst<-subset(a,out.Type=='Forecast')
path1 <- "D:\\R\\Wk_fcst"
appendage1 <- "._fcst_arima_COOK.csv"
string <- format(Sys.time(), format = "%Y-%B-%d-%H%M%S")
outputFile1 <- file.path(path1, paste0(string, appendage1))
write.csv(fcst, file = outputFile1)