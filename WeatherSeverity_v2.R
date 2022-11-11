#####################################################################
# Weather data
#####################################################################
df<-read.xlsx("HH-DST_LB-trial_data_Oct 12_2022.xlsx",sheet="Weather_1",startRow=2)
head(df)
ww<-df[,2:6]
colnames(ww)<-c("DateTime","temp_F","temp","rhum","DewPt_F")
ww$DateTime<-convertToDateTime(ww$DateTime)
ww$group_date<-as.character(date(ww$DateTime))
ww$group_hour<-paste0(date(ww$DateTime)," ",hour(ww$DateTime))

df<-read.xlsx("HH-DST_LB-trial_data_Oct 12_2022.xlsx",sheet="Weather_2",startRow=2)
head(df)
rr<-df[,c(2,3,5)]
colnames(rr)<-c("DateTime","par","srad")
rr$DateTime<-convertToDateTime(rr$DateTime)
rr$group_date<-as.character(date(rr$DateTime))

df<-read.xlsx("HH-DST_LB-trial_data_Oct 12_2022.xlsx",sheet="Rainfall",startRow=2)
head(df)
pp<-df[,c(2,6)]
colnames(pp)<-c("date","prec")
pp$date<-convertToDateTime(pp$date)

###################################################################
smr_date_mean<-ww%>%
  group_by(group_date)%>%
  summarize_if(is.numeric, mean, na.rm=TRUE)

smr_date_min<-ww%>%
  group_by(group_date)%>%
  summarize_if(is.numeric, min, na.rm=TRUE)

smr_date_max<-ww%>%
  group_by(group_date)%>%
  summarize_if(is.numeric, max, na.rm=TRUE)

smr_date_sum<-rr%>%
  group_by(group_date)%>%
  summarize_if(is.numeric, max, na.rm=TRUE)
smr_date_sum$srad_MJ_day<-smr_date_sum$srad*0.0864/2.5

###################################################################  
smr_hour_mean<-ww%>%
  group_by(group_hour)%>%
  summarize_if(is.numeric, mean, na.rm=TRUE)

smr_hour_min<-ww%>%
  group_by(group_hour)%>%
  summarize_if(is.numeric, min, na.rm=TRUE)

smr_hour_max<-ww%>%
  group_by(group_hour)%>%
  summarize_if(is.numeric, max, na.rm=TRUE)

wdata<-data.frame("date"=smr_date_mean$group_date,
                  "temp"=smr_date_mean$temp,
                  "tmin"=smr_date_min$temp,
                  "tmax"=smr_date_max$temp,
                  "rhum"=smr_date_mean$rhum,
                  "rhumn"=smr_date_min$rhum,
                  "rhumx"=smr_date_max$rhum,
                  "srad"=smr_date_sum$srad_MJ_day)
wdata<-subset(wdata,wdata$srad>5)
seq_date<-seq(SowingDate, as.Date(wdata$date[1])-1, by="days")
miss_wdata<-data.frame("date"=seq_date)
miss_wdata$temp<-mean(wdata$temp,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$tmin<-mean(wdata$tmin,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$tmax<-mean(wdata$tmax,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$rhum<-mean(wdata$rhum,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$rhumn<-mean(wdata$rhumn,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$rhumx<-mean(wdata$rhumx,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)
miss_wdata$srad<-mean(wdata$srad,na.rm=TRUE)*runif(length(seq_date),min=0.7,max=1.3)

climate<-data.frame("date"= c(miss_wdata$date, wdata$date))
climate$temp<-c(miss_wdata$temp, wdata$temp)
climate$tmin<-c(miss_wdata$tmin, wdata$tmin)
climate$tmax<-c(miss_wdata$tmax, wdata$tmax)
climate$rhum<-c(miss_wdata$rhum, wdata$rhum)
climate$rhumn<-c(miss_wdata$rhumn, wdata$rhumn)
climate$rhumx<-c(miss_wdata$rhumx, wdata$rhumx)
climate$srad<-c(miss_wdata$srad, wdata$srad)

climate$tt<-thermalTime(date=climate$date,tmin=climate$tmin,
            tmax=climate$tmax, sowing=SowingDate, 
            endHarvest=as.Date(climate$date[length(climate$date)]), 
            EmergencyDays = 15, parameters = c(0, 12, 24, 35))$tt
climate$dap<-as.numeric(climate$date - SowingDate)

v1=vector(length=nrow(smr_hour_mean))
v2=vector(length=nrow(smr_hour_mean))

for(i in 1:nrow(smr_hour_mean)){
  v1[i]=strsplit(smr_hour_mean$group_hour[i]," ")[[1]][1]
  v2[i]=strsplit(smr_hour_mean$group_hour[i]," ")[[1]][2]
}
w1<-data.frame("date"=v1,"hour"=v2)
w1$temp<-smr_hour_mean$temp
w1$rhum<-smr_hour_mean$rhum
w1$HumidHrs<-NA
w1$humidtmp<-NA
w1$HumidHrs<-ifelse(w1$rhum>RHthreshold,1,w1$HumidHrs)
w1$humidtmp<-ifelse(w1$rhum>RHthreshold,w1$temp,w1$humidtmp)

smr_w1_mean<-w1%>%
  group_by(date)%>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

smr_w1_sum<-w1%>%
  group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=TRUE)

Wfile<-data.frame("Date"=as.Date(smr_w1_sum$date))
Wfile$Rainfall<-pp$prec
Wfile$Tmp<-smr_w1_mean$temp
Wfile$HumidHrs<-smr_w1_sum$HumidHrs
Wfile$humidtmp<-smr_w1_mean$humidtmp
rownames(Wfile)<-Wfile$Date

(EndEpidDate<-as.Date(Wfile$Date[nrow(Wfile)]))
(EmergDate<-as.Date(WS1$EmergDate))
WS<-list("Wfile"=Wfile, "Sfile"=WS1$Sfile, 
         "EmergDate"=EmergDate,"EndEpidDate"=EndEpidDate)
