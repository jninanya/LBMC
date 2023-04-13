LBMC
=====

R script for preprocesing meteorological data to run the [LATEBLIGHT](https://doi.org/10.1094/PHYTO-95-1191) model. It also include a sensitivity analysis of the model.

R code
=====
```{r eval=F}
###############################################
# Analisis de sensibilidad y Calibracion
###############################################
per<-c(0.7,0.8,0.9,1.0,1.1,1.2,1.3)

####### 1) LP

LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-per*LP
n=length(ix)
out1<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
 LP<-ix[i]  
 model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
 out1[,i]=model$Ofile$SimSeverity 
}
write.csv(out1,"sens_analysis_for_LP.csv")

####### 2) LGR
LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-per*LGR
n=length(ix)
out2<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
  LGR<-ix[i]  
  model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
  out2[,i]=model$Ofile$SimSeverity 
}
write.csv(out2,"sens_analysis_for_LGR.csv")

####### 3) SR
LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-per*SR
n=length(ix)
out3<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
  SR<-ix[i]  
  model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
  out3[,i]=model$Ofile$SimSeverity 
}
write.csv(out3,"sens_analysis_for_SR.csv")

####### 4) IE
LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-per*IE
n=length(ix)
out4<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
  IE<-ix[i]  
  model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
  out4[,i]=model$Ofile$SimSeverity 
}
write.csv(out4,"sens_analysis_for_IE.csv")


####### 5) IL
LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-c(5,10,50,100,500,1000,10000)
n=length(ix)
out5<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
  IL<-ix[i]  
  IniSpor <- IL
  model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
  out5[,i]=model$Ofile$SimSeverity 
}
write.csv(out5,"sens_analysis_for_IL.csv")

####### 6) DOI
LP <- 3.10         # latent period (days)
LGR <- 0.00375 # lesion growth rate (m day-1)
SR <- 207000000  # sporulation rate (sporangia m-2 day-1)  
IE <- 0.9          # infection eficiency
IL <- 50          # initial lesion
DOI <- (as.Date(EmergDate)+11) # Day of inoculation

InMicCol <- 0
IniSpor <- IL
InocDate <- DOI

ix<-DOI+c(-6,-4,-2,0,2,4,6)
n=length(ix)
out6<-as.data.frame(matrix(nrow=99,ncol=n))
for(i in 1:n){
  DOI<-ix[i] 
  InocDate <- DOI
  model<-lateblight(WS, Cultivar="Shangi",ApplSys="nonfungicide", InocDate, LGR,IniSpor,SR,IE, LP, MatTime='LATESEASON',InMicCol)
  out6[,i]=model$Ofile$SimSeverity 
}
write.csv(out6,"sens_analysis_for_DOI.csv")

```

![plot](https://github.com/jninanya/LBMC/blob/main/fig_lbmodel_sa_v2.png)
