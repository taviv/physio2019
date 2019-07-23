#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#scoring with an 12-hr sliding window model trained in xgboost July 11th
# fix problems with data pitching 
library(xgboost)

get_sepsis_score = function(data, model){
   
temp<- matrix(data=NA,nrow=12,ncol=40)
num_row<-nrow(tail(data,12))    
temp[1:num_row,]<-tail(data[,1:40, drop=T],num_row)

mean<-apply(temp,2,mean,na.rm=T)
sd<-apply(temp,2,sd,na.rm=T)
sat_fi<-mean[2]/mean[11]
sat_fi[is.infinite(sat_fi)] = 5000
plat_wbc<-mean[34]/mean[32]

x<-c(mean,sat_fi,plat_wbc,sd)
x<-matrix(x,1,82)

score = predict(model,x)
label = score > 0.4

    predictions = as.matrix(cbind(score, label)) 
    return(predictions)

}

load_sepsis_model = function(){
xgb.load("xgb_windowAB.model")

}

