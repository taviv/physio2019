#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#scoring with an 12-hr sliding window model trained in xgboost on setA and B Aug05
# centering/scaling will be done on the new data 
library(xgboost)

#counter<-0
alldata<-data.frame(matrix(NA,1,20))
names(alldata)<-c('X40', 'X35', 'X39', 'X3', 'X1', 'X7', 'X4', 'X41' ,'X5', 'X2' ,'X43' ,'X6', 'X22', 'X44', 'X49', 'X46', 'X45', 'X47' ,'X32', 'X48')



get_sepsis_score = function(data, model){
#counter <<- counter+1
#print(counter)   
temp<- matrix(data=NA,nrow=12,ncol=40)
num_row<-nrow(tail(data,12))    
temp[1:num_row,]<-tail(data[,1:40, drop=T],num_row)
mean<-apply(temp,2,mean,na.rm=T)
sd<-apply(temp,2,sd,na.rm=T)
sat_fi<-mean[2]/mean[11]
sat_fi[is.infinite(sat_fi)] = 5000
plat_wbc<-mean[34]/mean[32]
x<-c(mean,sat_fi,plat_wbc,sd)
x<-data.frame(matrix(x,1,82))

#select top 20 variables
x<-x[,c('X40', 'X35', 'X39', 'X3', 'X1', 'X7', 'X4', 'X41' ,'X5', 'X2' ,'X43' ,'X6', 'X22', 'X44', 'X49', 'X46', 'X45', 'X47' ,'X32', 'X48')]

alldata <<- rbind(alldata,x)
#colmeans<-colMeans(alldata, na.rm=T)
#colsd<-apply(alldata,2,sd,na.rm=T)
scaled<-scale(alldata,center=T,scale=T)
x<-data.frame(t(scaled[nrow(scaled),]))

score = predict(model,as.matrix(x))
label = score > 0.001

    predictions = as.matrix(cbind(score, label)) 
    return(predictions)

}

load_sepsis_model = function(){
xgb.load("xgb_windowAB_aug05")

}

