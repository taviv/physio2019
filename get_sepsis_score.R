#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#scoring with an 12-hr sliding window model trained in xgboost on setA and B 
#aug 19 - best model was 1000 nrounds, depth=5, mcw=5, eta=0.2, subsample=0.5

library(xgboost)
library(matrixStats)

counter<-1
col_sums<-matrix(0,1,82)
col_sum_dif<-matrix(0,1,82)

get_sepsis_score = function(data, model){
counter <<- counter+1
#print(counter)
gender<-max(data[,36])     
temp<- matrix(data=NA,nrow=12,ncol=40)
num_row<-nrow(utils::tail(data,12))    
temp[1:num_row,]<-utils::tail(data[,1:40, drop=T],num_row)
#wmean<-apply(temp,2,mean,na.rm=T)
wmean<-colMeans(temp, na.rm=T)
#wsd<-apply(temp,2,sd,na.rm=T)
wsd<-colSds(temp, na.rm=T)
sat_fi<-wmean[2]/wmean[11]
sat_fi[is.infinite(sat_fi)] = 5000
plat_wbc<-wmean[34]/wmean[32]
x<-c(wmean,sat_fi,plat_wbc,wsd)
x<-matrix(x,1,82)
xtag<-x    
xtag[is.na(xtag)] <- 0
col_sums <<- col_sums + xtag
colmeans<-col_sums/(counter-1)
coldiff<-(x-colmeans)^2
colvar<-coldiff
colvar[is.na(colvar)]<-0  
col_sum_dif<<-col_sum_dif + colvar
colsds<-sqrt(col_sum_dif/(counter-1))

x<-data.frame((x-colmeans)/colsds)


#select top 20 variables
x<-x[,c('X40', 'X35', 'X39', 'X3', 'X1', 'X7', 'X4', 'X41' ,'X5', 'X2' ,'X43' ,'X6', 'X22', 'X44', 'X49', 'X46', 'X45', 'X47' ,'X32', 'X48')]
x<-cbind(x,gender)


score = predict(model,as.matrix(x))
label = score > 0.75

    predictions = as.matrix(cbind(score, label)) 
    return(predictions)

}

load_sepsis_model = function(){

xgb.load("xgb_windowAB_aug19") 
}
