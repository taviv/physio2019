#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#scoring with an 12-hr sliding window adaboost model trained on setA plus B July 21th
# fixed mistakes in data pitching function 
library(fastAdaboost)

get_sepsis_score = function(data, model){
num_row<-nrow(tail(data,12))

if (num_row <= 6) {return(as.matrix(cbind(score=0,label=0)))}

else {
   
temp<- matrix(data=NA,nrow=12,ncol=40)    
temp[1:num_row,]<-tail(data[,1:40, drop=T],num_row)

mean<-apply(temp,2,mean,na.rm=T)
sd<-apply(temp,2,sd,na.rm=T)
sat_fi<-mean[2]/mean[11]
sat_fi[is.infinite(sat_fi)] = 5000
plat_wbc<-mean[34]/mean[32]

x<-as.matrix(c(mean,sat_fi,plat_wbc,sd),1,82)
x<-t(x)
x<-data.frame(x)
pred = predict(model,x[1,])
score<-pred$prob[2]
#label = score > 0.5
label <- as.numeric(as.character(pred$class))

    predictions = as.matrix(cbind(score, label)) 
    return(predictions)
}
}

load_sepsis_model = function(){
readRDS("ada12hrAB.model")

}

