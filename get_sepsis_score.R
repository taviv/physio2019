#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#scoring with an 12-hr sliding window model trained in xgboost on setA and B Aug05
# centering/scaling will be done on the fly with matrixStats Aug09 


library(xgboost)
library(matrixStats)


alldata<-matrix(NA,1,20)
names(alldata)<-c('X40', 'X35', 'X39', 'X3', 'X1', 'X7', 'X4', 'X41' ,'X5', 'X2' ,'X43' ,'X6', 'X22', 'X44', 'X49', 'X46', 'X45', 'X47' ,'X32', 'X48')

get_sepsis_score = function(data, model){
 
temp<- matrix(data=NA,nrow=12,ncol=40)
num_row<-nrow(tail(data,12))    
temp[1:num_row,]<-tail(data[,1:40, drop=T],num_row)
wmean<-apply(temp,2,mean,na.rm=T) #replace with colMeans?
wsd<-apply(temp,2,sd,na.rm=T)     #replace with colSds? 
sat_fi<-wmean[2]/wmean[11]
sat_fi[is.infinite(sat_fi)] = 5000
plat_wbc<-wmean[34]/wmean[32]
x<-c(wmean,sat_fi,plat_wbc,wsd)
x<-data.frame(matrix(x,1,82))

#select top 20 variables
x<-x[,c('X40', 'X35', 'X39', 'X3', 'X1', 'X7', 'X4', 'X41' ,'X5', 'X2' ,'X43' ,'X6', 'X22', 'X44', 'X49', 'X46', 'X45', 'X47' ,'X32', 'X48')]

alldata <<- rbind(alldata,as.matrix(x))

scaled<-colScale(alldata) 
x<-data.frame(t(scaled[nrow(scaled),]))
score = predict(model,as.matrix(x))
label = score > 0.001

 predictions = as.matrix(cbind(score, label))
    return(predictions)

}

load_sepsis_model = function(){
xgb.load("xgb_windowAB_aug05")

}

# function for efficient normalization using matrixStats
#https://hopstat.wordpress.com/2016/02/23/a-faster-scale-function/
colScale = function(x,
    center = TRUE,
    scale = TRUE,
    add_attr = TRUE,
    rows = NULL,
    cols = NULL) {
 
    if (!is.null(rows) && !is.null(cols)) {
        x <- x[rows, cols, drop = FALSE]
    } else if (!is.null(rows)) {
        x <- x[rows, , drop = FALSE]
    } else if (!is.null(cols)) {
        x <- x[, cols, drop = FALSE]
    }
 
  ################
  # Get the column means
  ################
    cm = colMeans(x, na.rm = TRUE)
  ################
  # Get the column sd
  ################
    if (scale) {
        csd = colSds(x, center = cm, na.rm=T)
    } else {
        # just divide by 1 if not
        csd = rep(1, length = length(cm))
    }
    if (!center) {
        # just subtract 0
        cm = rep(0, length = length(cm))
    }
    x = t( (t(x) - cm) / csd )
    if (add_attr) {
        if (center) {
            attr(x, "scaled:center") <- cm
        }
        if (scale) {
            attr(x, "scaled:scale") <- csd
        }
    }
    return(x)
}
