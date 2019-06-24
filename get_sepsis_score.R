#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#this model was trained with imputed NAs
 
library(xgboost)

get_sepsis_score = function(data, model){
   
#m = nrow(data)
#get these "ICULOS","Age", "HR", "MAP", "O2Sat", "SBP", "Resp", "Temp", "Potassium", "Hct")
    x = as.matrix(data[, c(40, 35, 1, 5, 2, 4, 7, 3, 26, 29)])
    
    score = tail(predict(model,x),1)
    label = score > 0.1 
    predictions = as.matrix(cbind(score, label)) 
    return(predictions)
}

load_sepsis_model = function(){
xgb.load("xgboost7_ta.model")
   # return(NULL)
}

