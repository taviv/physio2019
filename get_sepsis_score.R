#!/usr/bin/Rscript
# Tzvi Aviv tzvika.aviv@gmail.com
#note modifed driver.R
 
library(xgboost)

get_sepsis_score = function(data, model){
   
    m = nrow(data)
#get these "Age", "HR", "MAP", "O2Sat", "SBP", "Resp", "Temp", "Potassium", "Hct")
    x = as.matrix(data[1:m, c(35, 1, 5, 2, 4, 7, 3, 26, 29)])
    
    score = predict(model,x)
    label = score > 0.1
    predictions = as.matrix(cbind(score, label)) 
    return(predictions)
}

load_sepsis_model = function(){
xgb.load("xgboost5_ta.model")
   # return(NULL)
}
