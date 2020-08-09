#Project: detection of chemical urine adulteration
#Titel: cross-validation of the fully-connected artificial neural network model 
#Author: Gabriel Streun
#Date: March 2020

#load libraries
library(keras)
library(caret)

#set batch size and epochs
batch_size<-1
epochs<-20

#scale columns to a range = [0,1] and shuffle the rows
data[,2:ncol(data)]<-normalize(data[,2:ncol(data)])
data<-data[sample(nrow(data)),]

#create x and y train set for cross-validation
x_train<-data[, 2:ncol(data)]
y_train<-data[, 1]
testtargets<-data[, 1]

#one hot encoding
y_train<-to_categorical(y_train)

#prepare and split the dataset for k fold cross validation
k<-10
indices<-sample(1:nrow(x_train))
folds<-cut(indices,breaks=k,lables=FALSE)

#initialize empty vectors to capture each iteration's output measures 
acc_scores<-c()
sens_scores<-c()
spez_scores<-c()
ppv_scores<-c()
npv_scores<-c()

#conduct k fold cross validation
for(i in 1:k){
  cat("processing fold #", i, "\n")
  
  val_indices<-which(folds == levels(folds)[i], arr.ind=TRUE)
  
  val_data<-x_train[val_indices,]
  val_targets<-y_train[val_indices,]
  val_targets_vector<-testtargets[val_indices]
  partial_train_data<-x_train[-val_indices,]
  
  partial_train_targets<-y_train[-val_indices,]
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 76, activation = 'relu', input_shape = dim(data)[2]-1, regularizer_l2(0.01)) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 43, activation = 'relu', regularizer_l2(0.01)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(loss='binary_crossentropy', optimizer='adam', metrics='accuracy')
  
  model %>% fit(partial_train_data, partial_train_targets, batch_size = batch_size, epochs = epochs, shuffle=TRUE,verbose=1)
  
  results<-model %>% evaluate(val_data,val_targets)
  
  pred <- model %>% predict_classes(val_data)
  
  table1 <- table(Predicted=pred, Actual =  val_targets_vector)[2:1,2:1]
  
  acc_scores<-c(acc_scores,results$acc)
  sens_scores<-c(sens_scores,sensitivity(table1))
  spez_scores<-c(spez_scores,specificity(table1))
  ppv_scores<-c(ppv_scores,posPredValue(table1))
  npv_scores<-c(npv_scores,negPredValue(table1))
}

#display the mean of each output measure with range
x<-mean(acc_scores)
paste("After 10-fold CV the mean accuracy is ", round(x*100,2), "% ", "(Min = ", min(acc_scores)*100, "%, ", "Max = ", max(acc_scores)*100, "%)", sep="")

x<-mean(sens_scores)
paste("After 10-fold CV the mean sensitivity is ", round(x*100,2), "% ", "(Min = ", min(sens_scores)*100, "%, ", "Max = ", max(sens_scores)*100, "%)", sep="")

x<-mean(spez_scores)
paste("After 10-fold CV the mean specificity is ", round(x*100,2), "% ", "(Min = ", min(spez_scores)*100, "%, ", "Max = ", max(spez_scores)*100, "%)", sep="")

x<-mean(ppv_scores)
paste("After 10-fold CV the mean ppv is ", round(x*100,2), "% ", "(Min = ", min(ppv_scores)*100, "%, ", "Max = ", max(ppv_scores)*100, "%)", sep="")

x<-mean(npv_scores)
paste("After 10-fold CV the mean npv is ", round(x*100,2), "% ", "(Min = ", min(npv_scores)*100, "%, ", "Max = ", max(npv_scores)*100, "%)", sep="")


