#Project: detection of chemical urine adulteration
#Titel: local interpretable model-agnostic explanations approach
#Author: Gabriel Streun
#Date: March 2020

#load the libraries
library(keras)
library(tibble)
library(lime)

#convert the train and test set matrices to tibbles
x_testdf<-as.data.frame(x_test)
x_traindf<-as.data.frame(x_train)
x_test_tbl<-as_tibble(x_testdf) #test set
x_train_tbl<-as_tibble(x_traindf) #train set

#Setup lime::model_type() function for keras
model_type.keras.engine.sequential.Sequential<-function(x, ...) {
  "classification"
}
#Setup lime::predict_model() function for keras
predict_model.keras.engine.sequential.Sequential<-function(x, newdata, type, ...) {
  pred<-predict_proba(object = x, x = as.matrix(newdata))
  data.frame(treated = pred[,2], untreated = pred[,1])
}
#Test the predict_model() function
predict_model.keras.engine.sequential.Sequential(x = model, newdata = x_test_tbl, type = 'raw') %>%
  tibble::as_tibble()

#Run lime() on training set
explainer<-lime::lime(
  x = x_train_tbl, 
  model = model
)
#Run explain() on explainer
explanation<-lime::explain(
  x_test_tbl[2:15,], #select a subset of the test set with an equal representation of both classes
  explainer = explainer, 
  n_labels = 2,
  n_features = 5
)
unique(explanation$feature)
