#Project: detection of chemical urine adulteration
#Titel: transform the data that result after Progenesis QI pre-processing 
#Author: Gabriel Streun
#Date: March 2020

#import the csv file with the abundance table exported from Progenesis QI
data<-as.matrix(read.csv(file.choose(), header=TRUE, sep=";"))

#inspect the data
dim(data)

#set the amount of samples
samples<-500

#display the normalized features and then store them in rawdata
data[2,16:(16+samples-1)]
rawdata<-data[2:nrow(data),16:(16+samples-1)]

#clean_PG_data function to obtain a matrix with columns=features and rows=samples
clean_PG_data<-function(rawdata){
  rawdata<-t(rawdata)
  rownames(rawdata)<-rawdata[,1]
  rawdata<-rawdata[,-1]
  colnames(rawdata)<-data[3:nrow(data),1]
  procdata<-sapply(rawdata, FUN=as.numeric)
  procdata<-matrix(data=procdata, nrow=dim(rawdata)[1], ncol=dim(rawdata)[2])
  colnames(procdata)<-data[3:nrow(data),1]
  rownames(procdata)<-NULL
  data<<-procdata
}
clean_PG_data(rawdata)

#inspect the data
dim(data)

#add_labels function to attach the sample labels as a new column (1=treated, 0=untreated)
#prerequisite: experimental design in Progenesis QI started with the treated group and there is an equal amount of treated and untreated samples
add_labels<-function(data){
  t<-rep(c(1), times = dim(data)[1]/2)
  ut<-rep(c(0), times = dim(data)[1]/2)
  m<-matrix(ncol=1,nrow=dim(data)[1], c(t,ut))
  data<<-cbind(m,data)
  colnames(data)[1]<-"class"
}
add_labels(data)

#inspect the data
dim(data)
