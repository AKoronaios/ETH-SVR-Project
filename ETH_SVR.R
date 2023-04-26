library(e1071)
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)
library(ISLR)
library(Metrics)
library(boot)
library(lubridate)
setwd("D:/Σχολή/9ο εξάμηνο/Νευρωνικά δίκτυα - Βαθιά Μάθηση/Εργασίες/2η Εργασία")
data<-read_csv("ETH_USD_2020-12-14_2021-12-13-CoinDesk.csv", 
                       col_types = cols(Currency = col_skip()))
str(data)
summary(data)
#Αλλαγή ονόματος στις στήλες του πίνακα των δεδομένων
data<-rename(data,Closing_Price=`Closing Price (USD)`)
data<-rename(data, D_Open =`24h Open (USD)`)
data<-rename(data, D_High =`24h High (USD)`)
data<-rename(data, D_Low =`24h Low (USD)`)

#Πρωτό γράφημα της τιμής του κρυπτονομίσματος
ggplot(melt(data,id = "Date"),aes(x=Date,y=value,color=variable))+
geom_line()

#Δημιουργία train set & test set (60/40)
set.seed(1234)
smp_size<-floor(0.60*nrow(data))                    

train_ind<-sample(seq_len(nrow(data)),size = smp_size)

train <-data[train_ind,]
test <- data[-train_ind,]

#Υπολογισμός μέσου & sd στο train set για την κανονικοποίηση των train & test data 
attach(train)
summary(train)

CP_mean<-mean(Closing_Price)
CP_sd<-sd(Closing_Price)

D_Open_mean<-mean(D_Open)
D_Open_sd<-sd(D_Open)

D_High_mean<-mean(train$D_High)
D_High_sd<-sd(train$D_High)

D_Low_mean<-mean(D_Low)
D_Low_sd<-sd(D_Low)

train$Closing_Price<-(train$Closing_Price-CP_mean)/CP_sd
train$D_Open<-(train$D_Open-D_Open_mean)/D_Open_sd
train$D_High<-(train$D_High-D_High_mean)/D_High_sd
train$D_Low<-(train$D_Low-D_Low_mean)/D_Low_sd

summary(train)

#Γράφιμα Date~D_Open
Date_D_Open_graph<-ggplot(train,aes(x=Date,y=D_Open))+
  geom_line(color="red")

#Κανονικοποίηση του test set μέσα από τις παραμέρους του train set
test$Closing_Price<-(test$Closing_Price-CP_mean)/CP_sd
test$D_Open<-(test$D_Open-D_Open_mean)/D_Open_sd
test$D_High<-(test$D_High-D_High_mean)/D_High_sd
test$D_Low<-(test$D_Low-D_Low_mean)/D_Low_sd

#Μοντέλο SVR για την πρόβλεψη της τιμής Closing Price από την μεταβλήτη Date
Closing_Price_tune<-tune.svm(Closing_Price~Date, data = train,gamma = 10^(-4:4), cost = 2^(-10:10))
pred_Closing_Price<-predict(Closing_Price_tune$best.model,test)
mse(test$Closing_Price,pred_Closing_Price)

#Μοντέλο SVR για την πρόβλεψη της τιμής D_Low από την μεταβλήτη Date
D_Low_tune<-tune.svm(D_Low~Date, data = train, gamma = 10^(-4:4), cost = 2^(-10:10))
pred_Low_Price<-predict(D_Low_tune$best.model,test)
mse(test$D_Low,pred_Low_Price)

#Μοντέλο SVR για την πρόβλεψη της τιμής D_High από την μεταβλήτη Date
D_High_tune<-tune.svm(D_High~Date, data = train, gamma = 10^(-4:4), cost = 2^(-10:10))
pred_High_Price<-predict(D_High_tune$best.model,test)
mse(test$D_High,pred_Low_Price)

new_data<-data.frame(train[c(1,3)],Closing_Price_tune$best.model$decision.values,
                              D_Low_tune$best.model$decision.values,
                              D_High_tune$best.model$decision.values)
new_test<-data.frame(test[1],pred_Closing_Price,pred_High_Price,pred_Low_Price)

colnames(new_data)<-c("Date","D_Open","Closing_Price","D_Low","D_High")
colnames(new_test)<-c("Date","Closing_Price","D_Low","D_High")

D_Open_tune<-tune.svm(D_Open~.,data = new_data, gamma = 10^(-7:7), cost = 2^(-12:12))

test_pred_best<-predict(D_Open_tune$best.model,new_test)
mse(test$D_Open,test_pred_best)

#Φτίαχνω ένα μοντελό για το D_Open άλλα εκπαιδεύτε με τα δεδομένα του train
D_Open_Date_Only_tune<-tune.svm(D_Open~Date,data = train,gamma = 10^(-7:7), cost = 2^(-12:12))
pred_D_Open_from_Date_Only<-predict(D_Open_Date_Only_tune$best.model,test)
mse(test$D_Open,pred_D_Open_from_Date_Only)

#Διμιουργία γραμμικού μοντέλου
library(ISLR)
D_Open_Linear_Model<-lm(D_Open~Date, data = train)
Lineal_pred<-predict(D_Open_Linear_Model,test)
mse(test$D_Open,Lineal_pred)

Date_D_Open_graph+geom_smooth(method = "lm",col="black")

d<-ymd("2022-1-13")
data_p<-as_tibble(d)
colnames(data_p)<-("Date")
predict(Closing_Price_tune$best.model,data_p)
predict(D_Low_tune$best.model,data_p)
predict(D_High_tune$best.model,data_p)

data_p<-cbind(data_p,predict(Closing_Price_tune$best.model,data_p),
              predict(D_Low_tune$best.model,data_p),
              predict(D_High_tune$best.model,data_p))
colnames(data_p)<-c("Date","Closing_Price","D_Low","D_High")
x<-predict(D_Open_tune$best.model,data_p)
x_real<-x*D_Open_sd+D_Open_mean
x_real
