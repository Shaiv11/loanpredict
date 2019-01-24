library(randomForest)
library(caret)

#setwd("C:/Users/Shaival/Desktop/Project_Training/Project")

data<-read.csv("lendloan.csv", quote = "", row.names = NULL)

#data cleaning
for (i in 1:nrow(data)) {
  data$emp_title[is.na(data$emp_title)]<-"GeekMindz"  
}

data$next_pymnt_d=ifelse(data$loan_status=="Charged Off",data$next_pymnt_d=="False",data$next_pymnt_d)
data$next_pymnt_d=ifelse(data$loan_status=="Fully Paid",data$next_pymnt_d=="False",data$next_pymnt_d)

bad_indicators<-c("Late(16-30 days)","Late(31-120 days)","Default","Charged Off")
data$class<-ifelse(data$loan_status %in% bad_indicators,"Bad",ifelse(data$loan_status=="",NA,"Good"))

table(data$class)

#write.csv(data, file = "s.csv", append = TRUE)

set.seed(2)
#id<-sample(2, nrow(data), prob = c(0.7,0.3), replace = TRUE)
#data_train<-data[id==1,]
#data_test<-data[id==2,]

#data$emp_title<-factor(data$emp_title)

rf<- randomForest(factor(data$class)~emp_title+annual_inc+loan_amnt+purpose+home_ownership+addr_state, data = data, family=binomial(), na.action = na.omit)
#fit<-train(factor(data$class)~grade+emp_title+annual_inc+loan_amnt+purpose+home_ownership+addr_state,data = data, method="lda",metric=metric, trControl=control)
#priority of variables
#rf$importance

#tuning
#bestmtry<-tuneRF(data, data$class, stepFactor = 1.2, improve = 0.01, trace=T, plot = T, na.rm=TRUE)


data$prob_default<-predict(rf, newdata = data, type = "prob")
data$prob_default<-ifelse(data$class=="Bad", (data$prob_default[,1]),(data$prob_default[,2]))

data[,c("class", "prob_default")]
