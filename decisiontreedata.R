install.packages("C50")
install.packages("party")
library(party)
library(C50)
library(caret)
library(rpart)
Company_data = read.csv(choose.files())
company<- Company_data
mean(company$Sales)

Highsales<- ifelse(company$Sales <7.496,"No","Yes")
Highsales

company1<- data.frame(company[2:11],Highsales)
View(company1)

inTrainingLocal <- createDataPartition(company1$Highsales,p=.75,list = F)
training <- company1[inTrainingLocal,]
testing <- company1[-inTrainingLocal,]

#Model Building
modelA <- C5.0(training$Highsales~.,data=training) #Trails- Boosting Parameter
#model summary
summary(modelA)

#Predict for test data
pred <- predict.C5.0(modelA,testing[,-11])
a <- table(testing$Highsales,pred)
sum(diag(a))/sum(a) 
plot(modelA)

#Bagging
acc<-c()
for(i in 1:400)
{
  print(i)
  #Data partion
  inTrainingLocal <-createDataPartition(company1$Highsales,p=.85,list=F)
  training1 <-company1[inTrainingLocal,]
  testing<-company1[-inTrainingLocal,]
  #Model Building
  fittree <-C5.0(training1$Highsales~.,data=training1,trails=20)
  #predicting
  pred<- predict.C5.0(fittree,testing[,-11])
  a<-table(testing$Highsales,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)

#Boosting
#Data Partition
inTrainingLocal <-createDataPartition(company1$Highsales,p=.75,list=F)
training<-company1[inTrainingLocal,]
testing<-company1[-inTrainingLocal,]

#Model
modelB<-C5.0(training$Highsales~.,data=training,trials=10)
summary(modelB)

#predict test data set
pred<-predict.C5.0(modelB,testing[,-11])
b <- table(testing$Highsales,pred)
sum(diag(b))/sum(b) 
plot(modelB)


#Model1ss
model_C<-C5.0(training$Highsales~.,data=training,trials=20)
summary(model_C)

#predict test data set
pred1<-predict.C5.0(model_C,testing[,-11])
c <- table(testing$Highsales,pred1)
sum(diag(c))/sum(c) 
plot(model_C)