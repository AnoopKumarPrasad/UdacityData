#Read data file
mydata <- read.csv("Paper Presentation Data.csv")
View(mydata)
library(party)
set.seed(1234)
ind<-sample(2,nrow(mydata), replace = TRUE, prob=c(0.8,0.2))
train<-mydata[ind==1,]
test<-mydata[ind==2,]
Formu <- TOTALDET ~ DUST + NO + SMOKE
tree<- ctree(Formu,data=train)
tree
plot(tree)
tree1 <- ctree(Formu, data=train,controls = ctree_control(mincriterion = 0.9,minsplit=50))
tree1
plot(tree1)
predict(tree,test,type="prob")

library(rpart)
tree2<-rpart(Formu,train)
library(rpart.plot)
rpart.plot(tree2)

#PRediction
testPred<-predict(tree, newdata=test)
result<-table(testPred,test$TOTALDET)
print(result)
cm<-result
cm_accuracy<- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])*100
print("Accuracy % :")
print(round(cm_accuracy,1))