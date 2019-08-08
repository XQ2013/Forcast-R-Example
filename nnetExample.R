setwd("/Users/giyoon/Documents/Informs/Data")
data<-read.csv('CustR.csv')
install.packages('neuralnet')
library(neuralnet)
sum(is.na(data))
colSums(is.na(data))
data<-na.omit(data)
dim(data)
summary(data)
data$risk<-as.character(data$risk)
data$risk <- factor(data$risk, levels = c("1","2","3"),labels = c("Low","Medium","High"))

churn.matrix<-model.matrix(~risk+age+income+gender+marital+numkids+numcards+howpaid+mortgage+storecar+loans-1, data=data)
colnames(churn.matrix)<-c('Low','Medium','High','age','income','gender','marital','numkids','numcards','howpaid','mortgage','storecar','loans')
set.seed(100)
index <- sample(1:nrow(data),floor(nrow(data)*0.6))
train_set <- churn.matrix[index,]
test_set <- churn.matrix[-index,]
train_predictors <- train_set[,4:13]
test_predictors <- test_set[,4:13]
min_vector <- apply(train_predictors,2,min)
range_vector <- apply(train_predictors,2,max)-apply(train_predictors,2,min)
train_scaled <- cbind(scale(train_predictors,min_vector,range_vector),train_set[,1:3])
test_scaled <- cbind(scale(test_predictors,min_vector,range_vector),test_set[,1:3])
summary(train_scaled)
set.seed(100)
fit_1 <- neuralnet(Low + Medium + High ~ age+income+gender+marital+numkids+numcards+howpaid+mortgage+storecar+loans,train_scaled,hidden = c(8),linear.output = F)
plot(fit_1)

predictions <- compute(fit_1,test_scaled[,1:10]) 
output_sequence <- c('Low','Medium','High')
y_predicted <- apply(predictions$net.result,1,which.max)
y_true <- apply(test_scaled[,11:13],1,which.max)

confusion_matrix <- table(y_true,y_predicted) 
rownames(confusion_matrix) <- colnames(confusion_matrix) <- output_sequence
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

print(confusion_matrix)
print(paste('Accuracy',accuracy))