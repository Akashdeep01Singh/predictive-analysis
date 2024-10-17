getwd
intrusion_detection<-read.csv(file.choose(), stringsAsFactors = FALSE)
library(rpart)
library(rpart.plot)
View(intrusion_detection)
str(intrusion_detection)
set.seed(42)  #Setting seed for reproducibility 
indexes = sample(1:nrow(intrusion_detection),0.7*nrow(intrusion_detection))#Randomly select 70% of the data for training
intrusion_train=intrusion_detection[indexes, ]
View(intrusion_train)
intrusion_test.=intrusion_detection[-indexes, ]
target = class~.
tree = rpart(target,data = intrusion_train,method = "class")
rpart.plot(tree)
predictions=predict(tree,intrusion_test,type="class")
confusion_matrix=table(intrusion_test$class,predictions)
print(confusion_matrix)

#step 8: calculate accuracy 
accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
print(paste("Accuracy:",round(accuracy, 4)))