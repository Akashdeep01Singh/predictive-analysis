#step1:install and load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


#step 2:load the iris dataset 
data(iris)
str(iris)


#Step 3:split the data into training and test sets 
set.seed(42)  #Setting seed for reproducibility 
indexes = sample(1:nrow(iris),0.7*nrow(iris))#Randomly select 70% of the data for training
iris_train=iris[indexes, ]
View(iris_train)
iris_test=iris[-indexes, ]

#Step 4:Define the target formula and train the decision tree

target = Species~Sepal.Length+Sepal.Width + Petal.Length + Petal.Width
tree = rpart(target,data = iris_train,method = "class")

#step 5:Visualize the decision tree
rpart.plot(tree)

#step 6: Make prediction on the test set 
predictions=predict(tree,iris_test,type="class")


#step 7: Evaluate the model by  creating a confusion matrix
confusion_matrix=table(iris_test$Species,predictions)
print(confusion_matrix)

#step 8: calculate accuracy 
accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
print(paste("Accuracy:",round(accuracy, 4)))