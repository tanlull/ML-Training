#### Install packages (comment out if not needed)
install.packages('caret') # building model
install.packages('e1071') # tree model
install.packages('rpart.plot') # visualize tree
install.packages('Hmisc') # describe
install.packages("corrplot") # For correlation plot

#### loading packages
library(caret)
library(e1071)
library(rpart.plot)
library(Hmisc)
library(corrplot)

# 1. load dataset
train_data <- read.csv('C:\\Users\\tan\\Desktop\\Kaggle\\titanic_data\\train.csv')
test_data <- read.csv('C:/Users/tan/Desktop/Kaggle/titanic_data/test.csv')
str(test_data)
head(train_data)
head(test_data)

#### check the data
dim(train_data)

# 2. Explore data
str(train_data)
summary(train_data)
describe(test_data)

# correlation
train_corr = cor(train_data[, c('Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Fare')],
                 use = "complete.obs")
corrplot(train_corr)

# 3. Clean Data

# Clean train data

#### create a copy of train data
train_clean <- cbind(train_data)

#### impute missing values
train_clean[which(is.na(train_clean$Age)), 'Age'] <- mean(train_clean$Age, na.rm = TRUE)
train_clean[which(is.na(train_clean$Fare)), 'Fare'] <- mean(train_clean$Fare, na.rm = TRUE)

#### convert data type
train_clean$Pclass <- as.factor(train_clean$Pclass)
train_clean$Cabin <- as.character(train_clean$Cabin)

#### For train data
train_clean$Survived <- as.factor(train_clean$Survived)


# 4. Train model: Decision Tree
set.seed(1234)
tree_model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train_clean, 
                  method = "rpart",
                  metric = "Accuracy",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 1))

#### visualize model
rpart.plot(tree_model$finalModel)

# 5. Train model : Random Forest
# Optimize using Random Search
tree_model_random <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,  
                           data = train_clean, 
                           method = "cforest",  
                           metric = "Accuracy", 
                           tuneLength = 5, #tune length 
                           trControl = trainControl(method = "cv",  
                                                    number = 5,  
                                                    search = "random", # random search 
                                                    verboseIter = T))

# Print model
tree_model_random

#### Prepare test data
test_clean <- cleanData(test_data)

summary(test_clean)

# 6. get prediction

# Clean test data
#### create a copy of test data
test_clean <- cbind(test_data)

#### impute missing values
test_clean[which(is.na(test_clean$Age)), 'Age'] <- mean(test_clean$Age, na.rm = TRUE)
test_clean[which(is.na(test_clean$Fare)), 'Fare'] <- mean(test_clean$Fare, na.rm = TRUE)

#### convert data type
test_clean$Pclass <- as.factor(test_clean$Pclass)
test_clean$Cabin <- as.character(test_clean$Cabin)

# Get prediction
prediction <- predict(tree_model, newdata = test_clean, type = "raw", na.action = )
length(prediction)

# 7. export prediction
answers = data.frame(PassengerId = test_clean$PassengerId, Survived=prediction)
answers
write.csv(answers, file="answers-workshop1.csv", row.names = FALSE)
