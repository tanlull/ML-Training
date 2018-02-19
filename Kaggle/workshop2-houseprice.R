#### loading packages
library(caret)
library(corrplot)

# 1. load dataset
train_data <- read.csv('house_price_data/train.csv')
test_data <- read.csv('house_price_data/test.csv')
str(test_data)
head(train_data)
head(test_data)

#### check the data
dim(train_data)

## 2. explore data
str(train_data)
summary(train_data)

#### correlation
train_corr = cor(train_data[, c('SalePrice', 'X1stFlrSF', 'X2ndFlrSF', 'BedroomAbvGr', 'KitchenAbvGr')],
                 use = "complete.obs")
corrplot(train_corr)

# 3. clean data

# Select only first 1000 rows first
train_data1000 = train_data[1:1000,]

# 4. train model: Random Forest
set.seed(1234)
tree_model <- train(SalePrice ~ X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr,
                    data = train_data1000, 
                    method = "cforest",
                    metric = "RMSE",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 5,
                                             repeats = 1))

# Print model to see the CV result
tree_model

#### Optimize using Grid Search
my_grid_search <- expand.grid(mtry = c(3:5)) ## define grid search

tree_model <- train(SalePrice ~ X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr,
                    data = train_data1000, 
                    method = "cforest",
                    metric = "RMSE",
                    tuneGrid = my_grid_search, # tuneGrid 
                    trControl = trainControl(method = "cv",  
                                             number = 5,  
                                             search = "grid", # grid search 
                                             verboseIter = T))

# Print model
tree_model

#### Optimize using Random Search
tree_model_random <- train(SalePrice ~ X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr,  
                  data = train_data1000, 
                  method = "cforest",  
                  metric = "RMSE", 
                  tuneLength = 5, #tune length 
                  trControl = trainControl(method = "cv",  
                                           number = 5,  
                                           search = "random", # random search 
                                           verboseIter = T))

# Print model
tree_model_random

#### Prepare test data
summary(test_data)

# 5. get prediction
prediction <- predict(tree_model_random, newdata = test_data, type = "raw")
length(prediction)

# 6. export prediction
answers = data.frame(Id = test_data$Id, SalePrice=prediction)
answers
write.csv(answers, file="answers-workshop2.csv", row.names = FALSE)
