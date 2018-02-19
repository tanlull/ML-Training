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

train_data 


#### correlation
train_corr = cor(train_data[, c('SalePrice', 'X1stFlrSF', 'X2ndFlrSF', 'BedroomAbvGr', 'KitchenAbvGr','TotalBsmtSF') ],
                 use = "complete.obs")
corrplot(train_corr)


tree_model <- lm(SalePrice ~ X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenAbvGr +TotalBsmtSF  , data=train_data)  # 0.24  # -> add TotalBsmtSF get #0.23 

# Print model to see the CV result
tree_model


#### Prepare test data
summary(test_data)

# 5. get prediction
prediction <- predict(tree_model, test_data)

length(prediction)

# 6. export prediction
answers = data.frame(Id = test_data$Id, SalePrice=prediction)
summary(answers)
write.csv(answers, file="answers-workshop_house_price_varyBasic_3.csv", row.names = FALSE)
