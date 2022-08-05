#7.3
housing.df<-read.csv("~/BostonHousing.csv",header = T)
housing.df<-housing.df[,-14]
str(housing.df)

# as.factor 
housing.df$CHAS<-as.factor(housing.df$CHAS)
housing.df$RAD<-as.factor(housing.df$RAD)
# 60/40 split for training and validation
library(caret)
trainIndex <- createDataPartition(housing.df$MEDV, p=.6,list = FALSE, times = 1)
train.df <- housing.df[trainIndex,] 
valid.df <- housing.df[-trainIndex,] 



# use preProcess() from the caret package to normalize Data.
norm.values <- preProcess(train.df, method=c("center", "scale"))
train.norm.df <- as.data.frame(predict(norm.values, train.df))
valid.norm.df <- as.data.frame(predict(norm.values, valid.df))
housing.norm.df <- as.data.frame(predict(norm.values, housing.df))




train.knn.predictors<-train.norm.df[,-13]
train.knn.target<-train.df[,13]

valid.knn.predictors<-valid.norm.df[,-13]
valid.knn.target<-valid.df[,13]


#initialize a data frame with two columns: k, and accuracy
accuracy.df <- data.frame(k = seq(1, 5, 1), RMSE = rep(0, 5))

# compute knn for different k on validation.
for(i in 1:5){
  knn.pred<-class::knn(train = train.knn.predictors,                          
                       test = valid.knn.predictors,                          
                       cl =train.knn.target, k = i)
  accuracy.df[i,2]<-RMSE(as.numeric(as.character(knn.pred)),valid.knn.target)
}

accuracy.df


###############################  b #####################
#create new dataframe with table values
new.df<-data.frame(CRIM=0.2, ZN=0, INDUS=7 , CHAS=0, NOX=0.538, RM=6, AGE=62, DIS=4.7, RAD=4, TAX=307, PTRATIO=21, LSTAT=10)

#norm your new data
new.norm.values <- preProcess(new.df, method=c("center", "scale"))

new.norm.df <- predict(new.norm.values, newdata = new.df)

#predict the MEDV
new.knn.pred <- class::knn(train = train.knn.predictors,
                           test = new.norm.df,
                           cl = train.df$MEDV, k = 2)
new.knn.pred

######################## c ####################
new.accuracy.df<-RMSE(as.numeric(as.character(new.knn.pred)),valid.df[,13])
new.accuracy.df



