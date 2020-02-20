######################### set the working directory ##########################
setwd("C:\\Users\\DELL\\Desktop\\DS\\Term 2 and Term 3 Data and Project\\repgdssterm23projectexamdate\\Term 3 Project\\Term 3 Project")

######################### Import datasets ####################################
train_data <- read.csv("Train.csv")
test_data <- read.csv("Test.csv")

target <- test_data$Reached.on.Time_Y.N
######################### Exploratory data analysis ###########################
install.packages("xgboost")
library(dplyr)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
glimpse(train_data)
train_data$Customer_care_calls <- as.factor(train_data$Customer_care_calls)
train_data$Customer_rating <- as.factor(train_data$Customer_rating)
train_data$Prior_purchases <- as.factor(train_data$Prior_purchases)

######################### one hot encoding  for train #####################################
cat_data <- dplyr::select_if(train_data, is.factor)
num_data <- dplyr::select_if(train_data, is.integer)

var_name <- names(cat_data)
var <- paste(var_name, collapse = "+")

dummies<- dummyVars("~.", fullRank = TRUE,
                    data=cat_data )

data_ohe <- as.data.frame(predict(dummies, newdata = train_data))
df_train <- cbind(num_data, data_ohe)

############################ test_data_cleaning #####################
test_data$Customer_care_calls <- as.factor(test_data$Customer_care_calls)
test_data$Customer_rating <- as.factor(test_data$Customer_rating)
test_data$Prior_purchases <- as.factor(test_data$Prior_purchases)

######################### one hot encoding  for test #####################################
cat_data_test <- dplyr::select_if(test_data, is.factor)
num_data_test <- dplyr::select_if(test_data, is.integer)

var_name_test <- names(cat_data_test)
ncol(cat_data_test)
ncol(num_data_test)
var_test <- paste(var_name_test, collapse = "+")

dummies_test<- dummyVars("~.", fullRank = TRUE,
                    data=cat_data )

data_ohe_test <- as.data.frame(predict(dummies_test, newdata = test_data))
df_test <- cbind(num_data_test, data_ohe_test)
df_test <- cbind(df_test, target)

######################### divide the data into train and test ########################
set.seed(4511)
split  <- sample(1:nrow(df_train), 0.7 * nrow(df_train))
train <- df_train[split, ]
test <- df_train[-split, ]

train$ID <- NULL
test$ID <- NULL

prop.table(table(df_train$Reached.on.Time_Y.N))
prop.table(table(train$Reached.on.Time_Y.N))
prop.table(table(test$Reached.on.Time_Y.N))

######################### fit the model #####################################
######################### logistic Regression model ####################################

fit_lg <- glm(Reached.on.Time_Y.N ~.,
                data = train,
                family = "binomial")
summary(fit_lg)

#################### steo wise variable selection method
#step(fit_lg)

################### fit the model on important variables
fit_glm <- glm(Reached.on.Time_Y.N ~ Cost_of_the_Product + 
                 Discount_offered + 
                 Weight_in_gms + 
                 Warehouse_block.D +
                 Customer_care_calls.6 + 
                 Customer_care_calls.7 + 
                 Customer_rating.3 + 
                 Prior_purchases.4 + 
                 Prior_purchases.5 + 
                 Prior_purchases.6 + 
                 Product_importance.low + 
                 Product_importance.medium,
               family = "binomial",
               data = train)
summary(fit_glm)


############################### predictions 
pred_tr <- predict(fit_glm, newdata = train, type = "response")
pred_tr1 <- ifelse(pred_tr>0.6, 1, 0)
pred_tst <- predict(fit_glm, newdata = test, type = "response")
pred_tst1 <- ifelse(pred_tst>0.6,1,0)
pred_logistic <- predict(fit_glm, newdata = df_test, type = "response")
pred_logistic <- ifelse(pred_logistic>0.6, 1, 0)
############################## Accuracy 
library(caret)
pred_tr <- as.factor(pred_tr1)
Actual_tr <- as.factor(train$Reached.on.Time_Y.N)
Actual_test <- as.factor(test$Reached.on.Time_Y.N)

confusionMatrix(Actual_tr, as.factor(pred_tr1), positive = "0")
confusionMatrix(Actual_test, as.factor(pred_tst1), positive = "0")

install.packages("InformationValue")
library(InformationValue)
plotROC(actuals = Actual_tr, predictedScores = pred_tr1)
plotROC(actuals = Actual_test, predictedScores = pred_tst1)


######################### Random Forest #####################################

library(randomForest)
fit_rf <- randomForest(as.factor(Reached.on.Time_Y.N)~., data=train, do.trace =T)
importance(fit_rf)

pred_tr_rf <- predict(fit_rf)#, newdata = train)
pred_test_rf <- predict(fit_rf, newdata = test)
pred_random_forest <- predict(fit_rf, newdata = df_test)


confusionMatrix(Actual_tr, as.factor(pred_tr_rf), positive = "0")
confusionMatrix(Actual_test, as.factor(pred_test_rf), positive = "0")


plotROC(actuals = Actual_tr, predictedScores = pred_tr_rf)
plotROC(actuals = Actual_test, predictedScores = pred_test_rf)

######################## Support Vector Machine ################
 
library(e1071)
fit_svm <- svm(Reached.on.Time_Y.N ~., data=train, type = 'C-classification',
               kernel ="linear")

pred_tr_svm <- predict(fit_svm, newdata = train)
pred_test_svm <- predict(fit_svm, newdata = test)
pred_SVM <- predict(fit_svm, newdata = df_test)

confusionMatrix(Actual_tr, as.factor(pred_tr_svm), positive = "0")
confusionMatrix(Actual_test, as.factor(pred_test_svm), positive = "0")


plotROC(actuals = Actual_tr, predictedScores = pred_tr_svm)
plotROC(actuals = Actual_test, predictedScores = pred_test_svm)


####################### Boosting ######################
library(xgboost)
library(Matrix)
install.packages("cvAUC")
library(cvAUC)


train.mx <- sparse.model.matrix(Reached.on.Time_Y.N ~ ., train)
test.mx <- sparse.model.matrix(Reached.on.Time_Y.N ~ ., test)
dtrain <- xgb.DMatrix(train.mx, label = train$Reached.on.Time_Y.N)
dtest <- xgb.DMatrix(test.mx, label = test$Reached.on.Time_Y.N)

train.gdbt <- xgb.train(params = list(objective = "binary:logistic",
                                      #num_class = 2,
                                      eval_metric = "auc",
                                      eta = 0.3,
                                      max_depth = 1,
                                      subsample = 1,
                                      colsample_bytree = 0.5), 
                        data = dtrain, 
                        nrounds = 1000, 
                        watchlist = list(train = dtrain, test = dtest))

pred_tr_xgb <- predict(train.gdbt, newdata = dtrain)
pred_tr_xgb <- ifelse(pred_tr_xgb>0.5,1,0)
pred_test_xgb <- predict(train.gdbt, newdata = dtest)
pred_test_xgb <- ifelse(pred_test_xgb>0.5,1,0)

confusionMatrix(Actual_tr, as.factor(pred_tr_xgb), positive = "0")
confusionMatrix(Actual_test, as.factor(pred_test_xgb), positive = "0")

plotROC(actuals = Actual_tr, predictedScores = pred_tr_xgb)
plotROC(actuals = Actual_test, predictedScores = pred_test_svm)

############################### customer segmentation ######################
############################### Hierarchical Clustering #####################
attach(train_data)

cust_data <- train_data%>% 
  filter(train_data$Reached.on.Time_Y.N==1)

cust_data <- train_data[,c(1,6,10,11)]

##### scale the dataset
cust_data_f <- scale(cust_data)

##### disctance
dist.res = dist(cust_data_f, method = "euclidean")
hc <- hclust(dist.res, method = "complete")

######## cluster visualization
plot(hc, labels = F, hang = -1)
rect.hclust(hc, k=3, border = 2:3)

write.csv(pred_logistic, "prediction_test.csv")


################################# K-means Clustering #################
install.packages("vegan")
install.packages("permute")

library(vegan)
library(permute)
library(lattice)
fit <- cascadeKM(cust_data_f,1,10,iter = 100)
plot(fit, sortg=T, grpmts.plot = T)

calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


####### Elbow method
mydata <- cust_data
#Determine the optimal cluster size based on within sum of squares
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

#Plot the elbow chart to determine optimal cluster
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)

# From elbow chart it looks like 4 clusters
###Run the kmeans algorithm to generate the clusters
k1<-kmeans(cust_data_f, 4)

###Fetch the group means for each variable
k1$centers

###Fetch size/n of obs for the groups

k1$size

###Fetch the cluster for each obs
c <- k1$cluster

cust_data$cluster=k1$cluster
View(cust_data)

install.packages("factoextra")
library(factoextra)
fviz_cluster(k1, cust_data,palette = "Set2", ggtheme = theme_minimal())

################################### PAM clustering #######################
# library(fpc)
# 
# pam_clust <- pamk(cust_data_f)
# library(cluster)
# pam.result <- pam(cust_data_f, 3)
# table(pam.result$clustering)