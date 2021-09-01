##### Project - College Admission #####

getwd()  ## to see the current working directory ##

setwd("C:/Users/intel/Desktop/Simplilearn") ## setting the working directory ##

### First we will read the data from the csv file ###

college <- read.csv("College_admission.csv")

View(college)  ## view of the dataset

dim(college) ## gives the count of rows and columns ##

str(college) ## structure of the data for better interpretation ##

## 1 Find the missing values. (if any, perform missing value treatment) ##

sum(is.na(college))

## There are no missing values in the dataset ##

## 2 Find outliers (if any, then perform outlier treatment) ##

library(tidyverse) ## loading required package for data visualization ##

### checking for outliers in gre column using boxplot ###

gre_boxplot <- ggplot(data = college, mapping = aes(x = gre), horizontal = T) +
  geom_boxplot(fill = "blue", colour = "black") +
  ggtitle("Boxplot of gre")

gre_boxplot

## Also we can use the graphics below ##

gre_boxplot1 <- boxplot(college$gre,
                        main = "boxplot of gre",
                        xlab = "gre",
                        col = "green",
                        horizontal = T)

gre_boxplot1

gre_boxplot1$out

### So from above visualization we can see that there are some outliers present ##

## Also the output of gre_boxplot1$out shows there are 4 outliers in gre ##

## We will find the outliers using the IQR Method ##

gre_iqr <- IQR(college$gre)

gre_iqr

Q1 <- quantile(college$gre, 0.25) # to get 25 % value

Q1

Q3 <- quantile(college$gre, 0.75) # to get 75% value

Q3

quantile(college$gre) # alternate method to view all quantiles

gre_max <- Q3 + 1.5*gre_iqr

gre_max

gre_min <- Q1 - 1.5*gre_iqr

gre_min

## let's see which are the values above gre_max and below gre_min ##

which(college$gre > gre_max) 

## no outlier found ##

which(college$gre < gre_min)

## 4 outlier found in the rows - 72 180 305 316 ##

## checking for outliers in gpa column ##

gpa_boxplot <- ggplot(data = college, mapping = aes(x = gpa), horizontal = T) +
  geom_boxplot(fill = "light blue", colour = "black") +
  ggtitle("Boxplot of gpa")

gpa_boxplot

## Alternate Method of plotting ##

gpa_boxplot1 <- boxplot(college$gpa,
                        main = "boxplot of gpa",
                        xlab = "gpa",
                        col = "grey",
                        horizontal = T)

gpa_boxplot1

gpa_boxplot1$out

### So from above visualization we can see that there are some outliers present ##

## Also the output of gpa_boxplot1$out shows there is only 1 outlier in gpa ##

## We will find the outliers using the IQR Method ##

gpa_iqr <- IQR(college$gpa)

gpa_iqr

Q1.1 <- quantile(college$gpa, 0.25) # to get 25 % value

Q1.1

Q3.1 <- quantile(college$gpa, 0.75) # to get 75 % value

Q3.1

quantile(college$gpa) # alternate method to view all quantiles

gpa_max <- Q3.1 + 1.5*gpa_iqr

gpa_max

gpa_min <- Q1.1 - 1.5*gpa_iqr

gpa_min

## let's see which are the values above gpa_max and below gpa_min ##

which(college$gpa > gpa_max) 

## no outlier found ##

which(college$gpa < gpa_min)

## 1 outlier found in the row - 290 ##

## Now from above we found that there are outliers present in rows - 72 180 305 316 290 ##

## Let's remove these outliers ##

college <- college[-c(72, 180, 305, 316, 290),] ## removing rows containing outliers ##

View(college)

dim(college)

### Now we can see that the outliers are removed ##

## 3 Find the structure of the data set and if required, transform the numeric data type to factor and vice-versa ##

str(college)

## from str we will convert admit, ses, Gender_Male, Race, rank into factors for further analysis ##

college$admit <- as.factor(college$admit)

levels(college$admit)

college$ses <- as.factor(college$ses)

levels(college$ses)

college$Gender_Male <- as.factor(college$Gender_Male)

levels(college$Gender_Male)

college$Race <- as.factor(college$Race)

levels(college$Race)

college$rank <- as.factor(college$rank)

levels(college$rank)

str(college)

summary(college)

## So here summary will give more details about college data ##

## 4 Find whether the data is normally distributed or not. Use the plot to determine the same ##

## Here we will use KERNEL DENSITY PLOT to see how data is distributed ##

density_gre <- density(college$gre)

density_gre

plot(density_gre,
     main = "Kernel Density Plot of gre")
polygon(density_gre, col="brown", border="black")

## gre is almost normally distributed ##

density_gpa <- density(college$gpa)

density_gpa

plot(density_gpa,
     main = "Kernel Density Plot of gpa")
polygon(density_gpa, col="blue", border="black")

## gpa is somewhat near to normally distributed ##

library(moments)

skewness(college$gre, na.rm = T)

skewness(college$gpa, na.rm = T)

mean(college$gre)

mean(college$gpa)

## from above visualization we can see that data is not perfectly normally distributed but it is little skewed (-ve skewed) which we can ignore as skewness = 0 refers to normal distribution and values do not deviate drastically from 0 ##

## 5 Use variable reduction techniques to identify significant variables ##
## 6 Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables) ##

library(caTools)

## splitting the data into train and test ##

set.seed(100)

college[,2:3] <- scale(college[,2:3]) ## scaling the data for analysis ##

indices <- sample.split(college$admit, SplitRatio = 0.70)

train <- subset(college, indices == TRUE)

test <- subset(college, indices == FALSE)

dim(train) 

dim(test)

## Now we will build a Logistic Regression Model ##

model_lg <- glm(formula = admit ~ ., data = train, family = "binomial")

summary(model_lg)

## From above summary we can see that only rank and gpa are significant variables ##
## So we will drop insignificant variables - gre,ses,Gender_Male,Race and build another model ##

model_lg1 <- glm(formula = admit ~ gpa+rank, data = train, family = "binomial")

summary(model_lg1)

## Residual deviation increases in model_lg1 so we will use model_lg for prediction ##

predict_test <- predict(model_lg, type = "response", newdata = test[,-1])

predict_test

test$pred <- predict_test

View(test)

## Let's use the probability cutoff of 50% ##

test$pred <- factor(ifelse(predict_test >= 0.50,1,0))

View(test)

## 7 Calculate the accuracy of the model and run validation techniques ##

## creating confusion matrix ##

library(caret)

confusion_matrix <- confusionMatrix(test$pred, test$admit, positive = "1") 

confusion_matrix

## So the accuracy of logistic regression is 0.6723 i.e 67.23% ##

## Now we will run Repeated K-fold cross-validation ##

# Define training control #

set.seed(123)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model

model <- train(admit ~., data = train, method = "glm",
               trControl = train.control)

summary(model)

## Here we also found that the rank and gpa are significant variables for college admission ##

predicted <- predict(model, newdata = test)

summary(predicted)

confusion_matrix_1 <- confusionMatrix(predicted, test$admit, positive = "1")

confusion_matrix_1

## After performing validation techniques we got same results with accuracy 67.23% ##
## Analysis Tasks: Analyze the historical data and determine the key drivers for admission ##
## After above analysis we found that gpa and rank are the key drivers for admission ##

## 8 Try other modelling techniques like decision tree and SVM and select a champion model ##
## 9 Determine the accuracy rates for each kind of model ##

## SVM model with C = 1 ##

library(kernlab)
library(readr)

svm_model <- ksvm(admit ~ ., data = train, scale = T, C = 1)

## Predicting the model results ##

svm_result <- predict(svm_model, newdata = test)

svm_conf_mat <- confusionMatrix(svm_result, test$admit, positive = "1")

svm_conf_mat

## Here the accuracy is 67.23% ##

## using the train function to get the optimum value of C ##

set.seed(100)

trainControl1 <- trainControl(method="cv", number=15)

grid <- expand.grid(C=seq(1, 10, by=1))
grid

## Performing 15-fold cross validation ##

svm_model1 <- train(admit ~., data = train, method="svmLinear", metric="Accuracy", 
                  tuneGrid=grid, trControl=trainControl1)

svm_model1

svm_result1 <- predict(svm_model1, newdata = test)

svm_conf_mat_1 <- confusionMatrix(svm_result1, test$admit, positive = "1")

svm_conf_mat_1

## Here we can see that the accuracy is slightly improved and it is 68.07% ##

## Decision Tree ##

library(rpart)
library(rpart.plot)
library(caret)

tree.model <- rpart(formula = admit ~ ., data = train, method = "class")

## display decision tree ##

prp(tree.model)

summary(tree.model)

## make predictions on the test set ##

tree.predict <- predict(tree.model, test, type = "class")

tree.predict

dt_conf_mat <- confusionMatrix(tree.predict, test$admit, positive = "1")

dt_conf_mat

## Here the accuracy is 61.34% for decision tree ##

## 10 Select the most accurate model ##

## from above analysis we found that SVM and logistic regression are the best models till now ##

## 11 Identify other Machine learning or statistical techniques ##

## KNN Model ##

library(caret)
library(pROC)
library(mlbench)
library(caTools)

## we will again read the same file in college variable as there are some changes happened in the previous analysis ##

college <- read.csv("College_admission.csv")

college <- college[-c(72, 180, 305, 316, 290),] ## removing outliers we found earlier ##

college$admit[college$admit == 0] <- "NO"

college$admit[college$admit == 1] <- "YES"

## converting some columns into factors for analysis purpose ##

college$admit <- as.factor(college$admit)

college$ses <- as.factor(college$ses)

college$Gender_Male <- as.factor(college$Gender_Male)

college$Race <- as.factor(college$Race)

college$rank <- as.factor(college$rank)

view(college)

str(college)

summary(college)

college[,2:3] <- scale(college[,2:3]) ## scaling the data for analysis ##

## Data split ##

set.seed(100)

indices <- sample.split(college$admit, SplitRatio = 0.70)

train <- subset(college, indices == TRUE)

test <- subset(college, indices == FALSE)

dim(train)

dim(test)

## Building KNN Model ##

parameters <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


set.seed(222)

knn_model <- train(admit ~ .,
             data = train,
             method = 'knn',
             trControl = parameters,
             preProc = c("center", "scale"),
             metric = "ROC",
             tuneGrid = expand.grid(k = seq(1,60,2)))
knn_model

varImp(knn_model)

## Here we also found that rank and gpa are most significant variables ##

knn_pred <- predict(knn_model, newdata = test)

knn_conf_mat <- confusionMatrix(knn_pred, test$admit, positive = "YES")

knn_conf_mat

## The accuracy of KNN model is 67.23% however the sensitivity is very low compared to SVM and LR models##

## Naive Bayes Model ##

library(naivebayes)

## we will again read the same file in college variable as there are some changes happened in the previous analysis ##

college <- read.csv("College_admission.csv")

college <- college[-c(72, 180, 305, 316, 290),] ## removing outliers we found earlier ##

view(college)

str(college)

## converting some columns into factors for analysis purpose ##

college$admit <- as.factor(college$admit)

college$ses <- as.factor(college$ses)

college$Gender_Male <- as.factor(college$Gender_Male)

college$Race <- as.factor(college$Race)

college$rank <- as.factor(college$rank)

str(college)

summary(college)

college[,2:3] <- scale(college[,2:3]) ## scaling the data for analysis ##

## Data split ##

set.seed(100)

indices <- sample.split(college$admit, SplitRatio = 0.70)

train <- subset(college, indices == TRUE)

test <- subset(college, indices == FALSE)

dim(train)

dim(test)

## Building Naive Bayes Model ##

trControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)

set.seed(222)

nb_model <- train(admit ~ .,data = train,method = 'nb', trControl=trControl)

nb_pred <- predict(nb_model, newdata = test)

nb_pred

nb_conf_mat <- confusionMatrix(nb_pred, test$admit, positive = "1")

nb_conf_mat

## Here the accuracy of Naive Bayes Model is 66.39% however the sensitivity is 0 ##

## So after all the above analysis we found that accuracy and sensitivity is good only for SVM and Logistic Regression model compared to others so these two are the champion model ##

## Descriptive : Categorize the average of grade point into High, Medium, and Low (with admission probability percentages) and plot it on a point chart ##

View(college)

## Here we see that the original gre data has been scaled according to gpa ##
## so we will again read the data from csv to perform the descriptive analysis to new variable college_data ##

college_data <- read.csv("College_admission.csv")

college_data <- college_data[-c(72, 180, 305, 316, 290),] ## removing outliers 

str(college_data)

## we will use transform function to group data into Low, Medium, High according to gre score ##

grade_point <- transform(college_data,grade = ifelse(gre <= 440,"Low",ifelse(gre <= 580,"Medium","High")))

options(max.print = 99999) ## to print all rows in data ##

grade_point

## Now we will use aggregate function to calculate admissions according to grade ##

grade_sum <- aggregate(admit~grade, grade_point, FUN = sum) 

grade_sum

grade_length <- aggregate(admit~grade, grade_point, FUN = length) 

grade_length

data_table <- cbind(grade_sum, total_admit = grade_length[,2])

data_table

admission_percent <- (grade_sum$admit / grade_length$admit)*100

admission_percent

## Here admit shows actual admissions according to grade and total_admit shows total according to grade ##

Admission_table <- cbind(data_table,admission_percent)

Admission_table

## Creating a Point Chart ##

Admission_Point_Chart <- ggplot(data = Admission_table) +
  geom_point(mapping = aes(x = grade, y = admission_percent, color = grade)) +
  ggtitle("Admission Percentage according to grade levels")

Admission_Point_Chart
