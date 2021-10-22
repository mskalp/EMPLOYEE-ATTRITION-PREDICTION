#Load data
install.packages('rio') #1
library(rio)
install.packages('mice') #2
library(mice)
install.packages('skimr') #3
library(skimr)
install.packages('missMDA') #4
library(missMDA)
install.packages('tidyverse') #5
library(tidyverse) #stringr ggplot2 dplyr
install.packages("summarytools")
library(summarytools)
install.packages("Rcpp")
library(Rcpp)
install.packages("ROCR")
library(ROCR)
#install.packages("DMwR")
#library(DMwR)

install.packages("caret")
library(caret)
#Read input file
link = 'outfile2.csv'
df <- import(link)
#Convert all categorical variables to factor
cols <- c("target","relevent_experience","enrolled_university",
          "education_level","major_discipline","experience",
          "company_size","company_type","last_new_job","gender")
df[cols] <- lapply(df[cols], factor)  ## as.factor() could also be used


#0. Fixing the initial randomization
set.seed(100)

#1. Split the data into training and testing
train_positions <- createDataPartition(y = df$target, #Target
                                       p = .8,        #Training %
                                       list = F)     #Avoid a list output

training <- df[train_positions,]
testing <- df[-train_positions,]

#2. Cross-validation and model tuning options
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random',
                          sampling = 'smote') #to account for unbalanced


#3. Fit an algorithm to your data
train(target ~ .,
      data       = training,
      method     = 'glm',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit

#4. Final model
#model_fit$finalModel

#plot(model_fit$finalModel)

summary(model_fit)

#5. Model performance
#Predictions for testing values
testing_pred <- predict(model_fit, testing) 
testing_pred <- as.data.frame(testing_pred)

#Checking performance
postResample(testing_pred, testing$target)

#  Accuracy     Kappa 
#0.7444531 0.3653912 

#6. Variable Importance for training
varImp(model_fit, scale = T)
plot(varImp(model_fit, scale = T))

###########################################################
