# Aaron Elledge
# 10/2/2020
# ISTA 321

# Classification homework - the goal of this assignment is to predict if someone has 'good' or 'bad' credit.  You're going to use both a logistic regression model as well as a KNN model to try and best predict credit class.  

# NOTE - People get most tripped up at the end in either making sure their predictions are outputted as classes (or converted to them) and comparing them to the true test targets. 

# Load packages
library(tidyverse)
library(caret)
library(MuMIn)

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")
credit_df <- credit


# QUESTION - As always, take some time to explore your data.  What levels are present in the character columns?  
# ANSWER -
glimpse(credit_df)
unique(credit_df$checking_status) 
#4 Levels
unique(credit_df$credit_history) 
#5 Levels
unique(credit_df$purpose) 
#10 Levels
unique(credit_df$savings_status) 
#5 Levels
unique(credit_df$employment) 
#5 Levels
unique(credit_df$personal_status) 
#4 Levels
unique(credit_df$other_parties) 
#3 Levels
unique(credit_df$property_magnitude) 
#4 Levels
unique(credit_df$other_payment_plans) 
#3 Levels
unique(credit_df$housing) 
#3 Levels
unique(credit_df$job) 
#4 Levels
unique(credit_df$own_telephone) 
#2 Levels
unique(credit_df$foreign_worker) 
#2 Levels
unique(credit_df$class) 
#2 Levels


# QUESTION - Based on your exploration, pick two columns and describe how you think they might be related to credit score.  What levels within these features will have what effect on the target
# Class and Credit history would both be good for credit score, credit history helps make a credit score and would be good. As well as, class showing if it's good or bad


# QUESTION - Start by creating your dummy variables.  

# ANSWER - 
credit_dummy <- dummyVars(class ~ ., data = credit_df, fullRank = TRUE)
credit_df <- predict(credit_dummy, newdata = credit_df)
credit_df <- data.frame(credit_df)
glimpse(credit_df)




# QUESTION - you need to convert your target 'class' to a binary factor.  Make it so that if the value is 'good' that it's replaced with a 1, and if it's 'bad' it's replaced with a 0.  Then convert that to a factor. Do this all while overwriting the original 'class' column so that you don't have two targets. 

# ANSWER - 
class_vals <- credit %>% select(class)
credit_df <- cbind(credit_df, class_vals)
glimpse(credit_df)

credit_df$class <-  factor(ifelse(credit_df$class == 'good', 1, 0))
glimpse(credit_df)
colnames(credit_df) <- colnames(credit_df) %>% str_replace_all('\\.+', '_') %>% str_replace_all('_$', '')



# QUESTION - Now split your data into train and test features and targets.  Use an 80/20 train/test split.


# ANSWER - 
set.seed(888) # run this first and then put your answer below it. 
credit_80split <- createDataPartition(credit_df$class, p = 0.8, list = FALSE)
head(credit_80split, 10)

features_train <- credit_df[ credit_80split, !(names(credit_df) %in% c('class'))]
features_test  <- credit_df[-credit_80split, !(names(credit_df) %in% c('class'))]

target_train <- credit_df[ credit_80split, "class"]
target_test <- credit_df[-credit_80split, "class"]




# QUESTION - Take a second to verify that your targets and features contain the proper data.  Check the number of rows in them.  Check to make sure the proper columns are in them as well!

# ANSWER - 
head(features_train, 10)
head(features_test, 10)
head(target_train, 10)
head(target_test, 10)




# QUESTION - On to preprocessing.  Preprocess your data using centering, scaling, and the knnImpute within method.  

# ANSWER - 
preprocess_object <- preProcess(features_train, method = c('center', 'scale', 'knnImpute'))
preprocess_object

features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)




# QUESTION - Why do you get your preprocessing object from only your training data? 

# ANSWER - Data leakage! Essentially acting as if your data is blind, having no pre-knowledge of anthing prior



# QUESTION - Use the formula from the lesson/book to calculate your k value before we fit our kNN model.  Remember to round to an odd value.

# ANSWER - is 14.14214, rounding down to 13
k_form <- sqrt(length(target_test))
k_form 





# QUESTION - Fit a kNN model on your training data 

# ANSWER - 

knn_fit <- knn3(features_train, target_train, k = 9)
knn_fit 




# QUESTION - Now use that to predict the classes of your test data

# ANSWER - 
knn_pred <- predict(knn_fit, features_test, type = 'class' )
head(knn_pred, 10)




# QUESTION - Make a predictions data frame with your true target values and your knn_pred values

# ANSWER - Prediciton data frame built with target values & knn_pred values
predictions <- cbind(data.frame(target_test, knn_pred))
summary(predictions)




# QUESTION - Now fit a logistic regression.  Remember you have to join your features and target back together to train your model. You'll also have to rename your target back to 'class'

# ANSWER - Logistic regression fit
full_train <- cbind(features_train, target_train)
glimpse(full_train)
full_train <- full_train %>% rename(class = target_train)
log_train <- glm(class ~ ., family = 'binomial', data = full_train)




# QUESTION - Check out a summary of your logistic regression model.  Are all features important?  Do the ones you made predictions about earlier pan out?

summary(log_train)

# ANSWER - Credit column did not pan out. Credit history panned out, and is important





# QUESTION - Generate your predictions for your test data.  Be sure to look at the data and convert the values to classes if needed.

# ANSWER - 
log_pred <- predict(log_train, newdata = features_test, type = 'response')
head(log_pred)
log_pred <- ifelse(log_pred >= 0.5, 1, 0)




# QUESTION - Add these logistic regression predictions to your predictions data frame as a new column

# ANSWER - Data frame is a new column
predictions$log_pred <- factor(log_pred)
summary(predictions)




# QUESTION - Calculate error rates between our true test values and the predicted values from both models.  Which model did best?


# ANSWER - 
predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred, 1, 0)
predictions$log_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)




# QUESTION - Make confusion matrices for both models.  Which model had more true positives?  Which had more true negatives?   

# ANSWER - True Positives130
# True Negatives 16

knn_conf <- confusionMatrix(predictions$knn_pred, predictions$target_test)
knn_conf$table
# True positives 120
# True negatives 29
log_conf <- confusionMatrix(predictions$log_pred, predictions$target_test)
log_conf$table


# QUESTION - Go dial k back to 9 and then rerun the script.  Did that improve model fit?

# ANSWER - The model improved.




# QUESTION - What do you think of this in the end?  Is using these models better than naively predicting who had good or bad credit?  Are the as accurate as you would have thought?  Any thoughts on what we could do to improve fit? 

# ANSWER - They are good enough to predict good or bad credit. The data set have very broad ranges of values. Features that would improve the fit of accurately predicting good or bad credit.


