# Name: Aaron Elledge
# Assignment: Week 3- Preprocessing
# Date: 9/22/20
# Class ISTA 321


##### Background
#The goal of this assignment is to apply all your preprocessing skills to two new sets of data.  You'll need to explore the datasets, identify the issues, and then fix them. You'll also have to select out the columns and rows you need, drop those you don't.  Finally, there's some imputation, one hot encoding, and creating of dummy variables.  


# You'll need both tidyverse AND the caret package.  Load them up!
library(tidyverse)
library(caret)

###### Dataset # 1 - LA parking tickets
# This dataset is a slimmed down version of the 5 million or so parking tickets given in LA in 2018!  There a bunch of real-life errors within that happen when you have thousands of people writing tickets.  

# bring in the data
parking <- read_csv("https://docs.google.com/spreadsheets/d/11ahddH6snm10AuxF51MlOISx2yCsJ2WmPKFdfRFpInU/gviz/tq?tqx=out:csv")


# QUESTION - Explore the dataset at the whole dataset level.  What columns might not be useful based off your exploration?
glimpse(parking)
summary(parking)
# ANSWER - Agency, Plate, and violation code possibly wouldn't be useful for this dataset



# QUESTION - Explore the individual columns within the dataset.  Specifically what levels are present within make, body style, and color?  Any issues that need to be fixed?  You might need to do some googling to figure out what some of these codes mean. 
summary(factor(parking$make))
summary(factor(parking$body_style))
summary(factor(parking$color))

unique(parking$make)
unique(parking$body_style)
unique(parking$color)

# ANSWER - The "Make" column contains 9 level, Mercedes is represented "Merz" & "Merc", Toyota is "Toyo" and "Toyt". 
#  There are 19 levels in body_style 
# There are 10 levels in color, Grey, white, and silver are represented in different abbreviations as well.





# QUESTION - Explore the agency, plate and violation_code columns. Are they useful?  Should you drop them?  If so, do it.

summary(factor(parking$agency))
parking <- parking %>% select(-agency)
glimpse(parking)

parking$violation_code <- parking$violation_code %>% str_remove("[[:punct:]]|\\+")

# ANSWER - agency column is dropped due to the fact it uses numerical values that we don't know what they represent. If we knew what they meant it would become useful.
# I also cleaned up violation_code, so it is easier to work with.


# QUESTION - based on your earlier exploration there are too many body styles.  Enter summary(factor(parking$body_style)) into your column to get a list of how many observations there are for each style.  Below filter your dataset so it contains only the top for most common body styles
summary(factor(parking$body_style))

body_sty<-c('PA','PU','TK','VN')
parking<-parking %>%
   filter(body_style %in% body_sty)
glimpse(body_sty)

# ANSWER - Most common body style




# QUESTION - When you looked at the unique values within the make column I hope you saw that there were two labels for two of the car brands (Toyota and Mercedes).  Use the summary(factor()) method like you did above to figure out which of the two in each brand is the wrong label.  Then use ifelse to correct it in this data frame.
parking$make <- ifelse(parking$make =='MERC','MERZ',parking$make)
parking$make <- ifelse(parking$make =='TOYO','TOYT',parking$make)
summary(factor(parking$make))


# ANSWER - MERC is corrected to MERZ and TOYO is corrected to TOYT




# QUESTION - Colors have some similar errors in labels such as there being both GR and GY for grey.  Do what you did above and correct the three errors in color. 
parking$color <- ifelse(parking$color == 'GR','GY',parking$color)
parking$color <- ifelse(parking$color == 'SI','SL',parking$color)
parking$color <- ifelse(parking$color == 'WH','WT',parking$color)
summary(factor(parking$color))

# ANSWER - GR is corrected to GY, SI is corrected to SL, WH is corrected to WT




# QUESTION - Our fine column has several issues.  First, there is the $ sign in the number which prevents us from using it as a numeric column.  Remove the $ from all numbers. After removal convert the column to numeric. Next, there are NA values in the data frame.  Use whatever method you like to verify that there are NA values.  Then use an ifelse statement to median impute these missing values. 
parking$fine <- parking$fine %>%
  str_remove('[$]') %>%
  as.numeric()
head(parking$fine,10)
parking$fine<- ifelse(is.na(parking$fine),median(parking$fine,na.rm=TRUE),parking$fine)

w# ANSWER - $'s are removed, column is converted numeric, as well as NA is verified in the set





# QUESTION - The various levels in the violation column are a mess.  Replace all spaces with an underscore '_'.  Replace all forward slashes with a '-'.  Remove all periods.
parking$violation<-parking$violation %>%
  str_replace_all('\\s','_')%>%
  str_replace_all('/','-')%>%
  str_remove('[.]')
summary(factor(parking$violation))

# ANSWER - All spaces are replaced with an underscore, all forward slashes are replaced with a dash, and all periods are removed.







###########################################################

# Now for part two of our assignment - preprocessing our insurance data.

# In this dataset our ultimate goal is to predict insurance costs based on the other features.  Thus the target is the charges column.

# bring in data
costs <- read_csv("https://docs.google.com/spreadsheets/d/1WUD22BH836zFaNp7RM5YlNVgSLzo6E_t-sznxzjVf9E/gviz/tq?tqx=out:csv")

# QUESTION - Explore the whole dataset quickly
summary(costs)
glimpse(costs)
# ANSWER - Rows: 1,338, columns: 8 (age, sex, bmi, children, smoker, region, charges, bodyfat)




# QUESTION - Remember from our earlier lesson that bodyfat is highly collinear with BMI.  Drop bodyfat from out dataframe.
cost<-costs %>%
  select(-bodyfat)
glimpse(costs)

# ANSWER - Bodyfat is dropped from dataframe




# QUESTION - How many levels are present in the region column?  After exploring that, use the dummyVars function to one hot encode everything.  Remember our target is charges and will be dropped after you create the dummy variables, so you'll have to remember to join that back on.  If you're struggling look back at the tutorial!
summary(factor(costs$region))
my_dummies <- dummyVars(~age+sex+bmi+children+smoker+region+charges, data=costs, fullRank=TRUE)
my_dummies_pred <- predict(my_dummies, newdata=costs)
costs<-merge(costs,my_dummies_pred)

# ANSWER - there are 4 regions




# QUESTION - Maybe all that matters is if the individual has kids or not, and not how many kids they have.  Make a binary feature for children and call it children_b in your dataframe.  Drop the original children column afterwards.
costs$children_bin <- ifelse(costs$children > 0,1,0)
costs <- costs %>%
  select(-children)
glimpse(costs)

# ANSWER- Binary Feature is created




# QUESTION - Both age and bmi need to be scaled and centered.  Scale and center each as was done in the RPubs and assign back to their original columns
costs$bmi <- scale(costs$bmi)
hist(costs$bmi)

costs$age <- scale(costs$age)
hist(costs$age)

glimpse(costs)

# ANSWER - Back to original columns

summary(costs$charges)


# QUESTION - Make a linear regression model with charges as your target and all the other features as your predictors.  What region has the lowest healthcare costs?  How much does having children influence insurance costs?  Given we scaled and centered age and bmi, which one has a bigger effect on costs for a single SD increase in the respective feature?
summary(factor(costs$charges))<- lm(charges~region+regionnorthwest+regionsoutheast+regionsouthwest,data=costs)
summary(costs)


# ANSWER - The southeast region has the lowest cost for healthcare. Having children raises the cost of health care and age has a higher effect on healthcare charges.




# QUESTION - Make a figure showing how age or bmi impacts insurance costs.
ggplot(costs,
        aes(x=age, y=charges)) + 
  geom_point()

# ANSWER - can notice the costs increase as age increases



