# Aaron Elledge
# Assignment Week 2: Linear Regression
# Date 9/10/2020
# Class ISTA 321


#### BACKGROUND
# this week's assignment revolves around understanding how weather conditions influence the number of bike rides in a given day. This is real 2018 data from Chicago's DIVVY bikeshare system that I've connected to  2018 weather data.  

# you have the following relevant columns:
# number_rides - number of rides in a given day
# AWND - Average wind speed in the day
# TAVG - Average temperature in Fahrenheit
# SNOW - Total snowfall in the day
# PRCP - Total rainfall in the day

# load libraries 
library(tidyverse)

# load data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1DK8ZSmIgvZ1eVVF33NCNLyLxvYFA8t1jeGrJppaBDpI/gviz/tq?tqx=out:csv")

############## EDA

# QUESTION - Explore your data with a summary.  In a few sentences describe the data, if you think they make sense, and why that is.  

# ANSWER - This data set shows 365 rows, and 11 columns. I believe this data makes sense,
# if shows the days the bike was ridden, the number of rides as well as the the average trip time and distance with various
#  with various elements coming into play such as rain and snow.Ran more specific rows to see size numbers were showing up
summary(bikes)
glimpse(bikes)
unique(bikes$SNOW)
unique(bikes$number_rides)

# QUESTION - Make some histograms to understand the distribution of the columns mean_trip_time and mean_distance.  Describe a few features you see in each figure?

# ANSWER - I noticed when running a histogram of the trip time for the bikes it is skewed very left, with most numbers
# being be between the 0 to 20 for times. 
# The histogram with the average distance for bikes, is skewed center, with the majority of times being in the .9 range to the 1.5
hist(bikes$mean_trip_time)
hist(bikes$mean_distance)

ggplot(bikes,
       aes(x = mean_trip_time)) +
  geom_histogram()

ggplot(bikes,
       aes(x = mean_distance)) +
  geom_histogram()


# QUESTION - Use a method to figure out the weather conditions on the day that had the most rides.  Also, what date did this occur on? 

# ANSWER - The date with the most rides occured on (2018-07-28) with 20585 rides with a temp. average of 71 with no snow or rainfall
most_rides <- bikes %>% select(day, number_rides, TAVG, PRCP, SNOW) %>% filter(number_rides == max (number_rides))
glimpse(most_rides)

############## Linear regression

# QUESTION -  fit a linear regression model using TAVG as a feature to explain your target, number_rides.  Remember the format is lm(targer ~ feature, data = ...)

# ANSWER - 
rides_model <- lm(number_rides ~ TAVG, data = bikes)

# QUESTION - How much variation is being explained in your target? Please use code to get the answer from the summary and store it in an object. CAll that object vs. just writing out the answer.  

# ANSWER - .7859 is the variation

rides_model_summary<-summary(rides_model)
rides_model_summary$r.squared




# QUESTION - Calculate the confidence interval for B1 - Do it in a way that works if model structure or data gets added to or removed!  You can use the model coefficients from the model summary vs. calculating from scratch.

# ANSWER - 237.44 is the lower bound, and 264.96 is the upper bound of the confidence interval

rides_model_summary$coefficients
SE_B1<-rides_model_summary$coefficients[4]
B1_Est<-rides_model_summary$coefficients[2]
b1_upper<-round(B1_Est+2*SE_B1,2)
b1_lower<-round(B1_Est-2*SE_B1,2)



# QUESTION - Interpret your B1 coefficient in 'plain English.'

# ANSWER -  Essentially, we are saying we are 95% confident that the number of rides a day will be between 237.44 and 264.96



# QUESTION - Calculate the predicted number of rides if the average temperature is 68 degrees

# ANSWER - The predicted number of rides if the average temp. is 68 degrees is 14112.17

num_rides_68<-rides_model_summary$coefficients[1]+B1_Est*68+SE_B1



# QUESTION - Make a figure showing the relationship between TAVG and number_rides. This is two continuous variables which should tell you what type of plot you need.  You can then make a vector of x-values.  Then extract the coefficients from the model to predict y.  You then have all you need to add a geom_line() to your plot!  Also, I know that ggplot can fit this line for you, but please don't do that.  The goal is to demonstrate you understand the different parts of a regression model.

# ANSWER - 

ggplot(bikes,
       aes(x=TAVG,y=number_rides))+geom_point()
x_vals<-seq(from=min(bikes$TAVG),to=max(bikes$TAVG),length.out=nrow(bikes))
y_preds<-rides_model_summary$coefficients[1]+B1_Est*x_vals

ggplot(bikes,
+       aes(x=TAVG,y=number_rides))+geom_point()+geom_line(aes(x=x_vals,y=y_preds))


############## Comparing two linear regression models

# QUESTION - Fit another regression model using AWND as a feature but leave number_rides as the target

# ANSWER - 
rides_AWND<-lm(number_rides~AWND,data=bikes)


# QUESTION - Which is a better model, this one or the first one you fit?  Use two pieces of evidence from the model summaries to justify your answer.

# ANSWER - R^2 = .0947, confidence interval is -349.61 and -685.51.  The better model would be the TAVG model because R^2 was better and it had a much smaller range for its confidence interval.
rides_AWND<-lm(number_rides~AWND,data=bikes)
rides_AWND_summary<-summary(rides_AWND)
rides_AWND_summary$r.squared
Se_B1_AWND<-rides_AWND_summary$coefficients[4]
B1_AWND<-rides_AWND_summary$coefficients[2]
b1_upper_AWND<-round(B1_AWND+2*Se_B1_AWND,2)
b1_lower_AWND<-round(B1_AWND-2*Se_B1_AWND,2)

############## Multiple regression

# QUESTION -  fit a multiple regression model with number of rides as the target and then AWND, PRCP, SNOW, and TAVG as features.  Remember, multiple regression models have all these features in a single model.  

# ANSWER - 
num_rides_model<-lm(number_rides~AWND+PRCP+SNOW+TAVG, data=bikes)
summary(num_rides_model)


# QUESTION - How much extra variation did you explain by including these other features compared to just the simple linear model with only TAVG as a feature? 

# ANSWER -It added an extra .7583 variation to the AWND model and an extra .0671 variation to the TAVG model.




# QUESTION - Were any of the additional features not important?  Use two pieces of evidence to justify your answer.


# ANSWER - Snow: R^2= .07386, Pvalue= 1.336e^-07, Prcp: R^2=.0107, Pvalue= .0475.  PRCP was not an important parameter considering it had the lowest R^2 value and had a PValue closest to .05.

num_rides_snow<-lm(number_rides~SNOW,data=bikes)
num_rides_snow_sum<-summary(num_rides_snow)
num_rides_snow_sum$r.squared
num_rides_prcp<-lm(number_rides~PRCP,data=bikes)
num_rides_prcp_sum<-summary(num_rides_prcp)
num_rides_prcp_sum$r.squared


############# Multiple regression - interaction models

# finally, we're going to fit an interaction model
# before that, run this line of code that creates a binary snow feature so that it's just a 1 if there's any snow that day, and a zero if there's not
bikes$SNOW_B <- as.factor(ifelse(bikes$SNOW > 0, 1, 0))


# QUESTION - fit an interaction model between TAVG and SNOW_B

# ANSWER - 
bikes_TAVG_snow_model<-lm(number_rides~TAVG*SNOW_B,data=bikes)



# QUESTION - Interaction models are hard to interpret, so make a plot with two fit lines instead.  Remember, make a sequence of X values, then use these to estimate the Y values... one Y vector for if it snowed, the other if it didn't.  Again, do this from scratch and not using ggplot to fit the line itself. 

# ANSWER - 
bikes_n<-nrow(bikes)
TAVG_seq<- seq(min(bikes$TAVG),max(bikes$TAVG),length.out=bikes_n)
t_model<-summary(bikes_TAVG_snow_model)
y_snow <- t_model$coefficients[1] + t_model$coefficients[3,1] * TAVG_seq + 
  +     t_model$coefficients[2,1] * 1 + 
  +     t_model$coefficients[4,1] * TAVG_seq * 1
y_nosnow <- t_model$coefficients[1] + t_model$coefficients[3,1] * TAVG_seq + t_model$coefficients[2,1] * 0 + t_model$coefficients[4,1] * TAVG_seq * 0

ggplot(bikes,
      aes(x=TAVG,y=number_rides))+
      geom_point(aes(color=SNOW_B))+geom_line(aes(x=TAVG_seq,y=y_snow))+geom_line(aes(x=TAVG_seq,y=y_nosnow))



# QUESTION -  Based on the plot you created, interpret how snow and temperature interact to influence the number of rides. In other words, how does it snowing or not influence the relationship between temperature and the number of rides in a day?

# ANSWER - Snowing and temperature have a direct influence on number of rides with snow decreasing the amount of rides and temperature increasing the rides as the temperature rises.




############ ONE LAST QUESTION

# QUESTION - Make a model to determine how average temperature influences the average age of the rider on a given day.

# ANSWER - Model

ggplot(bikes,aes(x = mean_rider_age, y = TAVG)) + geom_point() + geom_smooth(method = 'lm')

# QUESTION - Given this model, what can you say about how temperature influences the age demographics of the bikeshare users? 

# ANSWER - When the average temp. is about 75 degrees the user tends to be 35 years old. 
# The lower average temp. between 30 and 40 degrees has users at 38 years old. With a downward slope
# it indicates when it gets colder people still use bikes, however the users get older.




