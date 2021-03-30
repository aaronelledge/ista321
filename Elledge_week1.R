# Aaron Elledge
# ISTA 321
# Assignment 1
# 9/2/20

library(tidyverse)

bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")


# For this assignment you're going to do some simple exploration and manipulation of the 2018 Chicago DIVVY bikeshare data.  Each row represents a single bike rental.


# QUESTION - Use your data exploration techniques to look at both a summary AND a glimpse of bikes.

# ANSWER
summary(bikes)
glimpse(bikes)
# ANSWER There are 12 different variables for bikes with start_time, end_time, bikeid, from_station_id, from_station_name , to_station_id, 
# to_station_name, usertype, gender, birthyear, tourist, distance_,miles.  There are 6 numeric columns which are start time, bike id, from station id, to station id, birthyear, and distance miles.  The other 6 are character columns.  There is also 154,966 observations.  



# QUESTION - What are the two data types present? How many rows and columns are present? Pick two columns and describe what you find in them (e.g. summary stats, character levels, etc. )
#ANSWER
unique(bikes$tourist)
unique(bikes$birthyear)
# ANSWER The Tourist section tells us whether someone is a tourist or not
# The birthyear section gives us the birth years of the bikers



# QUESTION - How many unique values are present in the column bikeid?
#ANSWER
unique(bikes$bikeid)

# ANSWER there are 991 unique values present in the column bikeid




# QUESTION - Use some visual exploratory data analysis by making histograms of two of the numeric columns (your choice).  NOTE: Do one in base R and one using ggplot.
#ANSWER Histogram and ggplot created
hist(bikes$birthyear)
ggplot(bikes, 
aes(x=distance_miles))+geom_histogram()





# QUESTION - Use base R to slice the data frame several different ways. Specifically, to do ALL OF the following operations: Grab just a single row, a single column, a series of rows, a series of columns, a list of rows, and a list of columns.
bikes[8,]
bikes[,4]
bikes[4:10,]
bikes[,5,10]
bikes[c(10,15,20),]
bikes[,c(5,6,7)]
# ANSWER Data sliced 




# QUESTION - Use base R to extract JUST the columns usertype, gender, and distance_miles to a dataframe called bikes_1.  Do this again using tidyverse.bikes[,c('usertype','gender','distance_miles')]

#Extracted the columns usertype, gender, and distance_miles
bikes_1 <- bikes[,c('usertype','gender','distance_miles')]
bikes_1 <- bikes%>%
  select('usertype','gender','distance_miles')




# QUESTION - Do the same as above but using tidyverse.  Assign it to an object called bikes_2.

bikes_2 <- bikes%>%
  select('usertype','gender','distance_miles')


# QUESTION - Use base R to create a dataframe of just subscribers.  Call the resulting dataframe bikes_subs.
subscribers <- bikes[bikes$usertype == 'Subscriber',]
bikes_subs <- data.frame(subscribers)
bikes_subs




# Question - Now use tidyverse to make a dataframe of just subscribers.  Name it whatever you want.
subs_tidy <- bikes %>% filter(usertype == 'Subscriber')
subs_tidy




# QUESTION - Use tidyverse to create a dataframe of just Customers who are also Male  Name this data frame bikes_male_cust
bikes_male_cust <- bikes %>%
  filter(gender == 'Male' & usertype == 'Customer')
bikes_male_cust

# ANSWER dataframe created of only male customers




# QUESTION - What's the average distance ridden by male customers?
mean(bikes_male_cust$distance_miles, na.rm=TRUE)
# ANSWER 1.75784




# QUESTION - Birthyear isn't super useful right now.  Having actual rider age would be better.  Use either base R or tidyverse to create a new column that calculates the rider's age (these data were collected in 2018, btw).  After it's made explore your new column (using whatever method you'd like) to make sure it makes sense.
rider_age <- 2018 - bikes$birthyear
rider_age

# ANSWER Lists all riders age




# QUESTION - I'm guessing you see some strange values in your newly created age column.  Why don't you create a new dataframe called bikes_realages where it only contains riders who ages are less than or equal to some age that seems realistic to you.
bikes_realages <- rider_age 
rider_age
# ANSWER




# QUESTION - Make a histogram of rider ages using ggplot.  Based on this, what age range used the bikeshare the most?
hist(bikes$rider_age)
# ANSWER




# QUESTION - Some of these data types could or need to be changed.  There are three variables that are currently numeric but should be a factor.  What are they and why?
str(bikes)
# ANSWER Bikeid, From_station_id, and to_station_id are all numeric but should be factors because they are not 
# countable but are identifiers




# QUESTION - Now change those three variables in the bikes dataframe to factors

#ANSWER Dataframe is swtiched to factors

bikes$bikeid
bikeid = as.factor(bikes$bikeid)

from_station_id = as.factor(bikes$from_station_id)

to_station_id = as.factor(bikes$to_station_id)


# QUESTION - Use some method to figure out what was the most frequently used bike

# ANSWER The most frequently used bike is bikeid "6234" at 74 times used
bikes$bikesid <- as.factor(bikes$bikeid)
summary(bikes$bikesid)





# QUESTION - How many miles in total was the most frequently used bike ridden.  You're going to need to filter and then do a sum on a column.

# ANSWER
most_freq <- bikes_realages %>% filter(bikeid == '6235') %>% select(distance_miles)
sum(most_freq)





# QUESTION - What was the least used bike?  How many miles was it ridden?

#ANSWER 
bikes$bikesid <- as.factor(bikes$bikeid)
summary(bikes$bikeid)

least_used <- bikes$distance_miles
summary(least_used)



# QUESTION - How many rides in our data set were returned to the same station they were checked out from?
# Remember that you can do a logical comparison between values using ==.  You can then sum your true/false values!

#ANSWER 
bikes %>%
  filter(from_station_id == from_station_name)



# QUESTION - Use base R to select just the column distance_miles from bikes and assign it to an object called target_1

# ANSWER 
target_1 <- bikes$distance_miles





# QUESTION - Now do the same but use tidyverse's select() function.  Assign to target_2

# ANSWER

target_2 <- select(bikes, distance_miles)



# QUESTION - Now get the mean of target_1 and target_2 using mean().  Don't forget you might need to specify na.rm = TRUE as an argument in mean().

# ANSWER
mean(target_1, na.rm = TRUE)
# target_1, mean is 1.331433
mean(target_2, na.rm = TRUE)
# target_2 is NA

# QUESTION - Did you get a return for both?  Why or why not?  What happens when you run is.data.frame() on target_1 and then target_2?  What about is.vector()?  What is this telling you about the way in which both methods extracted the column?

# ANSWER 
# I got a return on Target_1, but not on Target_2
is.data.frame(target_1)
# Returns false
is.data.frame(target_2)
# Returns True

is.vector(target_1)
# Returns True
is.vector(target_2)
# Returns False

# In order to create a dataframe, it needs to be obtained as a vector in order to get your mean.



