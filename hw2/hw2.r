# HW 2
# Graphics Skills
# Due Thursday February 12th by midnight 
# This .R file should contain your code and answers to questions

# The goals of this assignment are to: 
# 1. Become familiar with the variety of plotting functions available in R,
# 2. Learn which types of plots are appropriate for which types of data
# 3. Gain practice in making plots that make the data stand out,
#    facilitate comparison, and are information rich
# 4. Gain additional experience working w

# a. wr1500m - a data frame containing information about the 
# world record in the 1500 meter men's race

# b. SO2012Ctry which is a data frame with information about 
# each country that had an athlete participate in the olympics

# c. London2012ALL_ATHLETES.rda - a data frame which contains data
# on individual atheletes who participated in the 2012 Olympics


# We will be making 4 plots with these sources of data.
# You will make each plot by gradually adding more features to it.

# The help for plot.default describes many parameters that are  
# available for many of the plotting functions.
# Also, ?par provides help on many other plotting parameters.
# Some of these can be set in the plot function, while others
# are set in a call to par()

##############################
# PLOT 1. World Record in Men's 1500 meter run
# When watching the summer Olympics, we might be curious
# about how much faster today's runners are compared to
# runners 50 or 100 years ago. 

# A search on Wikipedia shows us tables of the times for the
# world record holder in 1500 meters dating back to 1892.
# We can use readHTMLTable() (from the package "XML" in R) and 
# this function goes off to the Web and downloads the tables we 
# need and reads them into R as data frames. After some text 
# manipulation, we have a data frame that we can use to examine 
# the world records graphically.
# Later in this semester, you will be able to do the same sort
# of thing (access data from the Web from within R and clean it
# for analysis.).  Right now, the data are available to you in
# the hw3 directory in the file WR1500MeterMen.rda.

# load the data
load("WR1500MeterMen.rda")

# The name of the object loaded is wr1500m
# The time (in the column "times") in these data are recorded in seconds, 
# and they are seconds over 3 minutes. 
# So a time of 70 is really 4 minutes and 10 seconds.

# Q1a. How many world records does this data frame contain
n.wr <- nrow(wr1500m)

# Q1b. Use R commands to find out who currently holds the world
# record in the men's 1500 meter.
 
wr.name <- wr1500m[which.min(wr1500m$times), 4]


# Let's look at the relationship between date and time.
# Q1c. What type of variable (numeric (continuous or discrete), nominal ordinal)
# are year and times? (no need to save the output, just look at it)
### Year: discrete numeric, nominal
### Time: continuous numeric, ordinal
# When we are examining a variable to see how it changes in time,
# we typically make a line plot, with time on the x-axes and 
# the (x,y) values connected with line segments.

# Q2a. Begin by making a step plot of year by times for these data.
# (hint: use the type argument in plot)
# Don't bother to make the plot pretty yet; we will get to that later.
# But do add 180 to the times so that they are accurate measurements in seconds,
# store that in a new variable and add to the data frame.


times_sec <- (wr1500m$times)+180
wr1500m$times_sec <- times_sec
# plot( your code here )
plot(wr1500m$year, wr1500m$times, type="s")
plot(wr1500m$year, wr1500m$times)


# Q2b. Redo the plot using a date that incorporates the month as 
# well as the year. For example, in Sep 1904 the world record 
# was broken by James Lightbody. Use a date of 1904.75 for this
# date. If any month is NA, use 0.5 for the fraction.
# Create a new variable, new_year, with the date in this format but
# first find and set all missing months to 0.5
# Add new_year to the dataframe.

wr1500m$month[is.na(wr1500m$month)] <- 6
new_year <- (wr1500m$year)+(wr1500m$month)/12
wr1500m$new_year <- new_year
# plot( your code here )
plot(wr1500m$new_year, wr1500m$times, type="s")

# Q3. The current world record was set in 1998. If we want to
# show that this record still stands in 2014, we could add a 
# horizontal line segment to the plot from 1998 to 2014 at the 
# 1998 record time.  
# To do this: remake the plot and set the xlim parameter 
# so that 2014 is included in the x-axis scale;
# then use the lines() function to add the additional segment.

# wr_1998 <- your code here
# plot( your code here )
# lines( your code here )
wr_1998 <- wr1500m[which(wr1500m$year == 1998), 6]
plot(wr1500m$new_year, wr1500m$times_sec, type="s", xlim = c(1892,2014))
lines (x=c(wr1500m[wr1500m$year == 1998, 7],2014), y= c(wr_1998, wr_1998))


# Q4. There are two times where the record stood for several
# years - in 1944 and 1998. Let's make it easier to see these
# dates and let's include the name of the athlete who set
# the record.  This additional reference information makes
# our plot richer.

# Add two grey vertical lines. One at 1944 and the other at 1998.
# Add the runner's name next to each vertical line.
# To do this, you will need the abline() function, the text() function,
# and you might want to consider the cex, col, pos, adj parameters.
# Also, do not type in the athlete's name. Instead, use subsetting
# of wr1500m$athlete to access it.

# wr_1944 <- your code here
wr_1944 <- wr1500m[which(wr1500m$year == 1944), 4]
wr_1998 <- wr1500m[which(wr1500m$year == 1998), 4]
abline(v = 1944 )
abline(v = 1998 )
text(x=1940,y=250,labels=wr_1944,cex=1 ,col="blue")
text(x=1990,y=240,labels=wr_1998,cex=1 ,col="blue")


# Q5. Now we are ready to add other contextual information.
# Remake the plot as before but now adding axis labels and a title.
# This is the FINAL version of the plot of world record times.

# put your final version of the plotting commands below.
plot(wr1500m$new_year, wr1500m$times_sec, type="s", xlim = c(1892,2014), main="world record in the 1500 meter men's race", 
     xlab="Year", ylab="Time in seconds")
wr_1998 <- wr1500m[which(wr1500m$year == 1998), 6]
lines (x=c(wr1500m[wr1500m$year == 1998, 7],2014), y= c(wr_1998, wr_1998))
wr_1944 <- wr1500m[which(wr1500m$year == 1944), 4]
wr_1998 <- wr1500m[which(wr1500m$year == 1998), 4]
abline(v = 1944 )
abline(v = 1998 )
text(x=1940,y=250,labels=wr_1944,cex=1 ,col="blue")
text(x=1990,y=240,labels=wr_1998,cex=1 ,col="blue")
## You have finised the first plot!!

################################
# PLOT 2
# A lot of medal counting goes on during the Olympics.
# We might wonder about the relationship between number of medals
# a country has and the size of its population and its wealth.
# We collected data from various sources (ManyEyes, Guardian,
# ISO) to create this data frame with GDP, population, and other information
# about each country that participated in the Olympics.

# The data frame SO2012Ctry contains this information.
# It can be loaded into R with

# load("~/src/stat133/assignments/hw2/SummerOlympics2012Ctry.rda")
load("SummerOlympics2012Ctry.rda")


#Q6 Take a look at the variables in this data frame.
# What kind of variable is GDP and population?
### GDP : continuous
### population : discrete

# What about Total?
### Total : discrete

# To examine the relationship between these three variables,
# we could consider making a scatter plot of GDP against population
# and use plotting symbols that are proportional in size to
# the number of medals. 

# To begin, make a plot of GDP against population. 
# Consider which of the three principles of good graphics this
# plot violates and why.

# plot( your code here )
plot(x=SO2012Ctry$pop, y=SO2012Ctry$GDP)

### Data stand out, Values are plotted on the top of each other  
### Facilitate comparison OR poor scale, We should zoom in on the bulk of the data
### Should fill data region, lots of empty space
### Information rich, for example identify outliers

#Q7. Let's examine GDP per person (create this new variable yourself)
# and population. Use a log scale for both axes. Use the symbols()
# function rather than plot(), and create circles for the plotting
# symbols() where the area of the circle is proportional to the 
# total number of medals.

GDP_per_person <- SO2012Ctry$GDPPP
GDP_per_person  -> SO2012Ctry$GDP_per_person
symbols(log(SO2012Ctry$pop), log(SO2012Ctry$GDP), circles = SO2012Ctry$Total)


# Q8. It appears that the countries with no medals are circles too.
# Remake the plot, this time using *only the countries that won medals*. 
# If necessary adjust the size of the circles.
# Then add the non-medal countries to the plot using the "." 
# plotting character.
symbols(log(SO2012Ctry$pop[SO2012Ctry$Total > 0]), log(SO2012Ctry$GDP_per_person[SO2012Ctry$Total > 0]), circles = SO2012Ctry[which(SO2012Ctry$Total > 0), 5])
points(log(SO2012Ctry$pop[SO2012Ctry$Total == 0]), log(SO2012Ctry$GDP_per_person)[SO2012Ctry$Total == 0], pch= 20)


# Q9. Make the plot information rich by adding axis labels, 
# title, and label 5 of the more interesting points
# with the country name. Use text() to do this.

symbols(log(SO2012Ctry$pop[SO2012Ctry$Total > 0]), log(SO2012Ctry$GDP_per_person[SO2012Ctry$Total > 0]), circles = SO2012Ctry[which(SO2012Ctry$Total > 0), 5], xlab="Population", ylab="GDP per person")
points(log(SO2012Ctry$pop[SO2012Ctry$Total == 0]), log(SO2012Ctry$GDP_per_person)[SO2012Ctry$Total == 0], pch= 20)
top5 <- as.character(SO2012Ctry$Country[order( SO2012Ctry$Total, decreasing = TRUE)[1:5]])
text(x=log(SO2012Ctry$pop[SO2012Ctry$Country == top5[1]]),y=log(SO2012Ctry$GDP_per_person[SO2012Ctry$Country == top5[1]]),labels=top5[1],cex=1 ,col="blue")
text(x=log(SO2012Ctry$pop[SO2012Ctry$Country == top5[2]]),y=log(SO2012Ctry$GDP_per_person[SO2012Ctry$Country == top5[2]]),labels=top5[2],cex=1 ,col="blue")
text(x=log(SO2012Ctry$pop[SO2012Ctry$Country == top5[3]]),y=log(SO2012Ctry$GDP_per_person[SO2012Ctry$Country == top5[3]]),labels=top5[3],cex=1 ,col="blue")
text(x=log(SO2012Ctry$pop[SO2012Ctry$Country == top5[4]]),y=-0.5+log(SO2012Ctry$GDP_per_person[SO2012Ctry$Country == top5[4]]),labels=top5[4],cex=1 ,col="blue")
text(x=log(SO2012Ctry$pop[SO2012Ctry$Country == top5[5]]),y=1+log(SO2012Ctry$GDP_per_person[SO2012Ctry$Country == top5[5]]),labels=top5[5],cex=1 ,col="blue")




######################################
# PLOT 3.
# Plotting points on maps can help us see geographic relationships
# 
#Q10. Install the maps library and load it into your R session.
# Make a map of the world, using the function map(),
# where the countries are filled with a light grey color.

## you only need to run these two lines once:
#install.packages("maps")
library("maps")
map()
world <- map( fill=TRUE, col= "light grey" )


#Q11. Use the symbols() function to add circles to the map where
# the circles are proportional in area to the number of medals
# won by the country. You may find the add parameter useful.
# (Be sure to NOT plot circles for countries with 0 medals).
# adjust the size of the circles if necessary

# Check what variables you have in the dataframe using names().

# pull out the contries that won at least one medal (you will need at least
# the contries longitude, latitude and Total.)

wonMedal <- SO2012Ctry[SO2012Ctry$Total > 0,]
names(wonMedal)
world <- map( fill=TRUE, col= "light grey" )
symbols(wonMedal$longitude, wonMedal$latitude, circles=wonMedal$Total, add = TRUE )

# wonMedal <- your code here
# world <- your code here
# symbols( your code here )


#Q12. Remake the plot and fill in the circles with a partially
# transparent gold color. To create this color: 
# install the RColorBrewer library and load it into R;
# call display.brewer.all() to examine the palettes;
# choose a palette and ask for the names of a few colors 
# using brewer.pal();
# pick one of the colors and create a new one that is transparent
# by adding two more digits to the end of the name, e.g.,
# if you want to use "#FEB24C" then make it transparent with
# e.g. myColor = "#FEB24CAA" or   "#FEB24C88"

# You only need to call these two lines once:
#install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()
brewer.pal(3, "YlGnBu" )
myColor <- "#EDF8B1AA"

symbols(wonMedal$longitude, wonMedal$latitude, circles=wonMedal$Total, bg=myColor, add = TRUE )
# display.brewer.all( your code here )
# brewer.pal( your code here )

# myGold <- your selected color

#world <- your code here
#symbols( your code here )


## That was the FINAL version of this plot

##############################################
# PLOT 4
# During the news coverage of the Olympics it was noted that this
# Olympics had by far the greatest number of women competing and
# that some countries had female athletes competing for the first time.

# The data file is called London2012ALL_ATHLETES.rda
# and contains information about every athlete who competed 
# in the Olympics.
# load("~/src/stat133/assignments/hw2/London2012ALL_ATHLETES.rda")
load("London2012ALL_ATHLETES.rda")


# There is one observation for each athlete. 
# (Actually, about 20 athletes have two records if they
# competed in different sporting events. Let's not worry about that.)

#Q13. We are interested in the relationship between Sport and Sex. 
# Examine the data frame and check which type of data each variable is.
names(athletes)
### Name : nominal
### Sex : nominal
### Sport : nominal
### Country : nominal
### MoreThan1Sport : ordinal or nominal is OK

# The table() and sum() functions might be helpful for answering 
# some of the questions below. 

# How many athletes competed in the 2012 Olympics?
# n.athletes <- your code here
n.athletes <- length(athletes$Name)
# How many women competed?
length(athletes$Name[which(athletes$Sex == "F")]) 

# What proportion of the participants were women?
# frac.women <- your code here
frac.women <- (length(athletes$Name[which(athletes$Sex == "F")]))/n.athletes 
n.sports <- length(table(athletes$Sport))
# How many sports were there?
# n.sports <- your code here


#Q14. Make a barplot of Sport and Sex that emphasizes the 
# important differences. To do this, first make a table of 
# Sex by Sport. This will be the input to barplot(). 
# Make the barplot with the parameter beside = TRUE and 
# and again with beside = FALSE. Determine which of these 
# barplots provides the easiest comparison. 
athTab <- table(athletes$Sex, athletes$Sport)
barplot(athTab, beside = FALSE)
barplot(athTab, beside = TRUE)
set.beside <- TRUE


#Q15. Remake the barplot above, but this time switch the order 
# of Sport and Sex in the call to table(). Use the value for
# the beside parameter that you decided was best for the 
# plot in Q 14. 

athTab2 <- table(athletes$Sport, athletes$Sex)
barplot(athTab2, beside = TRUE)

# Compare the barplot with (Sex, Sport) vs (Sport, Sex). 
# Which makes a more interesting visual comparison, plot 1 or 2?
# store your answer (1 or 2) in best.plot.

best.plot <- 1


# Q16. Notice that the bars are in alphabetical order by sport.
# To facilitate comparisons, we might want to arrange
# the bars in order of participation in a sport. To do this,
# call order() on the return value from making a table of Sport alone.
# Assign this vector to a variable, say orderSport.
# Then reorder your two-way table of Sport and Sex,
# using the orderSport vector and [ ] to subset the table and rearrange
# the rows/cols. The resulting barplot should show bars in 
# increasing height.

orderSport <- order(table(athletes$Sport), decreasing = F)
barplot(athTab[, orderSport], beside = TRUE)
barplot(athTab2[orderSport, ], beside = TRUE)
# barplot( your code here )


# Q17. Finally to make the plot more informaation rich, try turning
# the x-axis labels on their side. To do this, find a parameter
# in par() that will rotate the x-axis tick mark labels. Even though
# you found the parameter in the par() function, this
# parameter can be added in the call to barplot().
# Also find and use a parameter to shrink the text for these labels. 
# Lastly, add a title to the plot.

barplot(athTab[, orderSport], beside = TRUE, las=3, cex.names = 0.8, main="London 2012 Male vs. Female athlets" )

# This was the final version of the 4th plot.

# You are DONE.
# Hope you had fun making increasingly complex and attractive plots with R.
