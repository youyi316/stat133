# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for countries in 2012.

# your code here
load("SummerOlympics2012Ctry.rda")


# calculate the mean and the maximum of GDP in the dataset. Store these as the
# variables <mean.GDP> and <max.GDP> respectively.
mean.GDP<-mean(SO2012Ctry$GDP)
max.GDP<- max(SO2012Ctry$GDP)





# For each country in the dataset, calculate the number of female athletes (Female) divided
# by the total number of athletes (Female + Male). Store this as the variable
# <female.prop>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset.

female.prop <- SO2012Ctry$Female/((SO2012Ctry$Male)+(SO2012Ctry$Female))






# Create the following two subsets of the dataset and store them as variables with the
# indicated names:
# 1) Countries with 0 bronze medals: <subset.nobronze>
# 2) Countries with more than or exactly 3 bronze medals: <subset.threebronze>

subset.nobronze <- SO2012Ctry[which(SO2012Ctry$Bronze == 0),9]
subset.threebronze <- SO2012Ctry[which(SO2012Ctry$Bronze >= 3),9]




# For each of your subsets, create a vector giving the population size. Store
# these as variables <subset.nobronze.pop> and <subset.threebronze.pop>.

subset.nobronze.pop <- SO2012Ctry[which(SO2012Ctry$Bronze == 0),7]
subset.threebronze.pop <- SO2012Ctry[which(SO2012Ctry$Bronze >= 3),7]






# Implement the function medpopByGDPPP. Your function should take the following
# arguments:
#
# <GDPPP.cutoff>: a numeric constant giving a cutoff to subset by
# <GDPPP>: a numeric vector of GDP per person
# <pop>: a numeric vector of populations
#   (this should be the same length as <GDPPP>)
#
# Your function should return the median of the populations of countries
# whose values in <GDPPP> are strictly less that <GDPPP.cutoff>.

medpopByGDPPP <- function(GDPPP.cutoff, GDPPP, pop){
  if (GDPPP >= GDPPP.cutoff)
    stop("GDPPP greater than GDPPP.cutoff")
  if (GDPPP <= GDPPP.cutoff) {
    median(pop)
  }
  median.pop=median(pop)
  return(median.pop)
}


# Please create a plot of the proportion of female athletes (y-axis) 
# against the total number of athletes (x-axis). Your plot should include the following 
# features:
# 1) a title "Proportion of female athletes vs Total # athletes"
# 2) axis labels: "Proportion of female athletes" and "Total # athletes"
# 3) plotting character set to 10
# 4) a red horizontal line at female proportion of 0.50.

Total.athletes <- ((SO2012Ctry$Male)+(SO2012Ctry$Female))
graph = plot(Total.athletes, female.prop, main="Proportion of female athletes vs Total # athletes", 
     xlab="Total # athletes", ylab="Proportion of female athletes")
graph.new <- graph+abline(h = 0.5, col="red" )


#Ambiguious: plotting character set to 10, if it means the size of the point
graph = plot(Total.athletes, female.prop, main="Proportion of female athletes vs Total # athletes", 
             xlab="Total # athletes", ylab="Proportion of female athletes", cex=10)

#Ambiguious: plotting character set to 10, if it means the size of the title
graph = plot(Total.athletes, female.prop, main="Proportion of female athletes vs Total # athletes", 
             xlab="Total # athletes", ylab="Proportion of female athletes", cex.main=10)


