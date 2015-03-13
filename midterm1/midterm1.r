# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for countries in 2012.

# your code here

# calculate the mean and the maximum of GDP in the dataset. Store these as the
# variables <mean.GDP> and <max.GDP> respectively.

# mean.GDP <- your code here
# max.GDP <- your code here



# For each country in the dataset, calculate the number of female athletes (Female) divided
# by the total number of athletes (Female + Male). Store this as the variable
# <female.prop>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset.

# female.prop <- your code here




# Create the following two subsets of the dataset and store them as variables with the
# indicated names:
# 1) Countries with 0 bronze medals: <subset.nobronze>
# 2) Countries with more than or exactly 3 bronze medals: <subset.threebronze>

# subset.nobronze <- your code here
# subset.threebronze <- your code here




# For each of your subsets, create a vector giving the population size. Store
# these as variables <subset.nobronze.pop> and <subset.threebronze.pop>.

# subset.nobronze.pop <- your code here
# subset.threebronze.pop <- your code here






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
  # your code here  
}


# Please create a plot of the proportion of female athletes (y-axis) 
# against the total number of athletes (x-axis). Your plot should include the following 
# features:
# 1) a title "Proportion of female athletes vs Total # athletes"
# 2) axis labels: "Proportion of female athletes" and "Total # athletes"
# 3) plotting character set to 10
# 4) a red horizontal line at female proportion of 0.50.



