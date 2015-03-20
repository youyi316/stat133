# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).
  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  m = matrix(rep(initial.doctors,n.days), nrow=n.doctors, ncol=n.days)
  for (k in 1:n.days){
    n= sample (1:n.doctors, 2, replace = F)
    if ( !(m[n[1],k]== m[n[2],k])){
      x=n[m[n, k]==0]
      m[x, k:n.days] = sample (c(1,0),1,prob=c(p, 1-p))
    }
  }
  return(m) 
}






# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)


n.doctors = 20
n.days = 100
set.seed(42)
initial.doctors= sample(c(0,1), n.doctors, prob=c(0.9, 0.1), replace=T)
p=0.2
new.doctors=sim.doctors (initial.doctors, n.doctors, n.day, p)
sum.doctors <- function(n.days, new.doctors){
  s=rep(0, n.days)
  for (k in 1:n.days) {
    s[k]=sum(new.doctors[,k])
  }
  return(s) 
}
y=sum.doctors(n.days, new.doctors)
plot(1:n.days, y, type="l", xlab="days", ylab="Number of doctors accpeted the drug")



p2=0.4
new.doctors=sim.doctors (initial.doctors, n.doctors, n.day, p2)
sum.doctors <- function(n.days, new.doctors){
  s=rep(0, n.days)
  for (k in 1:n.days) {
    s[k]=sum(new.doctors[,k])
  }
  return(s) 
}
y2=sum.doctors(n.days, new.doctors)
lines (1:n.days, y2, type="s",col="blue")

p3=0.5
new.doctors=sim.doctors (initial.doctors, n.doctors, n.day, p3)
sum.doctors <- function(n.days, new.doctors){
  s=rep(0, n.days)
  for (k in 1:n.days) {
    s[k]=sum(new.doctors[,k])
  }
  return(s) 
}
y3=sum.doctors(n.days, new.doctors)
lines (1:n.days, y3, type="s",col="orange")


p4=0.6
new.doctors=sim.doctors (initial.doctors, n.doctors, n.day, p4)
sum.doctors <- function(n.days, new.doctors){
  s=rep(0, n.days)
  for (k in 1:n.days) {
    s[k]=sum(new.doctors[,k])
  }
  return(s) 
}
y4=sum.doctors(n.days, new.doctors)
lines (1:n.days, y4, type="s",col="red")


p5=0.8
new.doctors=sim.doctors (initial.doctors, n.doctors, n.day, p5)
sum.doctors <- function(n.days, new.doctors){
  s=rep(0, n.days)
  for (k in 1:n.days) {
    s[k]=sum(new.doctors[,k])
  }
  return(s) 
}
y5=sum.doctors(n.days, new.doctors)
lines (1:n.days, y5, type="s",col="green")


