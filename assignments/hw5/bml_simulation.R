#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.



## 1. To study how the density affects traffic jam
  m = bml.init (r,c,p)
  count=0
  while(count < 5000) {
    if (bml.step(m)[[2]] == TRUE) {
      m=bml.step(m)[[1]]
      count=count+1
    }else{
      return(count)
    }
  }
  return(count)
}


x <- replicate(50, {
  mm <- bml.sim(5,5,0.8)
  mean(mm)
})

plot(x)


## 2. To illustrate the point that most traffic jams happen within the first 40 steps. 
bml.sim2 <- function(r, c, p){
  m = bml.init (r,c,p)
  count=0
  while(count < 50) {
    if (bml.step(m)[[2]] == TRUE) {
      m=bml.step(m)[[1]]
      count=count+1
    }else{
      return(count)
    }
  }
  return(count)
}


x <- replicate(50, {
  mm <- bml.sim2(5,5,0.7)
  mean(mm)
})

plot(x)


## 3. To illustrate the point that most traffic jams happen within the first 40 steps. 
bml.sim3 <- function(r, c, p){
  m = bml.init (r,c,p)
  count=0
  while(count < 200) {
    if (bml.step(m)[[2]] == TRUE) {
      m=bml.step(m)[[1]]
      count=count+1
    }else{
      return(count)
    }
  }
  return(count)
}


x <- replicate(50, {
  mm <- bml.sim3(45,5,0.7)
  mean(mm)
})

plot(x)







