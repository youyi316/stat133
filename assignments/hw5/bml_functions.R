#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)


bml.init <- function(r, c, p){
  m = matrix(sample(c(1,2,0), size=r*c, prob=c(0.5*p, 0.5*p, (1-p)),replace = TRUE), 
              nrow=r, ncol=c)
  return(m) 
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  m_copy=m
  m_copy2=m_copy
  r=nrow(m)
  c=ncol(m)
  grid.new=FALSE
  for (i in 1:r) {
    for (j in 1:c) {
      if (j<c){
        if (m[i,j+1] == 0){
          if (m[i,j]==1 ) { 
            m_copy[i,j]=0 
            m_copy[i, j+1]=1
            m_copy2[i,j]=0 
            m_copy2[i, j+1]=1 
            grid.new=TRUE}
           }
      }else{
        if (m[i,c] == 1){
        if (m[i,1]==0 ) { 
          m_copy[i,1]=1
          m_copy[i,c]=0 
          m_copy2[i,1]=1
          m_copy2[i,c]=0 
          grid.new=TRUE}   
        }
      }
    }
  } 
  for (i in 1:r) {
    for (j in 1:c) {
      if (i>1){
        if (m_copy[i-1,j] == 0){
          if (m_copy[i,j]==2 ) { 
            m_copy2[i,j]=0 
            m_copy2[i-1,j]=2 
            grid.new=TRUE}
          }
     }else{
        if (m_copy[1,j] == 2){
        if (m_copy[r,j]==0 ){ 
            m_copy2[1,j]=0
            m_copy2[r,j]=2 
            grid.new=TRUE}      
        } 
      }
    }
  }
   return(list(m_copy2, grid.new))
}





#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
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

bml.sim(2,2,0.4)
