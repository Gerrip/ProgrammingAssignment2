## You can see this function as a dropbox outside the global environment. 
## There are four functions created here. With them you can set() a matrix in the dropbox
## and get() it out again. 
## CacheSolve() can use this function to store the inverseMatrix (setInverse())
## which it calculates. It can also check if there is already a inverseMatrix (getInverse()).
## In that cases, it won't have to calculate it again, but simply use the value,
## which is already calculated.
## All the four functions are stored in a list. If you store this function in variable x
## you can x$function() outside this environment to call the different functions 
## created here.

makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL #iM is variable where the inversed Matrix is stored
    
  #With x$set() you can put a new matrix into this environment. 
  #If so, any inverseMatrix which was cached will be set to NULL.
  set <- function(y) { 
    x <<- y
    iM <<- NULL
  }
  
  
  #With x$get() you can ask for the matrix stored 
  #in the environment created with this function. 
  get <- function() x 
  
  #With setInverse the other function cacheSolve() is able 
  #to store the inverse Matrix into the environment of this matrix. 
  setInverse <- function(inverse) iM <<- inverse 
  
  #with getInverse the other function cacheSolve will be able 
  #to check if there is already an inverse Matrix stored in 
  #this environment.
  getInverse <- function() iM 
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  
}


## cacheSolve will check if there is already a inverseMatrix of x in the 'dropbox' 
## makeCacheMatrix(). If so, it will get that value and give it back to you. If 
## not, it will calculate the inverse matrix, give it to you and store it in the 
## 'dropbox' makeCacheMatrix() so it doesn't have to do it the next time again. 

cacheSolve <- function(x, ...) {
  iM <- x$getInverse() # checks the value of the iM in 'dropbox' makeCacheMatrix()
  if(!is.null(iM)) { #checks is this NULL (not calculated) or has a value (calculated)
    message("getting inversed Matrix") 
    return(iM) #if so, gives you the inverseMatrix which was stored in the 'dropbox'
  }
  data <- x$get() #if not, it gets the normal matrix from the 'dropbox'
  iM <- solve(data, ...) #calculate the inverse Matrix
  x$setInverse(iM) #stores it in the 'dropbox' so it won't have to calculate it next time
  iM #and give the inverse back to you.
  
        
}
