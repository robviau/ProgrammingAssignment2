## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #initializing variable to store matrix
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  get <- function() x # just returns the matrix
  revmatrix <-function(rev) m<<- solve #store reversed matrix to m
  getmatrix <- function() m
  list(set = set, get = get,
       revmatrix = revmatrix,
       getmatrix = getmatrix) # make list vector of functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix() #read the memory for the matrix
  if(!is.null(m)) { #if reversed data present, just return it
    #print("getting cached data")
    return(m)
  }
  data <- x$get() # calculate if reverse matrix not present
  m <- solve(data, ...)
  x$revmatrix(m)
  m

    
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    #print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}