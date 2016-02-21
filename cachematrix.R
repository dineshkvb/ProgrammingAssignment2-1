## Function makeCacheMatrix creates a special matrix object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
## setmean and getmean are functions very similar to set and get.
## They do not calculate the mean but they simply store the value of the input in a variable a
## into the main function makeVector (setmean) and return it (getmean).
	
makeCacheMatrix <- function(x = matrix()) 
{
## variable a to store the input
      a <- NULL
## set is a function that changes the vector stored in the main function
      set <- function(y) 
	{
            x <<- y
            a <<- NULL
        }
## get is a function that returns the vector x stored in the main function
      get <- function() x
      setinverse <- function(solve) a <<- solve
      getinverse <- function() a
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Function cacheSolve computes the inverse of the special matrix (which is the input of cachemean) returned by makeCacheMatrix above
cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
      a <- x$getinverse()
      if(!is.null(a)) 
	{
            message("obtaining cached data")
            return(a)
        }
      data <- x$get()
      a <- solve(data, ...)
      x$setinverse(a)
      a        
}
