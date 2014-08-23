## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
temp <- NULL

makeCacheMatrix <- function(x = matrix()) {

  if(!is.null(temp)) {
    print("WOO")
	 temp <- cachemean(temp)
	 temp
  } else if(!is.null(x)) {
     temp <- makeVector(x)
	 temp <- cachemean(temp)
	 temp
  } else {
    return(NAN)
  }
  
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 solve(x)
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
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}