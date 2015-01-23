## Functions to calculate the inverse of a given matrix ##

## This function defines the getter and setter methods on the matrix 
## It also caches the inverse in a environment variable.
makeCacheMatrix <- function(x = matrix()) {
        ##initialize the inverse variable
        i <- NULL
        set <- function(y) {
                x <<- y
                ##reset the inverse environment variable
                i <<- NULL
        }
        
        ##get original matrix
        get <- function() x
        
        ##set the inverse of the matrix to the environment variable
        setinverse <- function(inverse) i <<- inverse
        
        ##return the inverse
        getinverse <- function() i
        
        ##Define the methods on the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##Get the original matrix
        data <- x$get()
        ##Cacluate the inverse of the orinal matrix
        i <- solve(data, ...)
        ##cache the inverse 
        x$setinverse(i)
        i
}
