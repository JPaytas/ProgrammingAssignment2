## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ##Initialize M as NULL
    ##Setters and Getters (Access/Retrieve)
    set <- function(y) { 
        x <<- y ##Assign Input 
        inverse <<- NULL ##M NULL to clear Cache
    }
    get <- function() x ##x is being retrieved from parent
    setinverse <- function(inverse) m <<- inverse ##Defines setter for the mean
    getinverse <- function() inverse ##retrieve correct m
    ##Set lists to names so they can be retrievable by $ operators, [] will not work!
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
            message("getting cached data for inverse")
            return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

mdat <- matrix(c(5,10, 10,1), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2")))
mdat2 <- matrix(c(50,100, 12,90), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2")))



mdat
summary(mdat)
attributes(mdat)
class(mdat)
structure(mdat)
solve(mdat)

acm <- makeCacheMatrix(mdat)
acm$get() ##Retrieves value of x
acm$getinverse() ##retrives the value of m, which should be NULL
cacheSolve(acm) ##notice the mean calculated is mean of 30 through 50
acm$getinverse() ##retrieve it directly, now that it has been set
