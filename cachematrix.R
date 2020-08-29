## Put comments here that give an overall description of what your
## functions do

## this function creates an empty matrix and sets up objects in the apprpriate environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## reset and empy out the variable m
        set <- function(y){
                x <<- y 
                ## assign the input argument matrix x to the parent environment as object 'mtrx'
                m <<- NULL 
                ## clears out any previous value of m from any prior runs
        }
        
        get <- function () x ## x is a free variable within get, so R will take the value of x from the parent environment, in this case makeCacheMatrix
        
        set.inverse <- function(inverse) m <<-inverse ## <<- is used here to make the object m available in the parent environment after set.inverse finishes running
        
        get.inverse <- function() m
        
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse
        )
}


## Check to see if the inverse is already calculated, and if not, then the is funciton will use the solve function and return the inverse.

cacheSolve <- function(x, ...) {
        m <-x$get.inverse ## check to see if the inverse is already calculated by retriving the get.inverse output of x
        
        if(!is.null(m)) { ## checks to see if the value of m is null.
                message("getting cached data")
                return(m) ##if the m is NOT NULL< then the value of m is returned (there is an implicit ELSE here)
        }
        
        data <- x$get() ##if m is null, so !is.null(m) is  FALSE, then R can retrive the data from x$get
        m <- solve(data, ...)
        x$set.inverse(m)
        m
        }
