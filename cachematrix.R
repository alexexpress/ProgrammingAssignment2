## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" which is actually list of four functions. 
## In here, we can store ("cache") a previously computed inverse of some matrix. 

makeCacheMatrix <- function(x = matrix()) {
        
        ## First, we create the empty object "inv".
        inv <- NULL
        
        ## setMat is a function that takes a matrix and sets it to the variable "x".
        setMat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## getMat is a function that retrieves the value for "x".
        getMat <- function() {
                x
        }
        
        ## setInv is a function that takes an input and sets it to the variable "inv".
        setInv <- function (inverse) {
                inv <<- inverse
        }
        
        ## getInv is a function that retrieves the value for "inv".
        getInv <- function() {
                inv
        }
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## cacheSolve takes a matrix as input and does one of two things:
## retrieves the previously stored inverse of the matrix
## if we don't have this, it calculates the inverse, stores it in "inv", and prints it

cacheSolve <- function(x, ...) {
        
        ## First, we use getInv to retrieve any previously calculated inverse and set it to "inv"
        inv <- x$getInv()
        
        ## If this is not empty we simply print a message and return the stored value for "inv"
        if(!is.null(inv)) {
                message("Retrieving Cached Data")
                return(inv)
        }
        
        ## If inv is null, we instead:
        
        ## Use getMat to set our input matrix to "data".
        data <- x$getMat()
        
        ## Solve the inverse and set it to "inv".
        inv <- solve(data, ...)
        
        ## Use setInv to store the calculated inverse
        x$setInv(inv)
        
        ## Lastly, print the newly calculated inverse of x
        inv
}

