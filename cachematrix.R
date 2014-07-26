## THIS PROGRAM WILL TAKE A MATRIX AND CALCULATE IT'S INVERSE 
## IT WILL THEN STORE THE INVERSE MATRIX IN THE CACHE AND 
## UTILIZE THAT VALUE RATHER THAN RECALCULATING THE MATRIX

#think of this function as list of commands that creates functions to
#1.build the matrix
#2.creates a function to call the matrix
#3.creates a function to calculate the inverse of the matrix
#and then store the result of the matrix inside the function as a "private"
#variable to the function

makeCacheMatrix <- function(x = matrix()) {
        
        invs <- NULL
        #1 set the the matrix
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        #2 gets the matrix
        get <- function() x
        #3 sets the inverse
        setsolve <- function(solve) invs <<- solve
        #4 gets the inverse
        getsolve <- function() invs
        #creates the list output
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## This function determines if the inverse of the matrix has already been calculated and if it hasn't 
## then it caculates the inverse of the matrix if the procedure has alread been completed then it 
##used the cached value

cacheSolve <- function(x, ...) {
        invs <- x$getsolve()
        #does a value for the inversion of the matrix already exist? if Y return cached data
        if(!is.null(invs)) {
                message("matrix inverted already getting cached data")
                return(invs)
        }
        #if solve has not already been performed then invert the matrix
        data <- x$get()
        invs <- solve(data, ...)
        x$setsolve(invs)
        invs
}

##testing github version changes by adding this comment