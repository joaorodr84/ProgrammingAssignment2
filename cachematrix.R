## The function 'makeCacheMatrix' has the following parameter:
## 'x' which is a matrix. By default, it assumes the empty matrix.
##
## This function is some kind of class (like in object-oriented programming).
## It contains 2 variables that are not accessible from the outside of
## 'makeCacheMatrix's scope. They can be considered as private variables, like
## in a class.
##
## The only way to obtain the 'x' matrix is to call the 'get' function.
## The 'x' matrix can be changed using the 'set' function, so there is no need
## to rerun the whole function again. We just call its 'set' function.
##
## 'get' and 'set' are like methods in a class. They are called "after" the
## 'makeCacheMatrix' has been run and stored in a variable, let's say 'a':
##     m1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
##     a <- makeCacheMatrix(m1)
##
## Now, we simply use a$get() to obtain the 'm1' matrix, which is stored in the
## 'x' private variable.
## We can set a new matrix in 'x' by doing:
##     m2 <- matrix(1:4, nrow = 2, ncol = 2)
##     a$set(m2)
##
## Now, 'x' contains 'm2' and calling a$get() will return the 'm2' matrix.
## Similarly, for the getinv and setinv functions. They will get/set the value
## stored in the private variable 'm'. Normally this value should contain the
## inverse of the matrix 'x', but we can set it to anything, even though that
## is not the purpose of this exercise.
##
## The purpose of this assignment is to calculate the inverse of the matrix 'x'
## and set it to the private variable 'm' via the function setinv. And that is
## exactly what 'cacheSolve' does.
##
## The function 'cacheSolve' has the following parameter:
## 'x' which is the value returned by calling 'makeCacheMatrix', i.e. some kind
## of instance of the 'makeCacheMatrix' class (let's call is 'a' like before).
##
## 'cacheSolve' starts by getting the inverse matrix from 'a'. If the inverse
## matrix has already been calculated, it simply print a message and gets the
## cached inverse.
##
## If the inverse has not yet been set in the instance 'a', the code proceeds:
##     - it gets the 'x' matrix of 'a';
##     - it calculates the inverse matrix of 'x'
##     - it sets the inverse matrix in the 'm' variable of 'a', using setinv
##     - it finally returns the inverse matrix 'm'
##
## So, these functions are a clever way to bring something like object-oriented
## programming to R.



## Encapsulates a matrix and its inverse if a different scope. It works like a
## class in object-oriented programming. This is a cache!

makeCacheMatrix <- function(x = matrix()) {
    ## Initializes the inverse matrix 'm' as NULL
    m <- NULL
    ## Defines the set function for the 'x' variable
    set <- function(y) {
        ## Sets the given 'y' matrix to 'x' / Caches 'y' into 'x'
        x <<- y
        ## Sets the inverse matrix to NULL, since it has not been calculated yet
        m <<- NULL
    }
    ## Defines the get function for the 'x' variable, which only returns 'x'
    get <- function() x
    ## Defines the set function for the 'm' variable, i.e. for the inverse
    setinv <- function(inv) m <<- inv
    ## Defines the get function for the 'm' variable, which only returns 'm'
    getinv <- function() m
    ## Returns a list with the 4 just defined functions. These functions are
    ##     the only data from 'makeCacheMatrix' that are visible from the
    ##     outside environment.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Calculates the inverse of the 'x' matrix from 'makeCacheMatrix' and sets it
## is the 'm' variable of 'makeCacheMatrix', using the setinv function.
## In other words, it calculates the inverse matrix and caches it.

cacheSolve <- function(x, ...) {
    ## Gets the inverse matrix from the cache
    m <- x$getinv()
    ## Checks whether the inverse matrix is defined
    if (!is.null(m)) {
        ## Print a message telling that the cached data is coming
        message("Getting cached data...")
        ## Returns the cached inverse matrix
        return(m)
    }
    ###
     # Case where the inverse matrix has not been calculated yet
    ##
    ## Gets the cached original matrix
    data <- x$get()
    ## Calculates the inverse of the cached matrix
    m <- solve(data, ...)
    ## Caches the inverse matrix
    x$setinv(m)
    ## Returns the inverse matrix of 'x'
    m
}
