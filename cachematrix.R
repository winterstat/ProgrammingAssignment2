## These two functions work together to cache the 
## inverse of a matrix. The assumption is that all
## matrices that are supplied are invertible.


## The first function creates a list containing a function
## to set the value of a matrix, get that value, set the value
## of the inverse matrix, and get that inverse matrix. The 
## first time this function runs, it will not return the inverse
## matrix, but a list of functions that can be used to calculate it.

makeCacheMatrix <- function(x = matrix()) {
        # create an empty object in the function environment that will hold
        # the inverse matrix. Makes such that m is NULL each time the function
        # is called.
        
        m <- NULL
        
        # a function that sets the value of the matrix in the global 
        # environment to y, and sets m to NULL. This function
        # is not called by cacheSolve, but can be used to change the 
        # original matrix x without rerunning makeCacheMatrix().
        
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        
        # a function to get the value of the original matrix. 
        
        get <- function() x
        
        # a function to set the inverse of the matrix. This function changes the
        # meaning of m in the global environment to solve (first it was NULL).
        
        setsolve <- function(solve) m <<- solve
        
        # a function to get the inverse of the matrix. This function calls the
        # m object, that now means solve.
        
        getsolve <- function() m
        
        # a list containing all the functions defined above so they can be called
        # by the cacheSolve() function.
        
        list(set=set, get=get, setsolve = setsolve,
             getsolve=getsolve)
}


## This second function takes as its argument the output
## of the first function, taking all the arguments of the original function (...)
## it then check whether the original function has already computed the inverse
## of the matrix. If so, it will return the cached matrix. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse matrix in the cache 
## via the setsolve function. Next time the function is called, it will get the
## cashed inverse matrix and it will be much faster.

cacheSolve <- function(x, ...) {
        
        # get the information that is in getsolve() part of the list returned 
        # by makeCacheMatrix()
        
        m <- x$getsolve()
        
        # if the information now in m (in the function environment) is not NULL
        # then return the cached inverse matrix.
        
        if(!is.null(m)) {
                message("getting cashed data")
                return(m)
        }
        
        # if m is NULL then get the original matrix from the get() part 
        # of the list returned by makeCacheMatrix()
        
        data <- x$get()
        
        # set m to solve(data, ...), that is, create the inverse matrix
        
        m <- solve(data, ...)
        
        # set the setsolve() part of the list returned by makeCacheMatrix()
        # to the value of m (the inverse matrix)
        
        x$setsolve(m)
        
        # return m (the inverse matrix)
        
        m
}
