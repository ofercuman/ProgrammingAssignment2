
# makeCacheMatrix: 
# when called, will create an object of type list,
# which contains 'x' (the original input matrix) 
# and 'inv' (the inverse of the matrix that will be passed in, initialized as NULL)

makeCacheMatrix <- function(x = matrix()) {     # input x will be a matrix

        inv <- NULL                             # inv will be our 'inverse' 
                                                # it's reset to NULL every time makeCacheMatrix is called
        
                                                # note these next three functions are not run when makeCacheMatrix is called 
                                                # instead, they will be used by cacheSolve() to get values for x or for inv (inverse)
                                                # and for setting the inverse. These are usually called object 'methods' 
        
        set <- function(y) {                    # takes an input matrix
                x <<- y                         # saves the input matrix
                inv <<- NULL                    # resets the inverse to NULL, basically what happens when a new object is generated
        }
        get <- function() x                     # this function returns the value of the original matrix
        
        setinverse <- function(solve) inv <<- solve    # this is called by cacheSolve() during the first cacheSolve() access
                                                       # it will store the value using superassignment
        
        getinverse <- function() inv            # this will return the cached value to cacheSolve() on subsequent accesses
        
        list(set = set, get = get,              # this is accessed each time makeCacheMatrix() is called,
             setinverse = setinverse,           # that is, each time we make a new object. This is a list of 
             getinverse = getinverse)           # the internal functions ('methods') so a calling function
                                                # knows how to access those methods. 

}


# cacheSolve:
# when called, return a matrix that is the inverse of 'x'
# 'x' is the original input matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {                # the input x is an object created by makeCacheMatrix

        inv <- x$getinverse()                   # accesses the object x, using makeCacheMatrix, and returns the inverse of x, which equals inv
        
        if(!is.null(inv)) {                     # if object x and its inverse inv are stored in makeCacheMatrix (non-NULL value), 
                message("getting cached data")  # then returns message
                return(inv)                     # and returns previously stored inverse inv for x
        }                                       # function stops here for non-NULL value
        
        data <- x$get()                         # for NULL values, fetch original matrix x
        inv <- solve(data, ...)                 # calculate inverse of that matrix x, pass into inv
        x$setinverse(inv)                       # store inverse inv of x into makeCacheMatrix list
        inv                                     # returns inverse inv of x              
}
