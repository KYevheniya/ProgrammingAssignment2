## Put comments here that give an overall description of what your
## functions do
## The main objective is to learn how to assign a value to an object in an environment that is different from the current environment with the end goal to save time and computing power. 
## The functions below create a special object that stores a matrix vector and cache's its inverse.

## Write a short comment describing this function
## Firstly, makeCacheMatrix creates a special matrix which assigns and fetches the matrix, assigns and fetches the the inverse of the matrix.  


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## Secondly, cacheSolve checks to see if the inverse has already been calculated. 
## If it has been calculated, it gets the inverse matrix from the cache and skips the computation thus saving time and computation power.
## If not, it calculates the inverse of the matrix and sets the values of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
            message("fetching pre-calculated matrix")
            return(m)
        }
        data <-x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
