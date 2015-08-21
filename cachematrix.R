## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## The value of X have been set
        set <- function(y) {
        x <<- y
        m <<- NULL
        }

        ## The value of X have been recieved using get method
        get <- function() x

        ## We are setting the inverse value for matrix
        setinv <- function(sol) m <<- sol
         ## We are getting the inverse value for matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
} 


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()  ## getting the Inverse metrix value
        if(!is.null(m)) { ## returning the cached value if it is present
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(nrow(data) == ncol(data)){ ## Finding for the square matrix
            m <- solve(data, ...) ## Finding inverse of the matrix for the given data
            x$setinv(m)

        }else{
            message("Not a Square Matrix")
        }
        m   

} 
