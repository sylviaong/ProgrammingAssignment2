## By : SylviaOng
## Date : 23 May 2015
## Function for Matrix inversion - caching the inverse of a matrix rather than computing it repeatedly

## MakeCacheMatrix creates a special matrix, which is really a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverst 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	

}


## Calculate the inverse of a special matrix created with the above function
## Check first if the inverse is already calculated If yes, it will get the inverse from cache and skip the computation


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  	inv <- x$getinv()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        inv
}
