## The following functions cache the inverse of a matrix



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(data, row, col) {     ## define function with inputs of data, row, and column
     x <- matrix(data, row, col)
     m <- NULL
     set <- function(nd, nr, nc) {
          x <<- matrix(nd, nr, nc)
          m <<- NULL                              ## NULLs cached value when matrix re-set
     }
     get <- function() x
     setinverse <- function(minverse) m <<- minverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinverse()                          
     if(!is.null(m)) {                            ## check whether inverse already calculated
          message("getting cached data")
          return(m)
     }  else
                                                  ## calculate & set inverse if not cached
          message("calculating inverse")
          matrix <- x$get()
          m <- solve(matrix,...)
          x$setinverse(m)
          m
}
