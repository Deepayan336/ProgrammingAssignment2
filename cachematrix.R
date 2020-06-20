## These functions are used to compute the inverse of the matrix using the cache that 
## can be saved in the memory. If later the inverse to need to be found for the similar matrix then 
## it can be retrieved from the saved cache.


## This function creates a special "matrix" object that can cache its inverse
makeCachematrix <- function(x =matrix()) {
              inv <- NULL 
              set <- function(y) {
                x <<- y
               inv <<- NULL
              }
                get <- function() (x)
                setInverse <- function(inverse) (inv <<- inverse)
                getInverse <- function() {inv}
                list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cachesolve <- function(x,...) {
          inv <- x$getInverse()
          if(!is.null(inv)){
             message("getting cached data")
             return(inv)
            }
          mat <- x$get()
          inv <- solve(mat,...)
          x$setInverse(inv)
          inv
}

#output

pmatrix <- makeCachematrix(matrix(1:16, nrow=4 ,ncol=4))
> pmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16
> pmatrix$getInverse()
NULL
> pmatrix <- makeCachematrix(matrix(1:4 ,nrow=2,ncol=2))
> pmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> pmatrix$getInverse()
NULL
> cachesolve(pmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cachesolve(pmatrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> pmatrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


