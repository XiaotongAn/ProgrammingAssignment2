## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## The following functions compute the inverse of a matrix.
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

##Sample Test
#> x = rbind(c(1, -1/2), c(-1/2, 1))
#> r = makeCacheMatrix(x)
#> r$get()
#     [,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0
#> cacheSolve(r)
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#> 






