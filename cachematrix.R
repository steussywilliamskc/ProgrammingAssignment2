## The first function - makeCacheMatrix - ideally does what the function implies. It stores into cache memory a matrix.
## It is a list that essentially gets the value of a vector, set the value of the matrix and then gets the values of the matrix.
## The second function - cacheSolve - simply gets a matrix, however if it was already created and cached in memory then it skips over that computation.

## The first function - makeCacheMatrix - ideally does what the function implies. It stores into cache memory a matrix.
## It is a list that essentially gets the value of a vector, set the value of the matrix and then gets the values of the matrix.

makeCacheMatrix <- function(x=matrix()){
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## The second function - cacheSolve - simply gets a matrix, however if it was already created and cached in memory then it skips over that computation.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
