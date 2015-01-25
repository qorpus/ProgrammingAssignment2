## Creates a cache object for the matrix
makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() return(x)
  setcache <- function(cache) mtx <<- cache
  getcache <- function() return(mtx)
  list(set = set, get = get, setcache = setcache, getcache = getcache ) 
}

## solves the inverse of the matrix, caches the answer in the environment for fast retrieval later
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtx <- x$getcache()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data)
  x$setcache(mtx)
  mtx

}
