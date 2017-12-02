## For makeCacheMatrix - A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse.

## Intialize the inverse property
makeCacheMatrix <- function(m = matrix() ) {
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function () {
    
    ## Return the Matrix
    
    m
  }
  
  ## Method to set the inverse of the matrix
  setinverse <- function (inverse) {
    inv <<- inverse
  }
  
  ## Method to set the inverse of the matrix
  
  getinverse <- function () {
    
    ## Return the inverse property		
    
    inv
  }	
  
  
  ## Return a list of the methods	
  
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse )
  
}

## For cacheSolve - Compute the inverse of the special matrix returned by "makeCacheMatrix" above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <-x$getinverse()
  
  ## If inverse exist
  if(!is.null(m)) {
    message("getting cached data")	
    return (m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setinverse(m)
  
  ## Return the matrix
  m
}

## EXAMPLE
## m <- matrix(1:4, nrow=2, ncol=2)
## x <- makeCacheMatrix(m)
## s <- cacheSolve(x)
## print(s)

## Example (Getting Cached Data)
## s2 <- cacheSolve(x)
## "getting cached data"
## print(s2)