## makeCacheMAtrix is use to set up getters and setters for inversing the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Function to return inverse of matrix. If inverse already computed then it would display cached
## results, else it will compute the inverse and display

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
  
}


## Testing the function created

set.seed(1)
r = rnorm(100)
mat1 = matrix(r, nrow=10, ncol=10)


m1 <- makeCacheMatrix(mat1)
cacheSolve(m1)
