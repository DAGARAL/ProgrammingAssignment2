
makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL ## initialize inverted as void
  set <- function(y) {
    x <<- y  ## sends the value to the global environment (outside the function)
    inv <<- NULL ## sends the value to the global environment (outside the function)
  }
  get <- function() x
  setinverse <- function(inverse) inverted <<- inverse
  getinverse <- function() inverted
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  inverted <- x$getinverse()
  if(!is.null(inverted)) { # checks if the inverted already existis
    message("getting cached data.")
    return(inverted) # if it exists, returns the inverted matrix that is store
  }
 
  inverted <- solve(x$get()) # if it doesn't exists, then it calculates the inverse with solve()
  x$setinverse(inverted) # we set the inverse in the object X so next time the function knows that it is already done
  inverted
}
