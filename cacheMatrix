##makeCacheMatrix is a function consisting methods get, set, setInv, getInv
##we can use library(MASS) to find an inverse, but it's not mandatory. 
##We first initialize inverse as NULL, then define a method that obtains inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i<- NULL               
    set <- function(y){
                      x <<- y
                      i<<- NULL
    }
    get <- function()x               
    setInv <- function(inverse) i <<- inverse
    getInv <- function() {
                          inver<-ginv(x)
                          inver%%x
                          
    }
    list(set = set, get = get, 
         setInv = setInv, 
         getInv = getInv)
  }

## cacheSolve is for getting cached data
##we check for inverse not to be null and return it with a small message
cacheSolve <- function(x, ...) {
  i <- x$getInverse()           
  
  if(!is.null(i)){            
                  message("Let's get cached data!")
                  return(i)                       
  }
  m <- x$get()
  i <- solve(m,...)
  x$setInv(i)
  i
        
     }
