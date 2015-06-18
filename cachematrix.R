## Make a container that stores a matrix, can compute its inverse,
## and caches that inverse until the matrix is re-set

## Create a matrix container that caches its inverse until changed
## makeCacheMatrix is the closure of functions setMatrix, getMatrix, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
   x.inverse <- NULL

   ##Set the matrix value and null out the inverse at parent environment level
   setMatrix <- function(y) {  
     x <<- y
     x.inverse <<- NULL
   }
   
   ##Get the matrix itself
   getMatrix <- function() x  
   
   ##Set the inverse
   setInverse <- function(y) x.inverse <<- y
   
   ##Get the inverse
   getInverse <- function() x.inverse
   
   ##return a list of setter/getter functions
   list(setMatrix = setMatrix, 
        getMatrix = getMatrix, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## cacheSolve takes the list object returned by makeCacheMatrix
## and implements cached getting

cacheSolve <- function(x, ...) {
        MatrixInverse <- x$getInverse()
        if(is.null(MatrixInverse)) {
           message("Matrix inverse not yet computed; computing now")
           MatrixInverse <- solve(a<-x$getMatrix(), b<-diag(nrow(x$getMatrix())) )
           x$setInverse(MatrixInverse) 
        }
        return(MatrixInverse)
}
