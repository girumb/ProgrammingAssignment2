## As indicated, matrix inversion could be computationally expensive
## Using the <<- operator it is possible to store the inversions of a matrix for later use



## makeCacheMatrix creates a matrix and store it in the environment
## if the value is not null, the assumption being the matrix is relevant 
## for later use and should be cached accordingly

makeCacheMatrix <- function(x = matrix()) {
   
   set <- function(m){
          if(is.null(m))
               x <<-m
   }
   get <- function() x
   
   
   list (set=set,get=get)
  
}


## solve method can be used for computing the inverse of a square matrix
## saving the result of the inverse of a matrix can therefore be useful
## cacheSolve retrives the values of a saved result of an inverse of a matrix



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedMatrix <- x$get()
       if(!is.null(cachedMatrix))
          return  (solve(cachedMatrix))
    
    NULL
}
