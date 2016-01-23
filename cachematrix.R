## As indicated, matrix inversion could be computationally expensive
## Using the <<- operator it is possible to store the inversions of a matrix for later use



## makeCacheMatrix creates a matrix and store it in the environment
## if the value is not null, the assumption being the matrix is relevant 
## for later use and should be cached accordingly

makeCacheMatrix <- function(x = matrix()) {
   myi<- NULL
   
   set <- function(m){
          if(is.null(m))
               x <<-m
   }
   get <- function() x
   
   
   setInverse <- function(myinverse) {
     if(is.null(myinverse))
       myinverse<- solve(x)
     
     myi <<- myinverse
   }
   
   getInverse <- function()  myi
     
   list (set=set,get=get, setInverse=setInverse,getInverse=getInverse)
  
}


## solve method can be used for computing the inverse of a square matrix
## saving the result of the inverse of a matrix can therefore be useful
## cacheSolve retrives the values of a saved result of an inverse of a matrix



cacheSolve <- function(x, ...) {
    
      ## Retrive the matrix from cache using getter method
      cachedMatrix <- x$get()
      
      ## Check if isn't null and its inverse has been computed
       if(!is.null(cachedMatrix)){
         
         if(is.null(x$getInverse())){
           
           x$setInverse(NULL)
         }

         return (x$getInverse())
       }
      ## Otherwise compute the inverse using solve method
      matrix_data <- x$get()

      
      inversedResult <- solve(matrix_data)
      ## And dont't forget storing it to the cache
      x$setInverse(inversedResult)
      
      inversedResult

}
