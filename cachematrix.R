

## Code to  firstly set and get the value of a matrix and then also set and get the inverse of 
## the same matrix and storing it and caching it so we can access it later

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setInverse <- function() inv <<- solve(x) #calculate the inverse
      getInverse <- function() inv              #cache the inverse value
      
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse was already calculated. If so, it takes the inverse from the cache and 
##skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
  
        inv <- x$getInverse()         ##if inverse already calculated then it takes this value  
        
        if(!is.null(inv)){
              
             
                message("Inverse already calculated: ")      ## If inverse is already calculated return and skip computation
                return(inv)    
   
        }
        
                                        ## otherwise calculate it and return it
        inv <- solve(x$get())
        
        x$setInverse()
        
        return(inv)
        
}
