## 1 makeCacheMatrix takes a matrix as an input and returns a special matrix 
##   which is really a list that containing four functions to 
##   set the value of the matrix, get the value of the matrix
##   cache the value inverse matrix, retrieve the cached inverse matrix
## 2 cacheSolve takes this special matrix (list) as input, checks for a 
##   previously cached inverse matrix, if that exists the inverse is retrieved 
##   from the cache and returned.  If not, it calculates the inverse, returns it 
##   and also commits the result to a cached object for retrieval if required again
##   If a cached value is being retrieved an informational prompt 
##   informs 'inverse matrix retrieved from cache'
##   If the cache is empty prompt informs 'inverse matrix calculated & cached'

makeCacheMatrix <- function(x = matrix()) {
     
     ## initialize variable for the inverse matrix
     inverse <- NULL
     
          ##set the matrix
          set <- function(newMatrix) {
               x <<- newMatrix
               inverse <<- NULL
          }
          
          ##get the matrix
          get <- function() {
               x
          }
          
          ##set the inverse of matrix
          setinverse <- function(invMatrix) {
               inverse <<- invMatrix
          }
          
          ##get the inverse of matrix
          getinverse <- function(){
               inverse
          }
     
     ##Return special matrix which is a list
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     
     ##check for cached inverse matrix, if it exists (not NULL) retrieve inverse 
     ##matrix from cache, return this inverse matrix and exit the function
     if(!is.null(inverse)) {
          message("inverse matrix retrieved from cache")
          return(inverse)
     }
     
     ##If inverse matrix was not found in cache above the remainder of the 
     ##function will execute
     
     #assign matrix to variable matrix1
     matrix1 <- x$get()
     
     #calculate the inverse matrix
     invMatrix <- solve(matrix1)
         
     #cache the inverse matrix in object called 'inverse'
     x$setinverse(invMatrix)
     message("inverse matrix calculated & cached")
     return(invMatrix)
}

##     TESTING THE FUNCTIONS
##     > x = matrix(c(rnorm(9)),3,3)
##     > a <- makeCacheMatrix(x)
##     > cacheSolve(a)
##     inverse matrix calculated & cached
##     [,1]        [,2]       [,3]
##     [1,] -0.4400941  0.60302996 -0.6519017
##     [2,] -0.1485670 -0.01535973  0.8003504
##     [3,] -0.3794228 -0.89301738  1.3947931
##     > cacheSolve(a)
##     inverse matrix retrieved from cache
##     [,1]        [,2]       [,3]
##     [1,] -0.4400941  0.60302996 -0.6519017
##     [2,] -0.1485670 -0.01535973  0.8003504
##     [3,] -0.3794228 -0.89301738  1.3947931
##     > round(z %*% x, 6)
##     [,1] [,2] [,3]
##     [1,]    1    0    0
##     [2,]    0    1    0
##     [3,]    0    0    1
