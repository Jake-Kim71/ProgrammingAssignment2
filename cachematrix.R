
## By Jake Kim - Coursera R-programming wk3 assignment

## makeCacheMatrix funtion:  
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv_matrix <- NULL                                         # initialize the inverse matrix as NULL
    set <- function(y) {                                       # set the value of the matrix 
    x <<- y                       
    inv_matrix <<- NULL
    }
  
    get <- function() x                                        # get the matrix from above set function
    set_inverse <- function(inverse) inv_matrix <<- inverse    #set the inverse matrix of the given matrix
    get_inverse <- function() inv_matrix                       # get the inverse matrix of the given matrix 
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
   
  
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    inv_matrix <- x$get_inverse()          
    if(!is.null(inv_matrix)) {               # if the inverse matris is not null(if there is a value in cache)
      message("getting cached data")         # print Message getting cached data"
      return(inv_matrix)                     # retrun the inverse matrix
    }
    data <- x$get()                          # if there is no value in the cache, get the input matrix data
    inv_matrix <- solve(data, ...)           # get the inverse matrix from solve function 
    x$set_invers(inv_matrix)                 # set the inverse matrix
    inv_matrix                               # return the inverse matrixx
}
