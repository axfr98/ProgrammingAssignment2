## The functins will create a matrix (invertable), that can cache its inverse. The second 
## function will verify if the inverse exists already in the cache. If it doesn't exist, 
## it will calculate it, otherwise, will print the value that is already in the cache

## This first function will create a "special" matrix that can cache its inverse:
## 1.- It sets the value of the matrix
## 2.- It gets the value of the matrix
## 3.- It sets the value of the inverse of the matrix
## 4.- It gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { #The function requires the use of the function matrix to create the matrix
  cache <- NULL
  setMatrix <- function(value){
    x <<- value
    cache <<- NULL
  }
  getMatrix <- function()x
  cacheInverse <- function(solve) cache <<- solve
  getInverse <- function() cache
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This second function act as follows:
## 1.- gets the inverse value calculated in the MakeCacheMatrix function
## 2.- checks if the inverse has been already calculated
## 3.- if the inverse has been calculated, showed a message and returns the value already
## calculated
## 4.- if the inverese was not calculated, it calculates it and stores it in the cache, 
## showing the message calculating inverse

cacheSolve <- function(y, ...) {
  inverse <- y$getInverse ()
  if(!is.null(inverse)){
    message("Getting cache data...")
    return (inverse)
  }
  dataforinv <- y$getMatrix()
  inverse <- solve(dataforinv)
  y$cacheInverse(inverse)
  message("Calculating inverse...")
  inverse
        ## Return a matrix that is the inverse of 'x'
}

# Run with the following example:
# <- test <- makeCacheMatrix( matrix(rnorm(9,10), nrow = 3, ncol = 3) )
#> test$getMatrix()
# [,1]      [,2]      [,3]
# [1,] 10.783604  9.354692  8.370208
# [2,] 11.410471 11.293316 10.008900
# [3,]  9.474378  8.628469 10.269261
# No cache in the first run:
# cacheSolve(test)
# Calculating inverse...
# [,1]       [,2]        [,3]
# [1,]  0.7639708 -0.6151411 -0.02314771
# [2,] -0.5765798  0.8110436 -0.32052562
# [3,] -0.2203808 -0.1139309  0.38804698

# And getting from cache in subsequent runs:
# cacheSolve(test)
# Getting cache data...
# [,1]       [,2]        [,3]
# [1,]  0.7639708 -0.6151411 -0.02314771
# [2,] -0.5765798  0.8110436 -0.32052562
# [3,] -0.2203808 -0.1139309  0.38804698