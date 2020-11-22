
# Two  funtions, makeCacheMatrix and cacheSolve that calculate the inverse of a given matrix.

#Matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              #Method to get the matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #Method to set the inverse of the matrix
  getInverse <- function() invMatrix                     #Method to get the inverse of the mtrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,     
       setInverse = setInverse, getInverse = getInverse) #Returns a list of the methods
  
}


#If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return's the inverse matrix of x
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Matrix")  
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     #get original Matrix
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the inverse 
  return(invMatrix)                               
}

#Testing the function

M1 <- matrix(1:4,2,2)
M1

CacheM1 <- makeCacheMatrix(M1)
CacheM1$getMatrix()
CacheM1$getInverse()
cacheSolve(CacheM1)

--------------------------------
  
  
  M2 <- matrix(c(1,5,8,2),2,2)
M2

CacheM2 <- makeCacheMatrix(M2)
CacheM2$getMatrix()
CacheM2$getInverse()
cacheSolve(CacheM2)

--------------------------------
  
  
  M3 <- matrix(1:8,3,3)
M3

CacheM3 <- makeCacheMatrix(M3)
CacheM3$getMatrix()
CacheM3$getInverse()
cacheSolve(CacheM3)


