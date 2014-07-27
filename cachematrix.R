## R-Programming Course - Programming Assignment 2
## Matrix inversions can be expensive operations:
## The functions written will compute the inverse of a matrix and store it in cache.
## When the inverse of the same matrix is required during computations,
## the data can be retrieved from cache rather than computed over and over again.
## This improves performance of the process as well as saves resources such as RAM for other processes.

## makeCacheMatrix takes as its argument a matrix and
## creates a list containing a set of functions
## 1. getMatrix: this function simply returns the matrix
## 2. setMatrix: this function sets the value of the matrix on the "cache matrix object"
## 3. getInvertedMatrix: this function returns the inverted matrix for square matrices
##                       and a message that the matrix cannot be inverted for non square matrices
## 3. setInvertedMatrix: this function sets inverted matrix for square matrices
##                       or a message that the matrix could not be inverted for non square matrices
##                       on the "cache matrix object"
## this function, like in OOP, acts as a template for caching the inverted matrix
## TODO: Do not allow user to access $setInvertedMatrix(); 
##       this allows the computed inversion to be over-ridden by meaningless values set by a user
## *TODO = Nice to have, not part of requirement for this assignment*

makeCacheMatrix <- function(matrixToInvert = matrix()) {
  
    invMatrix <- NULL  
    setMatrix <- function(y) {
                     matrixToInvert <<- y
                     invMatrix <<- NULL  
                 }
  
    getMatrix <- function() {
                      matrixToInvert
                 }
  
    setInvertedMatrix <- function(inverted) {
                            invMatrix <<- inverted
                         }
  
    getInvertedMatrix <- function() {
                            invMatrix  
                          }
    
    ##return list of functions/getters-setters
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         setInvertedMatrix = setInvertedMatrix, 
         getInvertedMatrix = getInvertedMatrix)  

}


## This function does the computation needed for matrix inversion
## This function uses "solve" to find the inverse of a square matrix
## 1. The function works on a cache matrix object created by makeCacheMatrix (see above)
## 2. It first checks to see if the matrix already has its inverse stored in cache
##    Checks using: $getInvertedMatrix() on the input argument
## 3. If there is a cached value, computation is bypassed and the result is simply displayed
## 4. If there is no cached value, then the inverse of the matrix is determined using "solve"
## 5. There is a basic check to verify that the matrix is square and can indeed be inverted:
##    Note: not checking for determinant value of 0 here.
## 6. In case the matrix has different value for rows and cols, 
##    a message is stored instead of the inverted matrix
## Note: assignment assumes all matrices passed to this function will have an inverse
##       just a small attempt to implement a little more than requirement

cacheSolve <- function(matrixObj, ...) {
  
    invertedMatrix <- matrixObj$getInvertedMatrix()
    
    ##check cache first
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    
    ## get matrix from matrixObj
    matrixToInvert <- matrixObj$getMatrix()
    
    ## inverted matrix not found in cache
    ## compute inverted matrix and set it on the matrixObj
    invertedMatrix <- if (nrow(matrixToInvert) == ncol(matrixToInvert)) {
                          solve(matrixToInvert, ...)
                      } else {
                          "matrix is not square: inversion not possible"      
                      }
  
    matrixObj$setInvertedMatrix(invertedMatrix)
  
    ## return value
    invertedMatrix
}

##              --X--  Tests and outcomes  --X--

## Test case 1: Dataset is a valid square matrix

## Step 1: Create cahe matrix obj with list of getter/setter functions

## > m1 <- matrix(data=c(4,2,7,6), nrow=2, ncol=2)
## > m <- makeCacheMatrix(m1)
## > m$getMatrix()
##        [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > m$getInvertedMatrix()
## NULL
## Now that we have confirmed that m is an object that can be passed to cacheSolve, move on to Step 2

## Step 2: Call cacheSolve on m
## Expected Outcome: since the inverse of m has not been calculated even once, computation should occur

## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## Step 3: Call cacheSolve(m) again
## Expected Outcome: Inverse of m must be retrieved from m's cache

## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## Step 4: Just an extra step to verify that matrix product 
## of m and its inverse gives the identity matrix
## > m$getMatrix() %*% m$getInvertedMatrix()
##        [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## Test case 2: Dataset is not a  square matrix

## Step 1: Create a non-square matrix obj with list of getter/setter functions

## > n1 <- matrix(data=c(1,2,3,4,5,6), nrow=2, ncol=3)
## > n <- makeCacheMatrix(n1)
## > n$getMatrix()
##        [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
## > n$getInvertedMatrix()
## NULL
## The object has been created and we can see that the inverted matrix has not been set on the object

## Step 2: Call cacheSolve on n
## Expected Outcome: A message that indicates inversion could not be completed

## > cacheSolve(n)
## [1] "matrix is not square: inversion not possible"

## Step 3: Call cacheSolve on n again
## Expected Outcome: A message that indicates inversion could not be completed;
##                   However this time, this message is retrieved from the cache of n

## > cacheSolve(n)
## getting cached data
## [1] "matrix is not square: inversion not possible"

## Test Case 3: Other interesting outcomes
## In its current form, user can override the inverted matrix information other than cacheSolve
## For example in test case 2, n was not a square matrix. So inversion was not possible
## However one can make the following call and override the value in the cache:

## n$setInvertedMatrix("n is not a square matrix. But I can override that here")

## If we try to get this inverted matrix value, we retrieve from cache the value written by the 
## previous function call

## > n$getInvertedMatrix()
##[1] "n is not a square matrix. But I can override that here"

## Not if we call cacheSolve on n, this overriding value will be retrieved from cache!

## > cacheSolve(n)
## getting cached data
## [1] "n is not a square matrix. But I can override that here"

## As an improvement, one can restrict access to setInvertedMatrix, allowing only cacheSolve to
## set the value of the inverted matrix on the object

## Another interesting outcome is calling cacheSolve on matrices that are built on the fly
## i.e., anonymous matrices not assigned to any variable
## In this case even if the subsequent calls are all the same ones, there is still no caching
## since we are not storing the cache matrix object with its list of functions in any variable

## > cacheSolve(makeCacheMatrix(matrix(data=c(4,2,6,7), nrow=2,ncol=2)))
##         [,1]   [,2]
## [1,]  0.4375 -0.375
## [2,] -0.1250  0.250

## Calling cacheSolve again on a similar matrix will lead to computation and not cache retrieval

## > cacheSolve(makeCacheMatrix(matrix(data=c(4,2,6,7), nrow=2,ncol=2)))
##        [,1]   [,2]
## [1,]  0.4375 -0.375
## [2,] -0.1250  0.250

##    --X--   END OF TESTING    --X--