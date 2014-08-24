## Set the folder containing "cachematrix.R" as working directory
##
## Load this script via source("cachematrix.R")
##
## Create your input matrix, e.g.
##
## myInputMatrix <- matrix(rexp(1000000), 1000, 1000)
##
## Then you should create a cachable matrix function object 
## for that matrix by either
##
## cachableMatrix <- makeCacheMatrix(myInputMatrix)
##
## or 
##
## cachableMatrix <- makeCacheMatrix()
## cachableMatrix$setInput(myInputMatrix)
##
## Then you can pass this function to cacheSolve, e.g.
## 
## cacheSolve(cachableMatrix)
##
## Calling cacheSolve(cachableMatrix) will return the cached
## result (indicated by printing "getting cached inversed matrix")


## Creates a wrapper function for the given matrix, that is able
## the store the given matrix as input. If no matrix is passed as
## argument an empty matrix is used as input.
## Passed to another function this function can store the result 
## of an operation applied to the input retrieved from the wrapper 
## function (via $getInput()) in this wrapper function as well 
## (via $setCacheResult(matrix)). As long as the input isn't changed 
## this stored result can be returned directly (via $getCacheResult()) 
## instead applying the operation over and over again. Calling
## $setInput(matrix) will change the input and thus clear the result.

makeCacheMatrix <- function(inputMatrix = matrix()) {
        
        cachedResultMatrix <- NULL
        
        setInput <- function(inputMatrixToSet) {
                inputMatrix <<- inputMatrixToSet
                cachedResultMatrix <<- NULL
        }
        
        getInput <- function() {
                inputMatrix
        }
        
        setCachedResult <- function(matrixResult) {
                cachedResultMatrix <<- matrixResult
        }
        
        getCachedResult <- function() {
                cachedResultMatrix
        }
        
        list(
                setInput=setInput, 
                getInput=getInput,
                setCachedResult=setCachedResult,
                getCachedResult=getCachedResult
        )
}

## This function takes a function to be created with makeCacheMatrix
## before. If the given input function holds already a cached result 
## (retrieved via cachableMatrix$getCachedResult) this is returned 
## otherwise the input (retrieved via cachableMatrix$getInput) is 
## passed to the solve function to calculate the result. This result 
## is then stored in the input function before it is returned.

cacheSolve <- function(cachableMatrix, ...) {
        cachedResultMatrix <- cachableMatrix$getCachedResult()
        if(!is.null(cachedResultMatrix)){
                message("getting cached inversed matrix")
                return(cachedResultMatrix)
        }
        inputMatrix <- cachableMatrix$getInput()
        resultMatrix <- solve(inputMatrix, ...)
        cachableMatrix$setCachedResult(resultMatrix)
        resultMatrix
}
