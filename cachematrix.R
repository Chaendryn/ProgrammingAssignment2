## The makeCacheMatrix function creates a special object that stores a 
## list of functions for use on matrix calculation, and the cacheSolve 
## function creates the inverse of the initial matrix and cache's this 
## inverse matrix


## The makeCacheMatrix function is creates a special matrix object which 
## is ## list containing four functions for matrix manipulation.

makeCacheMatrix <- function(myMatrix = matrix()) { 
      m <- NULL               ## initialises an empty matrix which will be used to store the inverse matrix data
      set <- function(y) {    ## creates the 'set' accessor function, which sets the value of the matrix
            myMatrix <<- y    ## Makes the 'set' function available to the other functions outside the 
                              ## environment of this function 
            m <<- NULL        ## Makes the initialised inverse storage matrix available to the cacheSolve function 
      }
      get <- function() myMatrix ## Creates the 'get' accessor function, which gets the value of the matrix
      setIMatrix <- function(solve) m <<- solve ## Sets the inverse matrix m, and makes the solve function 
                                                ## available outside the scope of this function
      getIMatrix <- function() m ## gets the value of the inverse matrix
      list(set = set, get = get, ## creates the function list with get, set, setIMatrix and getIMatrix
           setIMatrix = setIMatrix,
           getIMatrix = getIMatrix)
}


## The cacheSolve function takes as an input the special matrix object 
## created in makeCacheMatrix to calculate the inverse of that matrix 
## and stores the inverse matrix in cache.  If running the solve function 
## on the matrix, and the values have not changed, it pulls the inverse matrix
## data from cache instead of recalculating the matrix inverse again.

cacheSolve <- function(myMatrix, ...) {
      m <- myMatrix$getIMatrix()
      ## The if statement checks to see whether the inverse matrix has already been calculated and stored in cache.
      ## The inverse matrix object m will have values if already calculated.  The values are pulled from cache and 
      ## a message is printed to the console.
      if(!is.null(m)) {
            message("cached data retrieved") ## sets the message to be printed to the console
            return(m)         ## prints the cached inverse matrix to the console
      }
      data <- myMatrix$get()  ## retrieves the variables for the matrix with which the solve function will 
                              ## calculate the inverse
      m <- solve(data, ...)   ## computes the inverse matrix from data retrieved and writes it to m
      myMatrix$setIMatrix(m)  ## Sets the values computed by the solve function run on myMatrix
      m                       ## prints the inverse matrix to the console
      
}
