# CacheMatrix will store the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) 
{
     # This function creates a special "matrix" object that can cache its inverse.
     inverseMat <- NULL
     initGlobalVars <- function(y)
     {
          # save initial matrix and create
          x <<- y
          inverseMat <<- NULL
     }
     getInitMatrix <- function()
     {
          # return initial matrix 
          x
     }
     StoreInverseMatrix <- function(im)
     {
          # set inverse matrix
          inverseMat <<- im
     }
     getInverseMatrix <- function()
     {
          # returns inverseMat.  If not see NULL will be returned
          inverseMat
     }
     
     list(initGlobalVars = initGlobalVars, 
          getInitMatrix = getInitMatrix,
          StoreInverseMatrix = StoreInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...) 
{
     # Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
     # If the inverse has already been calculated (and the matrix has not changed), then 
     # the cachesolve should retrieve the inverse from the cache.
     
     # assumptions: supplied matrix is always invertible
     
     ## Return a matrix that is the inverse of 'x'
     
     ############################################
     # check if inverse matrix exists.  If NULL is not returned then cached inverse matrix does exist
     ############################################
     inv_matrix <- x$getInverseMatrix()
     
     if(!is.null(inv_matrix))
     {
          message('Getting cached inverse matrix data')
          return(inv_matrix)
     }
     ############################################
     # if inverse matrix is not cached.  Cache it and return the inverse matrix
     ############################################
     
     # get init matrix
     init_M <- x$getInitMatrix()
     
     # make inverse matrix and cache it. 
     # Computing the inverse of a square matrix can be done with the solve function in R.
     inv_matrix <- solve(init_M, ...)
     
     # Cache inverse Matrix in obj
     x$StoreInverseMatrix(inv_matrix)
     
     # return inverse matrix
     inv_matrix
}

