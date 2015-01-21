#               Coursera R Programming class
#               Programming Assignment 2
# Function:  makeCacheMatrix
# Date:      January 2015
# Note: The input matrix for this function is assumed to be invertible.
#
# ----------     Example of using the function     --------------#
#  > x<-rbind(c(1, -.1, .2), c(-.1, 1, .3), c(-.2, .3, 1))
#  > m <- makeCacheMatrix(x)

#-----------------------------------------------------------------#
makeCacheMatrix <- function(x = matrix()) 
{
  # The makeCacheMatrix defines four functions:
  # 1. set (a matrix)
  # 2. get (the matrix)
  # 3. set (the inverse of the matrix)
  # 4. get (the inverse of the matrix)
  
  # Initiallize
  inv <- NULL  
  # --- set function ---
  set <- function(y) {
    x <<- y
    inv <<- NULL   
  }
  # --- get function ---
  get <- function() x 
  # --- set the inverse ---
  setinverse <- function(inverse) inv <<- inverse
  # --- get the inverse ---
  getinverse <- function() inv
  # --- put the functions in a list ---
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
} # ----------------- end of makeCacheMatrix() ------------------#



# Function:   cacheSolve
# Date:        January 2015
# Notes: This function returns a matrix that is the inverse of 'x'
# The function avoids the re-computing the inverse matrix
# and obtains the already computed one, if it exists.
#
# --------     Example using the function     ----------
#  > x<-rbind(c(1, -.1, .2), c(-.1, 1, .3), c(-.2, .3, 1))
#  > x
#       [,1] [,2] [,3]
#  [1,]  1.0 -0.1  0.2
#  [2,] -0.1  1.0  0.3
#  [3,] -0.2  0.3  1.0
#  m <- makeCacheMatrix(x)
#  > cacheSolve(m)
#  Computing the inversed matrix ... 
#          [,1]    [,2]    [,3]
#  [1,] 0.96809  0.1702 -0.2447
#  [2,] 0.04255  1.1064 -0.3404
#  [3,] 0.18085 -0.2979  1.0532
#  > cacheSolve(m)
#  Obtaining the cached inversed matrix ...  
#          [,1]    [,2]    [,3]
#  [1,] 0.96809  0.1702 -0.2447
#  [2,] 0.04255  1.1064 -0.3404
#  [3,] 0.18085 -0.2979  1.0532
#  Please note! One can verify the operations.
#  > inv_matrix <- cacheSolve(m)
#  Obtaining the cached inversed matrix ... 
#  > inv_matrix %*% x
#             [,1]      [,2]      [,3]
#  [1,]  1.000e+00 2.776e-17 2.776e-17
#  [2,]  1.388e-17 1.000e+00 0.000e+00
#  [3,] -2.776e-17 0.000e+00 1.000e+00
 
#-----------------------------------------------------------------#
cacheSolve <- function(x, ...) 
{
  # The function returns a matrix that is the inverse of the input 'x'
  # The function expects that the matrix 'x' is invertible.
  
  # Get the state to see if the inverse has been computed yet
  inv <- x$getinverse()    
  # If it has been computed, return it 
  if(!is.null(inv)) 
  {
    # Notify the user		
    message("Obtaining the cached inversed matrix ... ")
    # Return the inverse
    return(inv)
  }  
  # If the inverse was not computed, get the matrix and find its inverse 
  data <- x$get()
  message("Computing the inversed matrix ... ")
  inv <- solve(data, ...)  
  # Cache this result in the object
  x$setinverse(inv)  
  # Return the new result
  inv    
} #---------------- end of cacheSolve() -------------------------#
