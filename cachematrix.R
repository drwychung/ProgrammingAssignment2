## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# Parameters: 
# x - invertible matrix*
#
# * if not provided, $set() must be used to set the matrix to 
#   be inverted before using cacheSolve()
# 
# Assumption: 
# 1) user installed and loaded matlib. If not, 
#
#    install.packages('matlib') 
#    library(matlib) 
#
# 2) User-supplied arguments meet the following criteria
#    (matrix and invertible): 
#
#    is.matrix(x) & (det(x) != 0) & (dim(x)[1] == dim(x)[2])
#

makeCacheMatrix <- function(x = matrix()) {
        mat  <- x
        mati <- NULL 
        
        set <- function(m) {
                if (!identical(m,mat)) {
                        mat <<- m 
                        mati <<- NULL
                } # do nothing if setting the same matrix
        }
        
        get <- function()  mat
        setinv <- function(mi = NULL) mati <<- mi
        getinv <- function() mati 
        list(
                set=set, get=get, 
                setinv=setinv, getinv=getinv
        )
}


## Write a short comment describing this function
#
# Parameters: 
#   x - x object created using makeCacheMatrix() function above. 
# 
# Returns: 
#   inverted matrix of the matrix stored in x object*
#
# * x$get() %*% cachSolve(x)    produces identity matrix
#   sum( x$get() %*% cachSolve(x) ) == dim(x$get())[1] 
#
# If the matrix has not been changed since the inverted matrix is 
# calculated, it will retrieve "cached" inverted matrix.
# 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mati <- x$getinv()
        if (is.null(mati)) {
                if ( identical(x$get(), matrix()) ) 
                        warning('Must set the matrix to be inverted.')
                else mati = inv(x$get()) 
        }
        x$setinv(mati)
        mati
}


