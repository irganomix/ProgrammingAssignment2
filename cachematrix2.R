# These functions create a special "matrix" from a user supplied matrix, that 
# can be cached, and then computes the inverse. If the inverse is already
# available in cache data, it returs the cached inverse and lets the user know

# This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y){
        x <<- y
        inv_mat <<- NULL
    }
    get <- function()x
    set_inv <- function(m) inv_mat <<- m
    get_inv <- function() inv_mat
    list (set = set, get = get, 
          set_inv = set_inv,
          get_inv = get_inv)

}


# This function checks if inverse is already available in cache data, and if not
# available, it will compute the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'   
        cacheSolve <- function(x,...){
    m <- x$get_inv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
    data <-x$get()
    m <-solve(data)
    x$set_inv(m)
    m
        }

}
