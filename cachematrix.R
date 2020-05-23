#makeCacheMatrix calculates and stores the inverse in cache 
#cacheSolve checks if the inverse is already calculated and is
#present in cache... if yes, it gets the inverse from cache


#Function which creates a special vector i.e. a list containing
#functions to set, get, set inverse and get inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

    # set the values of the new matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # output the matrix
    get <- function() x

    #find the inverse and store 
    setinv <- function(inverse) inv <<- solve(x)

    #get the stored inverse
    getinv <- function() inv
    list(set=set, get=get,
    setinv= setinv,
    getinv= getinv)

}


#if the inverse is already calculated... get it from cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv()

    #if inv returned is not null return the cached value
    if(!is.null(inv)){
        message("Getting From cache")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
