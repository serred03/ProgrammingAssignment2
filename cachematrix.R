#Creates a "matrix" object capable of storing values on different lexical scopes. 
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL 
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#Solves the inverse of a matrix if there is no cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        
        if(!is.null(inv)){
                message("getting cached data ")
                return(inv)
        }
        
        data <- x$get()
        
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        
}
