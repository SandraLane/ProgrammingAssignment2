

## This assignment caches an inverse of a matrix rather than computing it repeatedly
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}
}

## This function computes the inverse of the `makeCacheMatrix`, and `cacheSolve` gets the inverse from the cache if the
   inverse has already been calculated. 
                
cacheSolve <- function(x, ...) {
        ##  returns a matrix
        
        
        inv <- x$getInv()
        if(!is.null(inv)) {
        	   message('getting cached data')
        	   return(inv)
        	   
        }
        ## calculates inverse
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInv(inv)
        return(inv)
}
                
}
