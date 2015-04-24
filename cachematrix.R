## These 2 functions work togheter to create a "special matrix" that
## is able to cache its inverse.

## makeCachematrix creates "special matrix functions":
## $set - set matrix and store it
## $get - get stored matrix
## $setinverse - set matrix' inverse and store it
## $getinverse - ger matrix'inverse

## "<<-" allows to assign a value in the makeCacheMatrix() environment when
## used inside the subfunctions

makeCacheMatrix <- function(x = matrix()) {     # argument is a matrix
                
        x.inverse <- NULL                       # initialize x.inverse as NULL
        
        set <- function(y = matrix()) {	        # function $set to set a matrix	
                
                x <<- y			        
                             
                x.inverse <<- NULL	        
                
        }
        
        get <- function () x		        # get the value of the matrix x	
        
        setinverse <- function(inverse) x.inverse <<- inverse	# set inverse
        
        getinverse <- function() x.inverse	# get inverse
        
        # makecacheMatrix gives this output which is a list of functions
        # that can be called individually: $set, $get ...
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cachesolve function gets makeCacheMatrix output to compute the inverse of
## the stored matrix. If it is the first time that the inverse has to be computed
## cachesolve will read that there is no inverse stored (NULL) and will calcultate
## it by means of the solve() function. Adter that it will call makeCacheMatrix()
## again to store it. Next time the inverse has to be computed, cacheSolve() will
## be able to read it and then it will just return it without recomputing again.

cacheSolve <- function(x, ...) {        # argument is the output of makeCacheMatrix()
        
        x.inverse <- x$getinverse()     # read inverse  
        
        if(!is.null(x.inverse)) {       # if inverse is NOT null just return it 
                
                message("getting cached data")	
                
                return (x.inverse)
                
        }
                
        m <- x$get()	                # if inverse is null read the matrix	
        
        x.inverse <- solve(m, ...)	# solve (inverse) the matrix
        
        x$setinverse(x.inverse)		# store it
        
        x.inverse                       # return the inverse

}
