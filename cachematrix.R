## Tanpreet Gambhir
## These functions are able to compute and cache the inverse of a matrix. The first function
## stores the matrix and its inverse while the other checks and returns the cached inverse
## or computes it and sets in the cache

## Function creates a 'special' matrix object which stores the set and get values of the passed matrix,
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #placeholder
        set <- function(y) {
                x<<-y #assign new matrix value
                inverse<<-NULL #set to null
        }
        get <- function() x #return matrix
        setinverse <-function(x) inverse <<- solve(x) #finds inverse of x
        getinverse <-function() inverse #returns inverse of x
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #special matrix object
}


## This function first checks if the inverse is already stored in the cache and return it from
## the cache if it is.
## If not, the inverse is calculated, and sets it in the cache as well (by calling the setinverse)
## The inverse is then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() #cached inverse
        if(!is.null(inverse)){ #check if cached inverse exists
                message("Getting cached matrix inverse")
                return(inverse) #returns cached inverse
        }
        data<-x$get() #get new matrix 
        inverse <- solve(data) #find matrix inverse
        x$setinverse(inverse) #set matrix inverse to cache
        inverse #return inverse
}

