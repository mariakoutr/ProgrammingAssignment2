## set the value of a matrix, gets the value of the matrix
## sets the value of the inverse of the matrix
## gets the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x 
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set,get=get,
             setinverse=setinverse,getinverse=getinverse)

}


## Calculates the inverse of the matrix created with makeCacheMatrix. 
## However, it first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache. 

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        
        inv
}
