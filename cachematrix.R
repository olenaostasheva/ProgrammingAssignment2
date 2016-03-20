# Some of the functions contain time consuming computations and therefore 
#it is good to cache the results to safe time. The following functions compute and 
#cashe the inverse of a matrix. 

## Creates a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x=matrix()){
        inver<-NULL
        setinv<-function(y) {
                x<<-y
                inver<<-NULL
        }
        get<-function() x
        set_inverse<-function(inverse) inver<<-inverse
        get_inverse<-function() inver
        list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Computes the inverse of the matrix that was returned by makeCacheMatrix(). 
#If the inverse was already calculated and the matrix has not changed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$get_inverse()
        if(!is.null(inver)){
                message("getting cached data.")
                return(inver)
        }
        data<-x$get()
        inver<-solve(data,...)
        x$set_inverse(inver)
        return(inver)
}
}
