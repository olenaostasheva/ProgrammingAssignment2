#Assignment 2 - Inverse matrix


#Function 1
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
#Function 2

cacheSolve<-function(x, ...){
        inver<-x$get_inverse()
        if(!is.null(inver)){
                message("getting cached data.")
                return(inver)
        }
        data<-x$get()
        inver<-solve(data)
        x$set_inverse(inver)
        inver
}

