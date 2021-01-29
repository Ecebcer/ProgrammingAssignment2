## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inversem <- NULL
   set <- function(y){
           x<<-y
           inversem<<-NULL
   }
   get<-function(){x}
   setInverse<-function(im){inversem<<-im}
   getInverse<-function() {inversem}
   list(set=set, get=get,
        setInverse= setInverse,
        getInverse= getInverse)
}

#Now we test it through a very simple example, using a 2 by 2 matrix.
x<-makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
x$get()

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse<-x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<- solve(data,...)
        x$setInverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

#Following with the previous example, we have..
cacheSolve(x)
