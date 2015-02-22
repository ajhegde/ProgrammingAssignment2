## The following two functions make use of the lexical scoping rules
## in R, by making use of the <<- operator. With the help of 
## makeCacheMatrix, the user is able to input a matrix in the function,
## and the inverser of the matrix will be stored in cache. 
## The function cacheSolve() works in conjunction to makeCacheMatrix
## in that it will return cached results if the matrix inputted is the
## same, but will re-calculate the inverse if it isn't.


## Upon calling the following function with a matrix as an argument,
## it calculates the inverse of that matrix with the solve() function,
## and stores it in cache using the setinverse() function.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function(){
                x
        }
        setinverse<- function(solve){
                s <<- solve
        }
        getinverse <- function(){
                s
        }        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
               
        }



## The following function first checks to see if the inverse
## of the matrix is stored in cache (by makeCacheMatrix()). If it isn't,
## the function will re-calculate the inverse using the solve() function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}