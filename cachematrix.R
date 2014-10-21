## Using two fuction "makeCacheMatrix" & "cacheSolve" 
## to Caching the inverse of a matrix

## makeCacheMatrix make input "matrix" a list containing a function below
##   set(): set the value of the vector
##   get(): get the value of the vector
##   setinverse(): set the value of the mean
##   getinverse(): get the value of the mean
##   And having "x=input", "inverse" = NULL in this enviroment

makeCacheMatrix <- function(x = matrix()) {
        ## make this contain "x <- input" in list enviroment
        inverse <- NULL
        set <- function(y= matrix(),...){
                x <<- y
                inverse <<- NULL
        }
        
        get <-function() x
        setinverse <- function(inverse_ans) inverse <<-inverse_ans
        getinverse <- function() inverse
        
        list (set = set, get = get,
                setinverse = setinverse, 
                getinverse = getinverse)

}


## cacheSolve using the result "List" of above function by the following step:
##   Remember here free veariable in "List$function()" will search
##   the enviroment where it define, in other words, List enviroment.
## 1.Using "if" to know whether getting cached data or not
## 2.if it didn't, calaulate it by solve function to get answer
## 3.put the result into List enviroment, and it can get cache data nexttime

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <-x$getinverse()
        
        if (!is.null(inverse)){
                print ("getting cached data")
                return(inverse)
        }
        
        data<- x$get()
        inverse <-solve(data,...)
        
        x$setinverse(inverse)
        
        inverse
        
        
}
