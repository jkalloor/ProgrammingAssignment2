
## This function "makeCacheMatrix" creates   an inverse of matrix in cache,
# as way to cache potentially time consuming computations.  This take
# advantage of the scoping rules of R language and how they can 
# be manipulated to preserve state inside of an R object.

# set function set the matrix
# get function get the input matrix 
# setmatrix compute the inverse of matrix and using "solve" and 
# getmatrix get the inverse matrix form cache   
# cachesSolve  check if inverse  exits in cache. If it is not NULL it use
# it otherwise compute the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # im will hold the inverse matrix initially set NULL
        
        im<- NULL  
        # A set function, set a matrix to object 
        # note:`<<-` to assign a value to an objectin an environment 
        # different from the current environment.
        
        set<-function(y){
                x<<-y
                im<<-NULL
        }
        
        # get function get the input matrix 
        
        get<-function()x
        
        #  set the inversed matrix 
        
        setmatrix<-function(solve)im<<- solve
        
        getmatrix<-function()im
        # Preserve the list of the used functions.
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
        im<-x$getmatrix()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        matrix<-x$get()
        im<-solve(matrix)
        x$setmatrix(im)
        im
}

