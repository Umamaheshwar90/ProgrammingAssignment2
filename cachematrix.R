## First, a function will be defined which will take a matrix as its input and will define various functions that will be called when we are try to find the inverse of a matrix.


## Here we input Matrix x. 
## Set M to NULL so that when we try to call inverse of a matrix from cache, if its not present, the cache has to return NULL.
## Define setinv function which will assign the calculated inverse to setinv element of the list.
## Define getinv function which will be called when we try to find the inverse of a matrix

makeCacheMatrix <- function(x=matrix()) {
  m=NULL
  get <- x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

## Here we input the list returned by makeCacheMatrix function.
## Check if there is already a inverse using the getinv element of makeCacheMatrix function and return the value if it is present.
## If not, we will create one and call the setinv element of makeCacheMatrix function which will assign the value to variable through a global operator '<<'.
## So when you again try to find the inverse of the same matric, it will search for the inverse using the getinv function and thereafter does same proces mentioned above.

cacheSolve=function(x){
  m=x$getinv()
  if(!is.null(m)){
    message("return the cached inverse of the matrix")
    return(m)
  }
  
  a=x$get
  d=nrow(a)
  idm=diag(d)
  m=solve(a,idm)
  x$setinv(m)
  m
  
}
