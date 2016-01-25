## Part One: Creating a 2 by 2 matrix 
## After the 2 by two matrix is created. The function creates a special matrix object. 

a <- makeCacheMatrix ()
a$set(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## Part Two. Solving for the inverse of said matrix. 
## This function calculates the inverse of the matrix. If the inverse of the matrix 
## has already been calculated then the fucntion will retrive it from the cache instead of calculating it again.  

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get ()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Run the following line to get the Solution 

cacheSolve(a)

## The solution itself 

     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


