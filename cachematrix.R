## solucion de vector 
makeCacheMatrix <- function(t = matrix()) {
  s <- NULL
  set <- function(i){
    t <<- i
    s <<- NULL
  }
  get <- function()t
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## 
cacheSolve <- function(x, ...) {

  s <- t$getInverse()
  if(!is.null(s)){
    message("obtener datos del cache")
    return(s)
  }
  mat <- t$get()
  s <- solve(mat,...)
  t$setInverse(s)
  s
}