###############################################################################
## 该文件用于创建可缓存逆矩阵的特殊矩阵对象(n阶方阵)并求出其逆矩阵
##
###############################################################################



## makeCacheMatrix函数用于创建可缓存逆矩阵的特殊矩阵
makeCacheMatrix <- function(x = numeric()) {
	  m <- NULL

	  set <- function(y){
	      x <<- y
	      m <<- NULL
	  }

	  get <- function(){
	  	  x
	  }

	  setsolve <- function(solve){
	      m <<- solve(solve)
	  }

	  getsolve <- function(){
	  	  m
	  }

	  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


##cacheSolve函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
##如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵。
##solve (a) %*%a
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()

    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m<- solve(data)
    x$setsolve(m)
    m

}

