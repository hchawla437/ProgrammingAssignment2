## First function will make a list of methods and initialize the variabe in parent enviroment

## The second function is passed the list from the first and attempts to calculate and set its inverse.

## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL  # initializing inverse
  
  # set the matrix in the parent enviroment
  
  set<-function(y=matrix()){
    x<<-y
    inv<<-NULL
  }
  # get the matrix
  
  get<-function() x
  
  # set inverse variable in parent enviroment
  setinv<-function(invert){
    inv<<-invert 
    return(inv)
  }
  getinv<-function() inv
  
  
  list(set=set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will return the cached inversion of a matrix if present otherwise will create a new one and will store for next return

cacheSolve<-function(x=matrix(),...){
  
  inv<-x$getinv()
  
  # if inverse is already present then return that only
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if inverse is not present than find inverse of matrix first than return it
  
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}


