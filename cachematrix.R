# Assign the result of makeCacheMatrix to a variable (let's call it "x")
# makeCacheMatrix will need a matrix to be passed as argument
# Running cacheSolve with x as an argument will give you the inverse of the original matrix
# This process should run faster than simply applying the solve function to your matrix in case you need this inverse several times
# Indeed, once the inverse of your matrix is calculated, it can be found again in the cache and doesn't need to be computed

#Caution: these functions work for a square matrix only

makeCacheMatrix <- function(x = matrix()) {
    
  m<-NULL
  
  #Set the value of the matrix
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    #Get the value of the matrix
    get<-function() 
    x
    
    #Set the inverse of the matrix
    setinv<-function(inv) 
    m<<-inv
    
    #Get the inverse of the matrix
    getinv<-function() 
    m
    
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


# Function based on the vector mean example - feel free to change the message between quote marks
cacheSolve <- function(x, ...) {
    
  m<-x$getinv() #getinv defined in makeCacheMatrix
    
  if(!is.null(m)){
        message("getting cached data")
        return(m)
  }
  
  data<-x$get() #get defined in makeCacheMatrix
  
  m<-solve(data,...)
  x$setinv(m) #setinv defined in makeCacheMatrix
  m
}
