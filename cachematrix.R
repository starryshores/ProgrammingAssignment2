## The function below takes a matrix (x) which is invertible
## and caches the matrix.

makeCacheMatrix<-function(x=matrix()) {{
  z=NULL            ## NULL promotes the object to an empty list
  set=function(p) { 
    x<<-p
    z<<-NULL       
    ## using <<- instead of <- sets z as NULL within the
    ## environment of the function only.  z doesn't not exist
    ## as NULL in the global environment.
  }
  get<-function()x                       
  setinvr<-function(inverse) z<<-inverse  
  getinvr<-function() z                   
  list(set=set, get=get, setinvr=setinvr, getinvr=getinvr)
}
  
}

## The function below returns the inverse of the matrix
## which was cached by makeCacheMatrix

cacheSolve<-function(x,...) { 
  invr=x$getinvr()
  if(!is.null(invr)){       ## if inverse is not null
    print("Getting cached data.")
    return(invr)
    
  }
  
  matrix.data=x$get()
  invr=solve(matrix.data,...) #solve function to inverse matrix
  x$setinvr(invr)
  return(invr)
}