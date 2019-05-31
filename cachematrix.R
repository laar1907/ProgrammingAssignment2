## Week 3: Lexical Scoping and Matrices
## The first function creates a sort of list that stores the main elements to calculate the inverse of a matrix
## In here we store the value of the matrix we establish, set the initial value of the inverse to null, as well 
## as the function that set new information and calculate the inverse. All is returned as a list like in the example given 

##Part 1: Creating the object that will store my main arguments
##What will it save? Elements of my matrix, the getters and setters (as in the makeVector example))

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  ##Like the example, we initially set the value of the inverse of a matrix (mat_inv)
  ##to NULL. By using the operator "<<-" we assing the values of y and NULL in the
  ## main environment. The last specification (mat_inv <<- NULL) allow us to remove / erase previous values
  ## of mat_inv that were executed when we call the function cacheSolve. 
  
  get <- function() x
  setInversa <- function(solve) mat_inv <<- solve
  getInversa <- function() mat_inv
  list (set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}

##Part 2

##Again, like in the example, we take advantage of the lexical scoping properties by defining
##our main arguments outside the function in order for R to 'get' it from the main environment
##In order to calculate the inverse of a matrix, we use the function solve. In here, the first 
##argument has to be a square numeric or a complex matrix. If we do not specify the second argument
##("b" as shown in the R Documentation), the function solve will calculate the inverse of the matrix.
##With the elements shown above we create a list that will be pass to the next function in order to calculate
## the inverse of a matrix through the cacheSolve function  

cacheSolve <- function(x, ...) {
  mat_inv <- x$getInversa()
  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  
  ##The first part of the second function lets us know if we're calling the cacheSolve function of 
  ##information that was previously stored and executed, the function will not run the multiple steps again
  ##but it will retrieve the previously saved data and give a message (getting cached data). 
  
  data <- x$get()
  mat_inv <- solve(data)
  x$setInversa(mat_inv)
  mat_inv     ## Return a matrix that is the inverse of 'x'
}
