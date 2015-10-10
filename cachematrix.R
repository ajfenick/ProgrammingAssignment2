+## Same as example
+## Creates a new list object where x is input matrix and inv is the inverse

+makeCacheMatrix <- function(x = numeric()) {
## Set Null to inv
+  inv <- NULL
## Set x = y
+  set <- function(y) {
+    x <<- y
+    inv <<- NULL
+  }
## get data
+  get <- function() x
## Set Inv matrix
+  setInv <- function(inv) inv <<- inv
## Get matrix
+  getInv <- function() inv
## Return a list with above objects
+  list(set = set, get = get,
+       setInv = setInv,
+       getInv = getInv)
 }
 
 
 ## Write a short comment describing this function
-## Return a matrix that is the inverse of 'x'
+## If inv was calculated then return cached data
+## else calculate identity matrix (b), solve Ax=b and save in x.
 cacheSolve <- function(x, ...) {
-## Get Inv from data
+  inv <- x$getInv()
-## If Inv exist the return cached data
+  if(!is.null(inv)) {
+    message("getting cached data")
+    return(inv)
+  }
-## Is inv is not calculated then retrieve data and genrate identity matrix
+  data <- x$get()
+  length=dim(data)[1]
+  b=diag(1,length)
## Calculate inverse matrix
+  inv <- solve(data,b, ...)
## Save in input object inv
+  x$setInv(inv)
## Print inv on screen
+  inv
 }
