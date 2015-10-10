+## Same as example
+## Creates a new list object where x is input matrix and inv is the inverse
 
-makeCacheMatrix <- function(x = matrix()) {
-
+makeCacheMatrix <- function(x = numeric()) {
+  inv <- NULL
+  set <- function(y) {
+    x <<- y
+    inv <<- NULL
+  }
+  get <- function() x
+  setInv <- function(inv) inv <<- inv
+  getInv <- function() inv
+  list(set = set, get = get,
+       setInv = setInv,
+       getInv = getInv)
 }
 
 
 ## Write a short comment describing this function
-
+## If exist then return cached data
+## else calculate identity (b vector), solve Ax=b and save in x.
 cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
+  inv <- x$getInv()
+  if(!is.null(inv)) {
+    message("getting cached data")
+    return(inv)
+  }
+  data <- x$get()
+  length=dim(data)[1]
+  b=diag(1,length)
+  inv <- solve(data,b, ...)
+  x$setInv(inv)
+  inv
 }
