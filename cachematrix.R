## Programming Assignment 2
## Contains two functions:
## 1. makeCacheMatrix - contains setters and getters for the matrix and its inverse
## 2. cacheSolve - obtains the inverse of the given matrix from the cache or computes if not in cache

## Function to make cache matrix that
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the inverse
## 4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
		im <- NULL
        set <- function(m) {
                x <<- m
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to calculate the inverse of a matrix using the above defined function
## First it checks if the inverse has already been calculated. 
## If so, it get's the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse using solve() and sets the value of the inverse in the cache via the setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        m <- x$get()
        im <- solve(m)
        x$setinverse(im)
        im
}

