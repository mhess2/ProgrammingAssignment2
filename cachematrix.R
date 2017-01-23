# Coursera
# Programming Assignment 2
# mhess2 2017-01-22

# The following functions follow the same logic as the makeVector and cachemean
# functions presented in the Programming Assignment 2: Lexical Scoping
# instructions. The two main differences are that these functions are specific
# to matrices and use the solve function to find the inverse. makeCacheMatrix
# sets and gets the value of the matrix and its inverse. cacheSolve checks and
# retrieves the inverse if it has already been cached. If not, it will go ahead
# and solve for the inverse matrix.

# Creates a matrix (actually a list of functions) that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

# Retrieves the inverse matrix if it has already been cached (from the previous)
# function, otherwise calculates the inverse matrix
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)){
        message("Getting cached data...")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}

# The following are just some examples of tests I ran to make sure the functions
# were working properly

# A <- matrix(1:4, 2, 2)
# B <- solve(A)
# a <- makeCacheMatrix(A)
# cacheSolve(a)

# C <- matrix(rnorm(16), 4, 4)
# D <- solve(C)
# c <- makeCacheMatrix(C)
# cacheSolve(c)


