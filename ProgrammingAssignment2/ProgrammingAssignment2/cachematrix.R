#Dennis Edwards
#JHU Foundations of R Week 2 Coursera
#30 AUG 2020-"R version 4.0.2 (2020-06-22)"
#Comments:  This is my first use of <<- and 
#Useful for storing computationally taxing objects in cache and recalling


#this function creates a list of four items set_mat, get_mat, get_inv, set_inv wher mat
#and inv are an invertible matrix and its inverse.  This heavily uses the example provided
# makevector

#assumes the matrix in invertible. An invertible matrix is a square matrix defined as
#invertible if the product of the matrix and its inverse is the identity matrix.
#here is my test case
# source:https://www.purplemath.com/modules/mtrxinvr2.htm
# known_matrix<-matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3,byrow=TRUE)
# known_matrix
# ans_matrix
# 
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1


makeCacheMatrix <- function(x = matrix()) {
  inv_mat<-NULL
  set_mat<- function(y){
    x<<-y
    inv_mat<<-NULL
  }#end set_mat function
  get_mat<-function(){x}
  set_inv<-function(inverse){inv_mat<<-inverse}
  get_inv<-function(){inv_mat}
  list(set=set_mat,get=get_mat,set_inv=set_inv, get_inv=get_inv)
  #this "special vector" is really the list of the 4 functions
}# end makeCacheMatrix function


## this function finds the inverse of a matrix, if the matrix inverse has been found before
#it pulls from the cache so that it doesn't waste time resolving something already done
#you will know if the matrix has been passed when you see the message "Using cached matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat<-x$get_inv()
  if (!is.null(inv_mat)){
    message("Using cached matrix")
    return(inv_mat)
  }#end if
  get_mat<-x$get()
  inverse<-solve(get_mat,...)
  x$set_inv(inverse)
  inverse
}#end cacheSolve function
