euclidean <-
function(x, y) {
  stopifnot(is.numeric(x),is.numeric(y))
  while(y!=0) 
  {
    r=x%%y    # x= y * Q + r
    x<-y
    y<-r
  }
  return(x)
}
