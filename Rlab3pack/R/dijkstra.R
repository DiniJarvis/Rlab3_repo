dijkstra <-
function(graph,init_node)
{
  non_used_edge<-unique(graph$v1) # 6 values for the given ex
  path<-vector(length=length(non_used_edge)) # initialising path with length of v1
  stopifnot(init_node<length(non_used_edge)) #check if length exceeds
  path[init_node]<-0 
  path[-init_node]<-999
  
  while (length(non_used_edge)!=0)
  {
    current<-non_used_edge[1] #taking first vertices value
    x<-graph[graph$v1==current,] # taking all rows from the wiki_graph data for the first edge value
    
    i<-1
    while (i <= nrow(x))
    {
      if((path[current]+x$w[i]) < path[x$v2[i]]) #check if the edge has shortest path
      {
        path[x$v2[i]]<-(path[current]+x$w[i]) #if shortest, save the current path
      }
      i<-i+1
    }
    
    non_used_edge<-non_used_edge[-1] #loop for next edge
  }
  return(path)
}
