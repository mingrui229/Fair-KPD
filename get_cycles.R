get_cycles <- function(nodes, edgelist, max_cycle_size){
  cycle_list <- list() 
  cycles <- 0 
  incidence <- function(from,to){
    return(edgelist %>% filter(X1==from, X2==to) %>% nrow > 0)
  }
  get_child <- function(from, current, visited){
    
    if (from != nodes){
      for (j in (from + 1):nodes){
        if (incidence(current,j) & !visited[j]){
          return(j)
        }
      }
    }
    
    return(-1)
  }
  node_stack <- numeric()
  current_head_node <- 1
  visited <- rep(FALSE, nodes)
  while (current_head_node <= nodes){
    node_stack <- c(node_stack, current_head_node)
    visited[current_head_node] <- TRUE 
    v <- get_child(current_head_node, current_head_node, visited)
    
    while(length(node_stack) > 0){
      if (v == -1){
        backtrack_node <- node_stack %>% tail(1)
        node_stack <- node_stack %>% head(-1)
        
        if (backtrack_node == current_head_node){
          visited[backtrack_node] <- FALSE
          break
        }
        visited[backtrack_node] <- FALSE
        new_top_node <- node_stack %>% tail(1)
        v <- get_child(backtrack_node, new_top_node, visited)
        
        
      } else {
        node_stack <- c(node_stack, v)
        visited[v] <- TRUE
        if (incidence(v,current_head_node)){
          if (length(node_stack) <= max_cycle_size){
            cycles <- cycles + 1
            cycle_list[[cycles]] <- node_stack
          }
          
        }
        
        if (length(node_stack) >= max_cycle_size){
          v <- -1
        } else {
          v <- get_child(current_head_node, v, visited)
        }
      }
    }
    current_head_node <- current_head_node + 1
  }
  return(cycle_list)
}