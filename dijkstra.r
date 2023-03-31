Mn <- function(R, A){
  min <- Inf
  m <- ""
  for (i in 1:length(A)){
    if ((R[i] < min) & (A[i] == 0)){
      m <- i
      min <- R[i]
    }
  }
  return(m)
}

Dijkstra <- function(M, v1, v2){
  n <- ncol(M)
  answer <- list()
  
  if (!(v1 %in% (1:n)) || !(v2 %in% (1:n))){
    stop("Такой вершины не существует")
  }
  else if (!(is.numeric(v1) || !(is.numeric(v2)))){
    stop("Вершина должна задаваться числом")
  }
  else if (v1 == v2) {
    stop("Введите разные вершины")
  }
  
  R <- M[v1, ]
  A <- rep(0, n)
  A[v1] <- 1
  P <- rep(0, n)
  P[(M[v1, ] != Inf)] <- v1
  
  while (sum(A) != length(A)){
    k <- Mn(R, A)
    if (k == "") {
      break
    }
    
    for (i in (1:n)) {
      if (R[i] > (R[k] + M[k, i])){
        R[i] <- R[k] + M[k, i]
        P[i] <- k
      }
      A[k] <- 1
    }
  }
  
  if (P[v2] != 0){
    path <- v2
    if (P[v2] == v1){
      path <- c(v1, v2)
    }else{
      while (P[v2] != v1){
        P[v2] <- P[path[1]]
        path <- c(P[v2], path)
      }
    }
  }else{
    path <- Inf
  }
  
  answer <- list(length = R[v2], path = path)
  return(answer)
}
#my_data = c(Inf, Inf, 5, 1,
            #Inf, Inf, Inf, 1,
            #Inf, 1, Inf, 3,
            #Inf, Inf, Inf, Inf)
map <- matrix(0, ncol = 11, nrow = 11)
map[1,2] <- 10
map[1,3] <- 4
map[1,4] <- 8
map[2,3] <- 8
map[2,5] <- 6
map[3,4] <- 4
map[3,5] <- 7
map[4,7] <- 7
map[4,6] <- 10
map[4,8] <- 7
map[8,6] <- 7
map[8,10] <- 6
map[10,6] <- 11
map[6,9] <- 4
map[10,9] <- 12
map[6,11] <- 5
map[5,4] <- 8
map[4,11] <- 13
map[9,11] <- 5

func_res <- Dijkstra(map, 1, 4)
print(func_res)


#график
install.packages('igraph')
library('igraph')
set.seed(42)
g <- map
g[g == Inf] <- 0
a <- graph.adjacency(g, mode = "directed", weighted = T)
plot(a, edge.label = c(t(g)[t(g) != 0]),
            edge.arrow.size = 0.5, layout = layout_in_circle)
plot(a, edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)