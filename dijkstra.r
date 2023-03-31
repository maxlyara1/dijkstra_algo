Mn <- function(R, A){
  min <- Inf
  m <- "clown"
  for (i in 1:length(A)){
    if ((R[i] < min) & (A[i] == 0)){
      m <- i
      min <- R[i]
    }
  }
  return(m)
}

Dijkstra <- function(M, v1, v2){
  #cheeck_mat(M)
  n <- ncol(M)
  answer <- list()
  
  if (!(v1 %in% (1:n))){
    stop("takaya vershina ne syshestvuet")
  }
  if (!(is.numeric(v1))){
    stop("vershina dolzhna zadavatsya chislom")
  }
  if (!(v2 %in% (1:n))){
    stop("takaya vershina ne syshestvuet")
  }
  if (!(is.numeric(v2))){
    stop("vershina dolzhna zadavatsya chislom")
  }
  if (v1 == v2) {
    stop("nuzhno vvesti raznie vershini")
  }
  
  R <- M[v1, ]
  A <- rep(0, n)
  A[v1] <- 1
  P <- rep(0, n)
  P[(M[v1, ] != Inf)] <- v1
  
  while (sum(A) != length(A)){
    k <- Mn(R, A)
    if (k == "clown") {break()}
    
    for (i in (1:n)) { #???
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

M <- matrix(0, nrow = 4, ncol = 4)
M[1,] <- c(Inf, Inf, 1, 5)
M[2,] <- c(Inf, Inf, Inf, 1)
M[3,] <- c(Inf, 1, Inf, 3)
M[4,] <- c(Inf, Inf, Inf, Inf)

Dijkstra(M, 1, 4)


#график
library('igraph')
set.seed(1)
g <- M
g[g == Inf] <- 0
a <- graph.adjacency(g, mode = "directed", weighted = T)
plot.igraph(a, edge.label = c(t(g)[t(g) != 0]),
            edge.arrow.size = 0.5, layout = layout_in_circle)
plot.igraph(a, edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)