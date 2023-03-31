# Загрузка пакета
install.packages('igraph')
library('igraph')


Mn <- function(R, vis_v){
  min <- Inf
  Matr <- ""
  for (i in 1:length(vis_v)){
    if ((R[i] < min) & (vis_v[i] == 0)){
      Matr <- i
      min <- R[i]
    }
  }
  return(Matr)
}

Dijkstra <- function(Matr, v1, v2){
  n <- ncol(Matr)
  answer <- list()
  
  # Сканирование входных данных на предмет ошибок
  if (!(v1 %in% (1:n)) || !(v2 %in% (1:n))){
    stop("Такой вершины не существует")
  }
  else if (!(is.numeric(v1) || !(is.numeric(v2)))){
    stop("Вершина должна задаваться числом")
  }
  else if (v1 == v2) {
    stop("Введите разные вершины")
  }
  
  
  R <- Matr[v1, ]
  vis_v <- rep(0, n)
  vis_v[v1] <- 1
  P <- rep(0, n)
  P[(Matr[v1, ] != Inf)] <- v1
  
  while (sum(vis_v) != length(vis_v)){
    k <- Mn(R, vis_v)
    if (k == "") {
      break
    }
    
    for (i in (1:n)) {
      if (R[i] > (R[k] + Matr[k, i])){
        R[i] <- R[k] + Matr[k, i]
        P[i] <- k
      }
      vis_v[k] <- 1
    }
  }
  
  if (P[v2] != 0){
    path <- v2
    if (P[v2] == v1){
      path <- c(v1, v2)
    }
    else{
      while (P[v2] != v1){
        P[v2] <- P[path[1]]
        path <- c(P[v2], path)
      }
    }
  }
  else{
    path <- Inf
  }
  #Длина пути
  length = R[v2]
  
  # Рисование графика
  set.seed(42)
  g <- Matr
  g[g == Inf] <- 0
  a <- graph.adjacency(g, mode = "directed", weighted = T)
  E(a)$color <- 'grey'
  for(i in 1:(length-1)){
    E(a)[path[i+1]%--%path[i]]$color <- 'red'
  }
  plot(a, edge.label = c(t(g)[t(g) != 0]),
       edge.arrow.size = 0.5, layout = layout_in_circle)
  plot(a, edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)
    
  answer <- list(length, path)
  return(answer)
}
#my_data = c(Inf, Inf, 5, 1,
            #Inf, Inf, Inf, 1,
            #Inf, 1, Inf, 3,
            #Inf, Inf, Inf, Inf)


Matr <- matrix(0, nrow = 4, ncol = 4)
Matr[1,] <- c(Inf, Inf, 1, 5)
Matr[2,] <- c(Inf, Inf, Inf, 1)
Matr[3,] <- c(Inf, 1, Inf, 3)
Matr[4,] <- c(Inf, Inf, Inf, Inf)



func_res <- Dijkstra(Matr, 1, 4)
print(func_res)

