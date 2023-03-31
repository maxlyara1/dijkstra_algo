# Загрузка пакета
install.packages('igraph')
library('igraph')

# Функция проверки на то, есть ли в принципе путь к вершине и не посещена ли она
access_check <- function(R, expl_v){
  min <- Inf
  m <- ""
  for (i in 1:length(expl_v)){
    if ((R[i] < min) & (expl_v[i] == 0)){
      m <- i
      min <- R[i]
    }
  }
  return(m)
}

# Основная функция
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
  
  
  # Длины путей(веса ребёр) от начального ребра к соответствующим вершинам
  R <- Matr[v1, ]
  
  # expl_v используется для указания того, рёбра от каких вершин мы рассмотрели
  expl_v <- rep(0, n)
  expl_v[v1] <- 1
  
  # на каждой итерации в vis_v записывается те вершины, пути к которым были 
  #доступны из рассматриваемой вершины
  vis_v <- rep(0, n)
  vis_v[(Matr[v1, ] != Inf)] <- v1
  
  # пока количество рассмотренных вершин не равно количеству доступных для 
  #рассмотрения вершин 
  while (sum(expl_v) != length(expl_v)){
    k <- access_check(R, expl_v)
    if (k == "") {
      break
    }
    
    for (i in (1:n)) {
      if (R[i] > (R[k] + Matr[k, i])){
        R[i] <- R[k] + Matr[k, i]
        vis_v[i] <- k
      }
      expl_v[k] <- 1
    }
  }
  
  # Проверяем, есть ли путь к конечной вершине и строим путь, если он есть, 
  #или устанавливаем путь Inf, если пути нет
  if (vis_v[v2] != 0){
    path <- v2
    if (vis_v[v2] == v1){
      path <- c(v1, v2)
    }
    else{
      while (vis_v[v2] != v1){
        vis_v[v2] <- vis_v[path[1]]
        path <- c(vis_v[v2], path)
      }
    }
  }
  else{
    path <- Inf
  }
  #Длина пути
  length = R[v2]
  
  # Рисование графа и кратчайшего пути
  set.seed(42)
  g <- Matr
  g[g == Inf] <- 0
  a <- graph.adjacency(g, mode = "directed", weighted = T)
  E(a)$color <- 'grey'
  for(i in 1:(length-1)){
    E(a)[path[i+1]%--%path[i]]$color <- 'red'
  }
  #plot(a, edge.label = c(t(g)[t(g) != 0]),
       #edge.arrow.size = 0.5, layout = layout_in_circle)
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

