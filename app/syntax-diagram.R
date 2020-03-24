library(igraph)
library(reshape2) # pour melt
library(TraMineR)
library(geomorph) # pour la rotation des coordonnées 
library(plyr) # pour rbind.fill
library(xtable)


make_map_formula <- function(df){
  # sélection de la première formule s'il en existe plusieurs versions:
  df$formule.splited <- unlist(lapply(df$formule,
                          function(x) unlist(strsplit(x, "<br>"))[1] ))
  
  # ajout de marqueurs autour des opérateurs:
  df$formule.splited <- gsub("([\\(\\)\\[\\]+/=#])", "{\\1}",
                             df$formule.splited, perl=T)
  # découpage de la formule ; on obtient une liste d'éléments:
  df$formule.splited <- strsplit(df$formule.splited, "[\\}\\{]" )
  
  # suppression des éléments vides:
  df$formule.splited <- lapply(df$formule.splited,
                               function(x) x[ ! x=="" ])
  # génère 
  df$formule.syntax.splited <- sapply(df$formule.splited,
                                      function(x) gsub("[0-9]+", "x",  x) )
  df$formule.syntax <- sapply(df$formule.syntax.splited,
                              paste, collapse="")
  df
}

make_map_sequence <- function(df){
  # transformer en data.frame:
  seq.df <- lapply(df$formule.splited,
                   function(x) as.data.frame(t(as.data.frame(x)) ))
  # transformer la liste de data.frames en un unique data.frame:
  seq.df <- do.call(rbind.fill, seq.df)
  names(seq.df)   <- 1:length(seq.df)
  seq.df <- as.matrix(seq.df)
  seq.df[seq.df %in% as.character(1:1000) ] <- "x"
  seq <- seqdef(seq.df) 
  seq <- seqdef(cbind("Ø", seq.df)) # ajout d'un symbol vide initial
  seq
}


map_seq_graph <- function(trans.matrix, from.level){
  elist <- trans.matrix[,, from.level]
  elist <- melt(elist, value.name = "weight")
  # set vertices names:
  elist$Var1 <- gsub("^\\[(.) ->\\]", "\\1", elist$Var1, perl=T)
  elist$Var1 <- paste(elist$Var1, from.level-1, sep="")
  elist$Var2 <- gsub("^\\[-> (.)\\]", "\\1", elist$Var2, perl=T)
  elist$Var2 <- paste(elist$Var2, from.level, sep="")
  # create and clean graph
  g <- graph_from_data_frame(elist)
  g <- delete_edges(g, E(g)[E(g)$weight ==0]) 
  g <- delete_vertices(g, degree(g)==0)
  g
}

join_map_graphs <- function(g1, g2){
  g <- graph.union(g1, g2)
  if(is.null(E(g)$weight_1)) return(g)
  E(g)$weight_1[is.na(E(g)$weight_1)] <- E(g)$weight_2[ ! is.na(E(g)$weight_2)]
  E(g)$weight <- E(g)$weight_1
  g <- delete_edge_attr(g, "weight_1")
  g <- delete_edge_attr(g, "weight_2")
  V(g)$name2 <- gsub("[0-9]", "",  V(g)$name)
  g
}

map_seq_plot <- function(g, main=""){
  # préparation des coordonnées des noeuds :
  g$layout <- layout_as_tree(g, root = V(g)[1])
  g$layout <- geomorph::rotate.coords(g$layout, type= "rotateCC")
  g$layout <- norm_coords(g$layout)
  V(g)$color <- "white"
  V(g)[degree(g, mode= "out") == 0]$color <- "grey90"
  # plot du graphe :
  plot(g, 
       vertex.size=10, 
       vertex.color = V(g)$color,
       vertex.label = V(g)$name2,
       layout =  g$layout,
       edge.arrow.size=.3,  edge.arrow.width = 1,
       edge.width = E(g)$weight * 3 + 1,
       edge.label = round(E(g)$weight, 2),
       edge.color = as.character(factor(E(g)$weight,
                                        levels = sort(unique(E(g)$weight)),
                                        labels = rainbow(length(unique(E(g)$weight)),
                                                         start=0, end = .15, alpha=.9, rev=T))),
       edge.label.cex=.6, edge.label.color= "black",
       main = main)
  # ajout habillage :
  axis(1, unique(g$layout[,1]), 
       0:c(length(unique(g$layout[,1]))-1))
  title(xlab = "Rangs des symboles dans les séquences")
}

.shortest.path.weight.mean <-  function(g){
  edges <- shortest_paths(g, from = V(g)[1],
                          to = V(g)[degree(g, mode= "out") == 0],
                          output = "epath", weights=E(g)$weight)$epath
  res <- lapply(edges, function(x) E(subgraph.edges(g, x))$weight )
  round(min(sapply(res, mean)), 2)
}

.diameter.weight.mean <-  function(g){
  len.diameter <- length(get_diameter(g, weights = rep(1, ecount(g)))) - 1
  res <- distances(g, v = V(g)[1],
                   to = V(g)[degree(g, mode= "out") == 0], mode="out", 
                   weights = E(g)$weight) 
  res <- res / len.diameter
  round(max(res), 2)
}

make_data_stats <- function(seq.list){
  rbind(
    "n. formules"   = sapply(seq.list, nrow),
    "n. formules ≠" = sapply(seq.list, function(seq)
                        length(which( ! duplicated(seq)))),
    "longueur max." = sapply(seq.list, function(seq) max(seqlength(seq)) -1)
  )
}

map_seq_stats <- function(g.list, xtable=F){
  trans.stats <- rbind(
    "n. sommets" = sapply(g.list, gorder),
    "n. symboles ≠" = sapply(g.list, function(x) length(unique(V(x)$name2)) - 1),
    "n. symb. terminaux" = sapply(g.list, function(x) length(which(degree(x, mode="out")==0))),
    "n. pts d'articulation" = sapply(g.list,
                                     function(x) length(articulation_points(x))),
    "centralisation de deg." = sapply(g.list,
                                      function(x) round(centr_degree(x)[[2]], 2)),
    "prob. médiane" = sapply(g.list, function(x) round(median(E(x)$weight), 2)),
    "prob. moy. diam." = sapply(g.list, function(x) .diameter.weight.mean(x)),
    "prob. moy. + court ch." =  sapply(g.list, .shortest.path.weight.mean)
    # c.bet = sapply(g.list, function(x) round(centr_betw(x)[[2]], 2))
  )
  if(xtable){
    trans.stats.out <- data.frame(apply(trans.stats, 2, as.character),
                                  row.names=rownames(trans.stats))
    return(xtable(trans.stats.out,
                  align= c("l", rep("r", ncol(trans.stats.out) ) ) ) )
  } else return(trans.stats)
}
