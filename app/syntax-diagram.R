library(igraph)
library(reshape2) # pour melt
library(TraMineR)
library(geomorph) # pour la rotation des coordonnées 
library(plyr) # pour rbind.fill
library(xtable)


# fct ajoutant les formules reformatées:
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

# Fct générant des objets séquences à partir des formules en chaînes de caractères:
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

# Fct transformant la matrice de transition en graphe:
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

# Fct fussionnant deux graphes en conservant les attributs:
join_map_graphs <- function(g1, g2){
  g <- graph.union(g1, g2)
  if(is.null(E(g)$weight_1)) return(g)
  E(g)$weight_1[is.na(E(g)$weight_1)] <- E(g)$weight_2[ ! is.na(E(g)$weight_2)]
  E(g)$weight <- E(g)$weight_1
  g <- delete_edge_attr(g, "weight_1")
  g <- delete_edge_attr(g, "weight_2")
  V(g)$name2 <- gsub("[0-9]", "",  V(g)$name)
  g$type <- "normal"
  g
}

# Fct générant une la sortie graphique d'un ou plusieurs graphes:
map_seq_plot <- function(g, type="normal", main=""){
  # préparation des coordonnées des sommets :
  g$layout <- layout_as_tree(g, root = V(g)[1])
  g$layout <- geomorph::rotate.coords(g$layout, type= "rotateCC")
  g$layout <- norm_coords(g$layout)
  
  if( g$type == "difference" ){ # pour les diagrammes de différence:
    # préparation des attributs :
    V(g)$name2 <- gsub("[0-9]", "",  V(g)$name)
    # plot du graphe :
    plot(g, 
         vertex.size=10, 
         vertex.color = V(g)$color,
         vertex.label = V(g)$name2,
         layout =  g$layout,
         edge.arrow.size=.2,edge.arrow.width = .8,
         edge.width = 2,
         edge.color = E(g)$color,
         main = "Differences")
    text(-1, -.9, paste("≠ nodes =", g$v.diff), pos=4, cex=.8, col="red") 
    text(-1, -1, paste("≠ edges =", g$e.diff), pos=4, cex=.8, col="red") 
  }
  else{ # pour les diagrammes normaux :
    # préparation des attributs :
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
         edge.label.cex=.7, edge.label.color= "black",
         main = main)
  }
  # ajout habillage :
  axis(1, unique(g$layout[,1]), 
       0:c(length(unique(g$layout[,1]))-1))
  title(xlab = "Symbols ranks")
}

# fct renvoyant un graphe des différences entre deux graphes:
map_seq_diff <- function(g1, g2){
  # calcul de la différence et de l'union des deux graphes:
  g  <- graph.union(g1, g2)
  g.diff <-  graph.union(difference(g1, g2),
                         difference(g2, g1))
  g.diff <- delete_vertices(g.diff, degree(g.diff, mode="all")==0)
  # préparation des attributs du graphe final:
  E(g)$weight <- 1
  V(g)$color <- "white"
  E(g)$color <- "grey"
  # recherche des arêtes et sommets communs :
  sel.e <- apply(get.edgelist(g), 1, paste, collapse="") %in%
    apply(get.edgelist(g.diff), 1, paste, collapse="")
  sel.v <- ( V(g)$name %in% V(g1)$name ) & ( V(g)$name %in% V(g2)$name ) 
  # marquage des différences:
  E(g)[ sel.e ]$color <- "red"
  V(g)[ ! sel.v ]$color <- "red"
  # préparation des coordonnées des noeuds :
  g$layout <- layout_as_tree(g, root = V(g)[1])
  g$layout <- geomorph::rotate.coords(g$layout, type= "rotateCC")
  g$layout <- norm_coords(g$layout)
  V(g)$name2 <- gsub("[0-9]", "",  V(g)$name)
  g$type <- "difference"
  g$e.diff <- length(which(sel.e))  # nombre d'arêtes différentes
  g$v.diff <- length(which(! sel.v))  # nombre de sommets différentes
  g
}

# fct renvoyant la moyenne des pondérations des arêtes des plus courts chemins:
.shortest.path.weight.mean <-  function(g){
  edges <- shortest_paths(g, from = V(g)[1],
                          to = V(g)[degree(g, mode= "out") == 0],
                          output = "epath", weights=E(g)$weight)$epath
  res <- lapply(edges, function(x) E(subgraph.edges(g, x))$weight )
  round(min(sapply(res, mean)), 2)
}

# fct renvoyant la valeur moyenne des pondération sur le diamètre
.diameter.weight.mean <-  function(g){
  len.diameter <- length(get_diameter(g, weights = rep(1, ecount(g)))) - 1
  res <- distances(g, v = V(g)[1],
                   to = V(g)[degree(g, mode= "out") == 0], mode="out", 
                   weights = E(g)$weight) 
  res <- res / len.diameter
  round(max(res), 2)
}

# fct prenant une liste d'objets séquence et renvoyant un df de statistiques;
make_data_stats <- function(seq.list){
  rbind(
    "n. sequences"   = sapply(seq.list, nrow),
    "n. different sequences" = sapply(seq.list, function(seq)
                        length(which( ! duplicated(seq)))),
    "sequence max. length" = sapply(seq.list, function(seq) max(seqlength(seq)) -1)
  )
}

# fct prenant une liste de graphes et renvoyant un df de statistiques (ou une sortie LaTeX):
map_seq_stats <- function(g.list, xtable=F){
  # sélection des graphes normaux: 
  g.list <- g.list[ sapply(g.list, function(g) g$type == "normal") ]
  
  trans.stats <- rbind(
    "n. nodes" = sapply(g.list, gorder),
    "n. different symbols" = sapply(g.list, function(x) length(unique(V(x)$name2)) - 1),
    "n. terminal symbols" = sapply(g.list, function(x) length(which(degree(x, mode="out")==0))),
    "n. articulation points" = sapply(g.list,
                                     function(x) length(articulation_points(x))),
    "degree centralisation" = sapply(g.list,
                                      function(x) round(centr_degree(x)[[2]], 2)),
    "median probability" = sapply(g.list, function(x) round(median(E(x)$weight), 2)),
    "diameter mean prob." = sapply(g.list, function(x) .diameter.weight.mean(x)),
    "shorter paths mean prob." =  sapply(g.list, .shortest.path.weight.mean)
    # c.bet = sapply(g.list, function(x) round(centr_betw(x)[[2]], 2))
  )
  if(xtable){
    trans.stats.out <- data.frame(apply(trans.stats, 2, as.character),
                                  row.names=rownames(trans.stats))
    return(xtable(trans.stats.out,
                  align= c("l", rep("r", ncol(trans.stats.out) ) ) ) )
  } else return(trans.stats)
}
