library(igraph) # pour les manipulations et calcul des graphes
library(reshape2) # pour melt()
library(TraMineR) # pour seqdef(), seqtrate()
library(geomorph) # pour rotate.coords() (rotation des coordonnées)
library(plyr) # pour rbind.fill()
library(xtable) # pas utile dans le cadre de l'application
library(NetworkDistance) # pour nd.gdd

# fct ajoutant les formules reformatées:
make_map_formula <- function(df, idElement){
  # récupération des données de la colonne des formules
  df$formule <- df[, which(names(df) %in% c("formula", "formule", "FORMULE", "FORMULA", "formule", "FORMULES"))]

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
  # remplacement des id par un symbole "x" :
  df$formule.syntax.splited <- sapply(df$formule.splited, function(x){
    overwrite <- grepl("[^+=/#()\\[\\]]+", x, perl=T) # marquage de tout sauf les opérateurs
    if( idElement[1] != 0 ){
      exception <- grepl(paste0("^", idElement[1], "$"),  x)  |
                   grepl(paste0("^", idElement[2], "$"),  x)
      overwrite[ exception ] <- F # marquage de l'élément à conserver
    }
    x[overwrite] <- "x"
    list(x)
  } )
  # concaténation: 
  df$formule.syntax <- sapply(df$formule.syntax.splited, paste0)
  df
}

# Fct générant des objets séquences à partir des formules en chaînes de caractères:
make_map_sequence <- function(df){
  # transformer en data.frame:
  seq.df <- lapply(df$formule.syntax.splited,
                   function(x) as.data.frame(t(as.data.frame(x)) ))
  # transformer la liste de data.frames en un unique data.frame:
  seq.df <- do.call(rbind.fill, seq.df)
  names(seq.df)   <- 1:length(seq.df)
  seq.df <- as.matrix(seq.df)
  seq <- seqdef(cbind("Ø", seq.df)) # ajout d'un symbol vide initial
  seq
}


# Fct transformant la matrice de transition en graphe:
map_seq_graph <- function(trans.matrix, from.level){
  elist <- trans.matrix[,, from.level]
  elist <- melt(elist, value.name = "weight")
  # set vertices names:
  elist$Var1 <- gsub("^\\[(.*) ->\\]", "\\1", elist$Var1, perl=T)
  elist$Var1 <- paste(elist$Var1, from.level-1, sep="-")
  elist$Var2 <- gsub("^\\[-> (.*)\\]", "\\1", elist$Var2, perl=T)
  elist$Var2 <- paste(elist$Var2, from.level, sep="-")
  # create and clean graph:
  g <- graph_from_data_frame(elist)
  g <- delete_edges(g, E(g)[E(g)$weight ==0]) 
  g <- delete_vertices(g, degree(g)==0)
  g
}

# Fct fusionnant deux graphes en conservant les attributs:
join_map_graphs <- function(g1, g2){
  g <- graph.union(g1, g2)
  if(is.null(E(g)$weight_1)) return(g)
  E(g)$weight_1[is.na(E(g)$weight_1)] <- E(g)$weight_2[ ! is.na(E(g)$weight_2)]
  E(g)$weight <- E(g)$weight_1
  g <- delete_edge_attr(g, "weight_1")
  g <- delete_edge_attr(g, "weight_2")
  V(g)$name2 <- gsub("-[0-9]", "",  V(g)$name)
  g$type <- "normal"
  g
}

# Fct générant une la sortie graphique d'un ou plusieurs graphes:
map_seq_plot <- function(g, type="normal", main="", idElement=NULL){
  # préparation des coordonnées des sommets :
  g$layout <- layout_as_tree(g, root = V(g)[1])
  g$layout <- geomorph::rotate.coords(g$layout, type= "rotateCC")
  g$layout <- norm_coords(g$layout)
  V(g)$shape <- "circle"
  # marquage de l'éventuel élément sélectionné par l'utilisateur:
  V(g)[ V(g)$name2 %in% idElement]$shape <- "square"
  V(g)[ V(g)$name2 == idElement[1] ]$name2 <- "id1"
  V(g)[ V(g)$name2 == idElement[2] ]$name2 <- "id2"
  
  if( g$type == "difference" ){ # pour les diagrammes de différence:
    # préparation des attributs :
    # plot du graphe :
    plot(g, 
         vertex.size=10, 
         vertex.color = V(g)$color,
         vertex.label = V(g)$name2,
         vertex.shape = V(g)$shape,
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
         vertex.size = 7 + scale(strength(g, mode = "all", weights = E(g)$weight2) / degree(g, mode = "all"), center=F) * 4,
         vertex.color = V(g)$color,
         vertex.label = V(g)$name2,
         vertex.shape = V(g)$shape,
         layout =  g$layout,
         edge.arrow.size=.3,  edge.arrow.width = 1,
         edge.width = .8 + scale(E(g)$weight2, center = F) * 2,
         edge.label = round(E(g)$weight2, 2),
         edge.color = as.character(factor(E(g)$weight2,
                                          levels = sort(unique(E(g)$weight2)),
                                          labels = rainbow(length(unique(E(g)$weight2)),
                                                           start=0, end = .15, alpha=.9, rev=T))),
         edge.label.cex=.7, edge.label.color= "black",
         main = main)
  }
  # ajout habillage :
  axis(1, unique(g$layout[,1]), 
       0:c(length(unique(g$layout[,1]))-1), cex.axis=.8)  # todo modif
  title(xlab = "Symbol rank")
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
  sel.e <- apply(as_edgelist(g), 1, paste, collapse="") %in%
    apply(as_edgelist(g.diff), 1, paste, collapse="")
  sel.v <- ( V(g)$name %in% V(g1)$name ) & ( V(g)$name %in% V(g2)$name ) 
  # marquage des différences:
  E(g)[ sel.e ]$color <- "red"
  V(g)[ ! sel.v ]$color <- "red"
  # préparation des coordonnées des noeuds :
  g$layout <- layout_as_tree(g, root = V(g)[1])
  g$layout <- geomorph::rotate.coords(g$layout, type= "rotateCC")
  g$layout <- norm_coords(g$layout)
  V(g)$name2 <- gsub("-[0-9]", "",  V(g)$name)
  g$type <- "difference"
  g$e.diff <- length(which(sel.e))  # nombre d'arêtes différentes
  g$v.diff <- length(which(! sel.v))  # nombre de sommets différentes
  g
}



# fct exécutant le calcul de la distance entre deux graphes:
.pairwise.graph.distance <- function(g, g1, g2, g.name1, g.name2, method){
  # sélection des arêtes à supprimer:
  sel.e1 <- ! apply(as_edgelist(g), 1, paste, collapse="") %in%
    apply(as_edgelist(g1), 1, paste, collapse="")
  sel.e2 <- ! apply(as_edgelist(g), 1, paste, collapse="") %in%
    apply(as_edgelist(g2), 1, paste, collapse="")
  # suppression :
  gg1 <- delete_edges(g, E(g)[sel.e1])
  gg2 <- delete_edges(g, E(g)[sel.e2])
  # récupération des pondérations :
  E(gg1)$weight2 <- eval(parse(text = paste0("E(gg1)$weight2_", g.name1)))
  E(gg2)$weight2 <- eval(parse(text = paste0("E(gg2)$weight2_", g.name2)))
  
  # obtenir une matrice d'adjacence pour chaque graphe:
  g.list <- list(gg1, gg2)
  g.list <- lapply(g.list, as_adjacency_matrix,
                   sparse=F, attr = "weight2")
  # calcul de distance
  if(method == "Laplacian"){
    nd.gdd(g.list)$D  # adapté aux graphes pondérés
    # nd.csd(g.list)$D  # méthode recommandée par JurmanEtal2010 / mais bug
  }
  else{
    norm(g.list[[1]] - g.list[[2]], type = "F") #dist. euclidienne (MartinezChavez2019):
  }
}

map_graph_distance <- function(graphlist, method){
  # union des graphes à comparer (graphe référence) :
  g  <- graph.union(graphlist)
  # sélection des arêtes à supprimer :
  pairs <- expand.grid(1:length(graphlist), 1:length(graphlist))
  # exécution du calcul de distance:
  res <- sapply(1:nrow(pairs), function(x) 
    .pairwise.graph.distance(g = g, 
                             g1 = graphlist[[ pairs[x , 1]]], 
                             g2 = graphlist[[ pairs[x , 2]]],
                             g.name1 = pairs[x , 1],
                             g.name2 = pairs[x , 2],
                             method)
  )
  matrix(res, ncol = length(graphlist))
}

# fonction prenant un graphe dont les arêtes sont pondérées par le nombre
# de transition, et renvoit le graphe après avoir modifié l'attribut 
# weight en divisant ses valeurspar nombre total de séquences à partir
# desquelles le graphe a été construit (capturé ici par le degré sortant du
# premier symbole de la séquence)
global.trate <- function(g){
  set_edge_attr(g, "weight2", value = E(g)$count / strength(g,
                   vids=1, mode="out", weights = E(g)$count) )
}

# fct renvoyant la moyenne des pondérations des arêtes des plus 
# courts chemins entre le symbol initial et les symboles terminaux.
.shortest.path.weight.mean <-  function(g){
  edges <- shortest_paths(g, from = V(g)[1],
                          to = V(g)[degree(g, mode= "out") == 0],
                          output = "epath", weights=E(g)$weight2)$epath
  res <- lapply(edges, function(x) E(subgraph.edges(g, x))$weight2 )
  round(min(sapply(res, mean)), 2)
}

# fct renvoyant la valeur moyenne des pondération sur le diamètre
.diameter.weight.mean <-  function(g){
  len.diameter <- length(get_diameter(g, weights = rep(1, ecount(g)))) - 1
  res <- distances(g, v = V(g)[1],
                   to = V(g)[degree(g, mode= "out") == 0], mode="out", 
                   weights = E(g)$weight2) 
  res <- res / len.diameter
  round(max(res), 2)
}

# fct prenant une liste d'objets séquence et renvoyant un df de statistiques;
make_element_stats <- function(seq.list, idElement){
  rbind(
    "Id 1: nr of positions" = sapply(seq.list, function(x)
      length(V(x)[V(x)$name2 %in% idElement  ])),
    "Id 1: median in-degree" = sapply(seq.list, function(x){
      median(strength(x, V(x)[V(x)$name2 %in% idElement ],
                        mode = "in", weights = E(x)$weight2),  na.rm = T)
    })
  )
}

# fct prenant une liste d'objets séquence et renvoyant un df de statistiques;
make_data_stats <- function(seq.list){
  rbind(
    "nr of selected sequences"   = sapply(seq.list, nrow),
    "nr of different sequences" = sapply(seq.list, function(seq)
                        length(which( ! duplicated(seq)))),
    "sequence max. length" = sapply(seq.list, function(seq) max(seqlength(seq)) -1)
  )
}

# fct prenant une liste de graphes et renvoyant un df de statistiques (ou une sortie LaTeX):
map_seq_stats <- function(g.list, xtable=F){
  # sélection des graphes normaux: 
  g.list <- g.list[ sapply(g.list, function(g) g$type == "normal") ]
  
  trans.stats <- rbind(
    "nr of nodes" = sapply(g.list, gorder),
    "nr of different symbols" = sapply(g.list, function(x) length(unique(V(x)$name2)) - 1),
    "nr of terminal symbols" = sapply(g.list, function(x) length(which(degree(x, mode="out")==0))),
    "nr of articulation points" = sapply(lapply(g.list, function(x) induced_subgraph(x,  V(x)[-1] ) ),
                                     function(x) length(articulation_points(x))),
    "degree centralisation" = sapply(lapply(g.list, function(x) induced_subgraph(x,  V(x)[-1] ) ),
               function(x) round(centr_degree(x)[[2]], 2)),
    "median probability" = sapply(g.list, function(x) round(median(E(x)$weight2), 2))
    # "diameter mean prob." = sapply(g.list, function(x) .diameter.weight.mean(x)),
    # "shorter paths mean prob." =  sapply(g.list, .shortest.path.weight.mean)
    # c.bet = sapply(g.list, function(x) round(centr_betw(x)[[2]], 2))
  )
  if(xtable){
    trans.stats.out <- data.frame(apply(trans.stats, 2, as.character),
                                  row.names=rownames(trans.stats))
    return(xtable(trans.stats.out,
                  align= c("l", rep("r", ncol(trans.stats.out) ) ) ) )
  } else return(trans.stats)
}
