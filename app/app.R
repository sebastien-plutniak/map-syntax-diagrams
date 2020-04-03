library(shiny)
library(ggplot2)
library(shinythemes)

# DEFINE UI ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  # titlePanel("MAP syntax diagrams"),
  br(),
  a(href="https://map-polytheisms.huma-num.fr",  target="_blank",
    img(src='map-logo.png', width="250", align = "center")),
  br(), 
  h3("MAP syntax diagrams"),
  p("A research tool by",
    a("S. Plutniak", href="https://sebastien-plutniak.github.io",  target="_blank")),
  br(),
  
  # siderbar ----
  sidebarLayout(
    sidebarPanel(
      h3("Input"),
      fileInput('file', 'Choose CSV file:',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      radioButtons(inputId = 'sep', label = 'Separator:', 
                   choices = c("," =',' , ";"=';'
                               ,"tab"='\t'), inline=T, selected = ','),
      checkboxInput("use_example", "use example data"),
      conditionalPanel(condition = "typeof output.stats !== 'undefined'",
                       h3("Export")),
      conditionalPanel(condition = "typeof output.stats !== 'undefined'",
                       downloadButton("downloadData", "Statistics table"),
        br(), 
        ),
      conditionalPanel(condition = "typeof output.plotOuput !== 'undefined'",
                       downloadButton("downloadPlot", "Plot (svg)"),
        br()
      ),
      h3("Analyse"),
      uiOutput("slider"),
      tags$b("Highlight elements:"),
      fluidRow(
        column(6, numericInput("idElement1", "id 1", value = "", min = 0, width="100%")),
        column(6, numericInput("idElement2", "id 2", value = "", min = 0, width="100%")),
      ),
      uiOutput("var"),
      conditionalPanel(condition = "output.twoGraphs",
                       checkboxInput("graphDiff", "Show differences", value = F)),
      conditionalPanel(condition = "output.allowDistances",
                       checkboxInput("computeDistances", "Compute distances", value = F)),
      conditionalPanel(condition = "output.allowDistances",
                       radioButtons("distanceMethod", 
                                    label = "Distance method:", selected="Euclidian",
                                    choices =c("Euclidian", "Laplacian"))),
      
      actionButton("goButton", "Run analyse"),
      br(),br(),
      uiOutput("values"),
      width = 2,
    ),
    # main panel ----
    mainPanel(
      column(12, align="center",
             
             conditionalPanel(condition = "typeof output.userdata == 'undefined'",
                              tags$div(
                                HTML("<div style=width:370px;, align=left>
                    <p><b>Welcome to <i>MAP syntax diagrams</i>. </b></p>
                    This application is intended for the analysis 
                    of ancient divine onomastic sequences.   
                    The input data must follow the format developed by the
                    <a target=_blank, href=https://map-polytheisms.huma-num.fr/?lang=en>Mapping Ancient Polytheisms</a> project
                    and be contained in a column entitled “formule”.
                    <p>In this format, an onomastic sequence such as: <br>
                    &nbsp; <i>lʿštrt lʾdny lʾšmn </i>  <br>
                    is represented as: <br>
                    &nbsp; To Astarte / [ to his lord # to Eshmoun ])  <br>
                    and encoded as: <br>
                    &nbsp; 1 # [ 2 + 3 ]
                    </p>
                    Numbers refer to the elements of the sequence, four 
                    symbols (+, /, #, =) qualify their relations, 
                    and grouping is allowed using brackets and parentheses.
                    <br><br>
                    For more information, read the <a target=_blank, href=https://bit.ly/3dOgUga>documentation</a> (in French).                 
                    </div>")
                              )
                              ),
             conditionalPanel(condition = "typeof output.userdata !== 'undefined'",
                              h2("Input table")),
             uiOutput("tab"), 
             br(),
             conditionalPanel(condition = "typeof output.seqlength !== 'undefined'",
                              h2("Sequence lengths")),
             imageOutput("seqlength", width = "90%", height = "250px"),
             br(),
             conditionalPanel(condition = "typeof output.stats !== 'undefined'",
                              h2("Diagram statistics")),
             tableOutput("stats"),
             br(),
             conditionalPanel(condition = "typeof output.distances !== 'undefined'",
                              h2("Graph distances")),
             tableOutput("distances"),
             br(),
             uiOutput("distplot2"),
             br(),
             conditionalPanel(condition = "typeof output.plot !== 'undefined'",
                              h2("Syntax diagram plot")),
             imageOutput("plotOuput", width= "100%" )
              ),
      width = 10)
))

# DEFINE SERVER  ----    
server <- function(input, output) { 
  
  source("syntax-diagram.R")
  
  userFile <- reactive({
    # attente du fichier de l'utilisateur ----
    validate(need(input$file, message = ""))
    input$file
  })
  
  # récupération du tableau de l'utilisateur ----
  data <- reactive({
        if(input$use_example == F){
          read.csv(userFile()$datapath, header=T, #quote = "",
                   sep=input$sep, stringsAsFactors = F)
        }else{
          read.csv("sample.csv", header=T, stringsAsFactors = F)
        }
  })
  
  # aperçu du tableau de l'utilisateur ----
  output$userdata <- renderTable({
    data <- data()
    head(data, 3)
    })
  
  output$tab <- renderUI({div(style = 'overflow-x: scroll; overflow: auto', 
                                    tableOutput('userdata'))})

  output$var <- renderUI({
    req(data)
    data <- data()
     
    if( sum(names(data) %in% c("formule", "FORMULE", "formules", "FORMULES")) == 0  ){
      showNotification("The data table must have a 'formule' column.",
                       type = "error", duration = 40)
    }
     items <- names(data)
     names(items) <- items
     selectInput("var", "Variable:", items)
    })

    output$values <- renderUI({
      req(input$var)
      data <- data()
      val <- unique(eval(parse(text = paste0("data$", input$var))))
      names(val) <- val
      
      times <- input$reset_input # pour permettre la remise à zéro
      div( # nécessaire pour permettre la remise à zéro
      actionButton("reset_input", "Reset values"),
      br(), br(),
      checkboxGroupInput("values", "Values:", c("All", sort(val) ))
      )
    })
    
    # gestion du bouton pour les graphe des différences ----
    output$twoGraphs <- renderUI({ 
      if(length(input$values) == 2)
        "true"
    })
    outputOptions(output, "twoGraphs", suspendWhenHidden = FALSE) # nécessaire
    
    # gestion du bouton pour le calcul des distances ----
    output$allowDistances <- renderUI({ 
      if(length(input$values) >= 2)
        "true"
    })
    outputOptions(output, "allowDistances", suspendWhenHidden = FALSE) # nécessaire
    
    
    # fonction générale pour le plot des diagrammes----
    plotInput <- eventReactive(input$goButton, {
      req(input$values)
      
      data <- data()
      
      idElement <- c(input$idElement1, input$idElement2)
      if(is.na(idElement[1])){idElement[1] <- 0}
      if(is.na(idElement[2])){idElement[2] <- 0}
      
      input$goButton

      data <- make_map_formula(data, idElement)
      seq  <- make_map_sequence(data)
      
      isolate({
      # calcul des longueurs des séquences ----
      seq.list.full <- lapply(input$values[input$values != "All"],
                              function(sel){
                                # sélection variable %in% values :
                                selection.v <- eval(parse(text = paste0("data$", input$var))) %in% sel
                                seq[ selection.v, ]
                              })
      names(seq.list.full) <- input$values[input$values != "All"]
      # si requis, ajout de l'ensemble du corpus :
      if("All" %in% input$values)                        {
        seq.list.full$"All" <- seq
      }
      lengths.df <- lapply(1:length(seq.list.full), function(x)
        data.frame(subset = names(seq.list.full)[x],
                   Length = seqlength(seq.list.full[[x]]) ) )
      lengths.df <- do.call("rbind", lengths.df)
      
      output$slider <- renderUI({ 
        slider.min.max <- c(1, 8)
        if( ! is.null(input$seqlengths)){
          slider.min.max <- c(input$seqlengths[1], input$seqlengths[2])
        }
        sliderInput("seqlengths", "Seq. length min/max",
                    min=1, max = max(lengths.df$Length),
                    value = slider.min.max )
      })
      
      # plot des longueurs des séquences  :
      output$seqlength <- renderPlot({
        slider.min.max <- c(1, 8)
        if( ! is.null(input$seqlengths)){
          slider.min.max <- c(input$seqlengths[1], input$seqlengths[2])
        }
        ggplot() +
          theme_linedraw(base_size = 14) +
          geom_path(data = lengths.df, aes(x = Length, color = subset, group = subset),
                    stat="bin", bins = max(lengths.df$Length) - 1, size= .8) +
          geom_vline(aes(xintercept = slider.min.max[1]),
                     linetype = 2, color = "gray50")+
          geom_vline(aes(xintercept = slider.min.max[2]),
                     linetype = 2, color = "gray50" )+
          geom_label(data = data.frame(lab = c("min." , "max.")),
                     aes(x = c(slider.min.max[1], slider.min.max[2]),
                         y = max(table(lengths.df$Length)) - max(table(lengths.df$Length)) * .1 ,
                         label = lab )) +
          scale_color_viridis_d() +
          scale_y_log10("Log(count)") + 
          scale_x_log10("Log(sequence length)", 
                        # breaks = 10^(-10:10), # log ticks
                        # minor_breaks =  rep(1:9, 21)*(10^rep(-10:10, each=9))
                        ) +
          annotation_logticks(sides = "bl")
      })
      
      # sélection des sous-ensembles de séquences à étudier ----
      #  longueurs maximales de séquences : 
      seq.min.max <- c(1, 8)
      if( ! is.null(input$seqlengths)){
        seq.min.max <- c(input$seqlengths[1], input$seqlengths[2])
      }
      
      selection.len <- c(seqlength(seq) <= seq.min.max[2] + 1) &
                       c(seqlength(seq) >= seq.min.max[1] + 1)
      # stockage des sous-ensembles dans une liste :
      seq.list <- lapply(input$values[input$values != "All"],
        function(sel){
        # sélection variable %in% values :
          selection.v <- eval(parse(text = paste0("data$", input$var))) %in% sel
          # union des deux sélections :
          sel.join <- selection.v & selection.len
          seq[ sel.join, ]
        })
      names(seq.list) <- input$values[input$values != "All"]
      #  si requis, ajout du diagramme de l'ensemble des séquences: ----
      if("All" %in% input$values ){
        seq.list$"All" <- seq[ selection.len, ]
      }
      
      # stats sur les données ----
      data.stats <- make_data_stats(seq.list)
      
      # calcul des taux de transition, conversion en graphe : ----
      seq.list.copy <- seq.list
      seq.list <- lapply(seq.list, function(x){ seqtrate(x, time.varying=1) })
      g.list <- lapply(seq.list, function(x){ lapply(1:seq.min.max[2],
                               function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list <- lapply(g.list, function(x){ Reduce(join_map_graphs, x)})
      # second calcul, avec décompte (et non fréquence) des transitions:----
      seq.list.count <- lapply(seq.list.copy,
                             function(x){ seqtrate(x, time.varying=1, count=T) })
      g.list.count <- lapply(seq.list.count,
                             function(x){ lapply(1:seq.min.max[2],
                             function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list.count <- lapply(g.list.count, function(x){ Reduce(join_map_graphs, x)})
      # ajout d'un attribut d'arête "nombre" (de transitions):
      g.list <- lapply(1:length(g.list), function(x){
        set_edge_attr(g.list[[x]], "count", value = E(g.list.count[[x]])$weight)})
      # modification de la pondération, normalisation en fct du nbre de séq
      g.list <- lapply(g.list, global.trate)
      # Distance entre graphes  ----
      output$distances <- renderTable({
          if( input$computeDistances & length(input$values) >= 2){
            # normalisation des pondérations en fonction du nbre de 
            # séquences intégrées dans le graphe :
            # calcul des distances
            res.dist <- map_graph_distance(g.list, 
                                           method = input$distanceMethod)
            # formatage du tableau:
            res.dist <- round(res.dist, 2)
            res.dist <- data.frame(res.dist)
            res.dist[upper.tri(res.dist, diag = T)] <- "–"
            colnames(res.dist) <- names(seq.list)
            rownames(res.dist) <- names(seq.list)
        
        output$distplot <- renderPlot({
         if( input$computeDistances & length(seq.list) >= 4){
              clus.res <- hclust(as.dist(res.dist), method="ward.D2")
              clus.res$labels <- names(seq.list)
              plot(clus.res, sub = paste("Method:", input$distanceMethod))
         }
        })
        
        output$distplot2 <- renderUI({
          if( input$computeDistances & length(seq.list) >= 4){
            fluidPage(plotOutput("distplot", width="60%") )    
          }
        })
        res.dist
        }
      })
      
      # stats sur les graphes et tableau à télécharger  ----    
      stats <- eventReactive(input$goButton,
      {
        data <- data()
        
        n.seq.total <- data[ eval(parse(text = paste0("data$", input$var))) %in% input$values, ]
        n.seq.total <- table(eval(parse(text = paste0("n.seq.total$", input$var))))
        
        if( is.na(input$idElement1) ){
        stats <- rbind("Total nr of sequence" = c(n.seq.total),
                       data.stats,
                       map_seq_stats(g.list))
        }else{
        stats <- rbind("n. sequences total" = c(n.seq.total),
                       data.stats,
                       make_element_stats(g.list, input$idElement1))
        }
          
        stats
      })
      
      output$stats <- renderTable({ stats() }, rownames = T ) 
      
      output$downloadData <- downloadHandler(
        filename = function(){"stats.csv"},
        content = function(fname){
          write.csv(stats(), fname)
        }
      )
      
      list(g.list, idElement, seq.list)
      })
    })
    
    output$plotOuput <- renderPlot({  # plot des diagrammes ----
    g.list <-  plotInput()[[1]]
    idElement <-  plotInput()[[2]]
    seq.list <-   plotInput()[[3]]
    # si requis, ajout du diagramme des différences : ----
    if(input$graphDiff & length(g.list) == 2 ){ 
      g.list$"Difference" <- map_seq_diff(g.list[[1]], g.list[[2]])
    }
    # plot function ----
    par(mfrow = c(ceiling( length(g.list) / 2), ceiling(2 - 1/length(g.list)) ) )
    for(i in 1:length(g.list)){
      map_seq_plot(g.list[[i]],
                   type = g.list[[i]]$type,
                   main = names(seq.list)[i],
                   idElement = idElement)
    }
    },
    height = function(){1 + ceiling( c(length(input$values) + sum(length(input$values) == 1) * 2 + sum(input$graphDiff)) / 2) * 600},
    res = 100
    )
    
    output$downloadPlot <- downloadHandler(
      filename = "syntax-diagram.svg",
      content = function(file) {
        g.list <-  plotInput()[[1]]
        idElement <-  plotInput()[[2]]
        seq.list <-   plotInput()[[3]]
        # si requis, ajout du diagramme des différences : ----
        if(input$graphDiff & length(g.list) == 2 ){ 
          g.list$"Difference" <- map_seq_diff(g.list[[1]], g.list[[2]])
        }
        svg(file,  pointsize = 9, 
            width = input$seqlengths[2],
            height = c(1 + ceiling( c(length(input$values)  + sum(length(input$values) == 1) * 2 + sum(input$graphDiff))  / 2) * 5  ) 
        )
        par(mfrow = c(ceiling( length(g.list) / 2), ceiling(2 - 1/length(g.list)) ) )
        for(i in 1:length(g.list)){
          map_seq_plot(g.list[[i]],
                       type = g.list[[i]]$type,
                       main = names(seq.list)[i],
                       idElement = idElement)
        }
        dev.off()
      }
    )


# fin du serveur
}

# Run app:
shinyApp(ui = ui, server = server)


