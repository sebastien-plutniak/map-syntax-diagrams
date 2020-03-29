library(shiny)
library(ggplot2)
library(shinythemes)

# DEFINE UI ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  # titlePanel("MAP syntax diagrams"),
  br(),
  a(href="https://map-polytheisms.huma-num.fr", 
    img(src='map-logo.png', width="250", align = "center")),
  br(), 
  h3("MAP syntax diagrams"),
  p("A research tool by",
    a("S. Plutniak", href="https://sebastien-plutniak.github.io")),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose CSV file:',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      radioButtons(inputId = 'sep', label = 'Separator:', 
                   choices = c("," =',' , ";"=';'
                               ,"tab"='\t'), inline=T, selected = ','),
      checkboxInput("use_example", "use example data"),
      conditionalPanel(condition = "typeof output.stats !== 'undefined'",
                       downloadButton("downloadData", "Statistics table"),
        br(),#br()
        ),
      conditionalPanel(condition = "typeof output.plot !== 'undefined'",
                       downloadButton("downloadPlot", "Plot (svg)"),
                       br(),br()
      ),
      numericInput("inputSeqlen",
                   "Sequence max. length:", 
                   value = 8, min = 3),
      uiOutput("output.Seqlen.min"),
      uiOutput("var"),
      conditionalPanel(condition = "output.twoGraphs",
                       checkboxInput("graphDiff", "Show differences", value = F)),
      uiOutput("values"),
      width = 2,
    ),

    mainPanel(
      column(12, align="center",
             
             conditionalPanel(condition = "typeof output.userdata == 'undefined'",
                              tags$div(
                                HTML("<div style=width:370px;, align=left>
                    <p><b>Welcome to <i>MAP syntax diagrams</i>. </b></p>
                    This application is intended for the analysis 
                    of anciennt divine onomastic sequences.   
                    The input data must be formatted according to 
                    the method developed by the
                    <a href=https://map-polytheisms.huma-num.fr/?lang=en>Mapping Ancient Polytheisms</a> project
                    and contained in a column entitled “formule”.
                    <p>In this format, an onomastic sequence such as: <br>
                    &nbsp; Ἀπό[λλω]ν[ο]ς Πυθίου καὶ Κεδριέως  <br>
                    is represented as: <br>
                    &nbsp; Apollōn # ( Puthios + Kedrieus )  <br>
                    and encoded as: <br>
                    &nbsp; 1 # ( 2 + 3 )
                    </p>
                    Numbers refer to the semantic elements, four 
                    symbols (+, /, #, =) qualify their relations, 
                    and grouping is allowed using brackets and parentheses.
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
             conditionalPanel(condition = "typeof output.plot !== 'undefined'",
                              h2("Syntax diagram plot")),
             imageOutput("plot", width= "100%" )
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
          read.csv(userFile()$datapath, header=T,
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
    
    output$output.Seqlen.min <- renderUI({
      numericInput("inputSeqlen.min",
                   "Sequence min. length:", 
                   value = 1, min = input$inputSeqlen)
    })
    # gestion du graphe des différences ----
    output$twoGraphs <- renderUI({ 
      if(length(input$values) == 2)
         "true"
      })
    outputOptions(output, "twoGraphs", suspendWhenHidden = FALSE) # nécessaire

        
    # fonction générale pour le plot des diagrammes----
    plotInput <- function(){
      req(input$values)
      
      data <- data()
      data <- make_map_formula(data)
      seq  <- make_map_sequence(data)

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
      # plot des longueurs des séquences  :
      output$seqlength <- renderPlot({
        ggplot(lengths.df, aes(x = Length, color = subset, group = subset) ) +
          theme_linedraw(base_size = 14) +
          geom_path(stat="bin", bins = max(lengths.df$Length) - 1, size= .8) +
          scale_color_viridis_d() +
          scale_x_log10("Log(Length)", 
                        # breaks = 10^(-10:10), # log ticks
                        # minor_breaks =  rep(1:9, 21)*(10^rep(-10:10, each=9))
                        ) +
          annotation_logticks(sides = "b")
      })
      
      # sélection des sous-ensembles de séquences à étudier ----
      #  longueurs maximales de séquences : 
      selection.len <- c(seqlength(seq) <= input$inputSeqlen + 1) &
                       c(seqlength(seq) >= input$inputSeqlen.min + 1)
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
      
      # calcul des taux de transition, conversion en graphe :
      seq.list.copy <- seq.list
      seq.list <- lapply(seq.list, function(x){ seqtrate(x, time.varying=1) })
      g.list <- lapply(seq.list, function(x){ lapply(1:input$inputSeqlen,
                               function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list <- lapply(g.list, function(x){ Reduce(join_map_graphs, x)})
      
      # second calcul, avec décompte (et non fréquence) des transitions:
      seq.list.count <- lapply(seq.list.copy,
                               function(x){ seqtrate(x, time.varying=1, count=T) })
      g.list.count <- lapply(seq.list.count,
                       function(x){ lapply(1:input$inputSeqlen,
                                                     function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list.count <- lapply(g.list.count, function(x){ Reduce(join_map_graphs, x)})

      # ajout d'un attribut d'arête "nombre" (de transitions):
      g.list <- lapply(1:length(g.list), function(x){
        set_edge_attr(g.list[[x]], "count", value = E(g.list.count[[x]])$weight)
      })
      
      # stats sur les graphes et tableau à télécharger  ----    
      output$stats <- renderTable({
      stats <- rbind(data.stats,  map_seq_stats(g.list))
      
      output$downloadData <- downloadHandler( 
        filename = function(){"stats.csv"},
        content = function(fname){
          write.csv(stats, fname)
        }
      )
      stats
      }, rownames = T )  
      
      # si requis, ajout du diagramme des différences : ----
      if(input$graphDiff & length(g.list) == 2 ){ 
        g.list$"Difference" <- map_seq_diff(g.list[[1]], g.list[[2]])
      }
      
      # plot function ----
      par(mfrow = c(ceiling( length(g.list) / 2),  ceiling(2 - 1/length(g.list)) ) )
      for(i in 1:length(g.list)){
          map_seq_plot(g.list[[i]],
                       type = g.list[[i]]$type,
                       main = names(g.list)[i] )
      }
   }
      
    output$plot <- renderPlot({  # plot des diagramme ----

      list(src = plotInput(),
           height = function(){1 + ceiling( c(length(input$values ) + sum(input$graphDiff)) / 2) * 600},
           alt = "This is alternate text")
    }, 
        height = function(){1 + ceiling( c(length(input$values ) + sum(input$graphDiff)) / 2) * 600},
        res = 100
    )
    
    # téléchargement de l'image en svg ---- 
      output$downloadPlot <- downloadHandler(
        filename = "syntax-diagram.svg",
        content = function(file) {
          svg(file, width = 10, pointsize = 9,
              height =  c(1 + ceiling( c(length(input$values) + sum(input$graphDiff))  / 2)   * 5 ) )
           plotInput()
          dev.off()
        }
        )
}

# Run the app ----
shinyApp(ui = ui, server = server)


