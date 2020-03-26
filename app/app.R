library(shiny)
library(ggplot2)
library(shinythemes)

# DEFINE UI ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  # titlePanel("MAP syntax diagrams"),
  br(),
  a(href="https://map-polytheisms.huma-num.fr", 
    img(src='map-logo.png', width="250", align = "left")),
  br(), br(),  br(), 
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
      numericInput("inputSeqlen",
                  "Sequence max. length:", 
                 value = 8, min = 2, max = 30),
      conditionalPanel(condition = "typeof output.stats !== 'undefined'",
        downloadButton("downloadData", "Statistics table"),
        br(),br()
        ),
      conditionalPanel(condition = "typeof output.plot !== 'undefined'",
                       downloadButton("downloadPlot", "Plot (svg)"),
                       br(),br()
      ),
      uiOutput("var"),
      conditionalPanel(condition = "output.twoGraphs",
                       checkboxInput("graphDiff", "Show differences", value = F)),
      uiOutput("values"),
      width = 2
    ),

    mainPanel(
      column(12, align="center",
             h2("Input table"),
             tableOutput("tab"),
             br(),
             h2("Sequence lengths"),
             imageOutput("seqlength", width = "90%", height = "250px"),
             br(),
             h2("Diagram statistics"),
             tableOutput("stats"),
             br(),
             h2("Plot"),
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
          # read.csv(userFile()$datapath, header=T, stringsAsFactors = F)
          read.csv(userFile()$datapath, header=T,
                   sep=input$sep, stringsAsFactors = F)
        }else{
          read.csv("sample.csv", header=T, stringsAsFactors = F)
        }
  })
  
  output$tab <- renderTable({ # aperçu du tableau de l'utilisateur ----
    data <- data()
    head(data, 3)
    })

  output$var <- renderUI({
     data <- data()
     items <- names(data)
     names(items) <- items
     selectInput("var", "Variable:", items)
    })

    output$values <- renderUI({
      req(input$var)
      data <- data()
      val <- unique(eval(parse(text = paste0("data$", input$var))))
      names(val) <- val
      checkboxGroupInput("values", "Values:", c("All", val) )
    })
    
    output$twoGraphs <- renderText({ # gestion du graphe des différences ----
      if(length(input$values) == 2)
         "true"
      })
    outputOptions(output, "twoGraphs", suspendWhenHidden = FALSE) # nécessaire
    
    plotInput <- function(){ # fonction générale pour le plot des diagrammes----
      req(input$values)
      
      data <- data()
      data <- make_map_formula(data)
      seq  <- make_map_sequence(data)

      # calcul des longueurs des séquences ----
      seq.list.full <- lapply(input$values[input$values != "All"],
      # seq.list.full <- lapply(input$values,
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
          scale_color_viridis_d()
      })
      
      # sélection des sous-ensembles de séquences à étudier ----
      #  longueurs maximales de séquences : 
      selection.len <- seqlength(seq) <= input$inputSeqlen + 1
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
      seq.list <- lapply(seq.list, function(x){ seqtrate(x, time.varying=1) })
      g.list <- lapply(seq.list, function(x){ lapply(1:input$inputSeqlen,
                               function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list <- lapply(g.list, function(x){ Reduce(join_map_graphs, x)})
      
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
      if(input$graphDiff){ 
        g.list$"Difference" <- map_seq_diff(g.list[[1]], g.list[[2]])
      }
      
      # plot function ----
      par(mfrow = c(ceiling( length(g.list) / 2), 2 ) )
      for(i in 1:length(g.list)){
          map_seq_plot(g.list[[i]],
                       type = g.list[[i]]$type,
                       main = names(g.list)[i] )
      }
   }
      
    output$plot <- renderPlot({  # plot des diagramme ----
      
      list(src = plotInput(),
           height = function(){1 + ceiling( c(length(input$values ) + sum(input$graphDiff)) / 2) * 1000},
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


