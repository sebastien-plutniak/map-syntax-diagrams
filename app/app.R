library(shiny)
library(shinythemes)

# Define UI ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  titlePanel("MAP syntax diagrams"),
  uiOutput("map"),
  uiOutput("author"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      checkboxInput("use_example", "use example data"),
      numericInput("inputSeqlen",
                  "Formula max. length", 
                 value = 8, min = 2, max = 30),
      conditionalPanel(condition = "typeof output.stats !== 'undefined'",
        downloadButton("downloadData", "Download stats"),
        br(),br()
        ),
      uiOutput("var"),
      uiOutput("values"),
      width = 2
    ),

    mainPanel(
      column(12, align="center",
             h2("Input table"),
             br(),
             tableOutput("tab"),
             h2("Graph statistics"),
             tableOutput("stats"),
             br(),
             h2("Plot"),
                plotOutput("plot", width= "100%" )
              ),
      width = 10)
))
    
# Define server logic ----
server <- function(input, output) {
  
  output$author <- renderUI({ tagList("by",
                                      a("S. Plutniak", href="https://sebastien-plutniak.github.io")) })
  output$map <- renderUI({ tagList("A research tool for the “",
               a("Mapping Ancient Polytheisms", href="https://map-polytheisms.huma-num.fr/"),
               "” project"
               ) })
  
  source("syntax-diagram.R")
  
  # data <- reactive({
  #   # req(input$file)
  #       read.csv(input$file$datapath, header=T, stringsAsFactors = F)
  # })
  
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = ""))
    input$file
  })
  
  # The user's data, parsed into a data frame
  data <- reactive({
        if(input$use_example == F){
          read.csv(userFile()$datapath, header=T, stringsAsFactors = F)
        }else{
          read.csv("sample.csv", header=T, stringsAsFactors = F)
        }
  })
  
  output$tab <- renderTable({
    # req(input$file)
    data <- data()
    head(data, 3)
    })

  output$var <- renderUI({
     # req(input$file)
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

    output$plot <- renderPlot({  # plot ----
      req(input$values)
      
      data <- data()
      data <- make_map_formula(data)
      seq  <- make_map_sequence(data)

      # sélection des longueurs maximales de séquences :
      selection.len <- seqlength(seq) <= input$inputSeqlen + 1
      
      seq.list <- lapply(input$values, function(sel){
        # sélection variable %in% values :
        selection.v <- eval(parse(text = paste0("data$", input$var))) %in% sel
        # union des deux sélections :
          sel <- selection.v & selection.len
          seq[ sel, ]
        })
      
      names(seq.list) <- input$values
      
      if("All" %in% input$values ){
        seq.list$"All" <- seq[selection.len, ]
      }
      
      # stats sur les données ----
      data.stats <- make_data_stats(seq.list)
      
      seq.list <- lapply(seq.list, function(x){ seqtrate(x, time.varying=1) })
      g.list <- lapply(seq.list, function(x){ lapply(1:input$inputSeqlen,
                               function(xx){ map_seq_graph(x, from.level=xx)})})
      g.list <- lapply(g.list, function(x){ Reduce(join_map_graphs, x)})

      # stats sur les graphes et tableau en sortie----    
      output$stats <- renderTable({
      stats <- rbind(data.stats,
                     map_seq_stats(g.list))

      # downloadable csv of selected dataset
      thedata <- reactive(stats)
      output$downloadData <- downloadHandler(
        filename = function(){"stats.csv"},
        content = function(fname){
          write.csv(thedata(), fname)
        }
      )
        stats
      }, rownames = T )
      
      # plot function ----
      par(mfrow = c(ceiling(length(input$values) / 2), 2 ) )
      for(i in 1:length(g.list)){
          map_seq_plot(g.list[[i]],
                       main = input$values[i])
      }
        },
        height = function(){1 + ceiling(length(input$values) / 2) * 600},
        res = 100
      # )

  )
}

# Run the app ----
shinyApp(ui = ui, server = server)


