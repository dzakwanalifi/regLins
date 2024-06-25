library(shiny)
library(shinythemes)
library(readxl)
library(shinycssloaders)
library(car)
library(lmtest)
library(nortest)
library(regLins)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Regresi Linear dengan Optimisasi"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Unggah File Excel atau CSV", accept = c(".csv", ".xlsx")),
      selectInput("separator", "Pemisah CSV", choices = c("," = ",", ";" = ";")),
      checkboxInput("convert_factors", "Ubah Kategori Menjadi Faktor", value = FALSE),
      uiOutput("response_ui"),
      uiOutput("predictors_ui"),
      selectInput("method", "Metode", choices = c("kuadrat terkecil", "kemungkinan")),
      actionButton("run", "Jalankan Regresi")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Ringkasan", withSpinner(verbatimTextOutput("summary"), color = "#0dc5c1")),
        tabPanel("Plot", withSpinner(plotOutput("plots"), color = "#0dc5c1"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    if (file_ext == "csv") {
      read.csv(input$file$datapath, sep = input$separator)
    } else if (file_ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      stop("Tipe file tidak didukung.")
    }
  })
  
  observe({
    df <- data()
    types <- sapply(df, class)
    choices <- paste(names(df), "-", types)
    updateSelectInput(session, "response", choices = choices)
    updateSelectInput(session, "predictors", choices = choices, selected = NULL)
  })
  
  output$response_ui <- renderUI({
    selectInput("response", "Variabel Respons", choices = NULL)
  })
  
  output$predictors_ui <- renderUI({
    selectInput("predictors", "Variabel Prediktor", choices = NULL, multiple = TRUE)
  })
  
  model <- eventReactive(input$run, {
    df <- data()
    response_name <- strsplit(input$response, " - ")[[1]][1]
    predictor_names <- sapply(strsplit(input$predictors, " - "), `[`, 1)
    
    if (!is.numeric(df[[response_name]])) {
      stop("Variabel respons harus numerik.")
    }
    
    if (input$convert_factors) {
      df[predictor_names] <- lapply(df[predictor_names], function(x) if (is.character(x) || is.factor(x)) factor(x) else x)
    }
    
    y <- df[[response_name]]
    X <- as.matrix(df[, predictor_names])
    colnames(X) <- make.names(predictor_names) 
    
    regLin(y, X, method=input$method)
  })
  
  output$summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$plots <- renderPlot({
    req(model())
    plot(model())
  })
}

shinyApp(ui = ui, server = server)
