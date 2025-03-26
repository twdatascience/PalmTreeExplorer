library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

DATA_FILE <- "./data/palmtrees.rds"
COLUMN_NAMES_TABLE_FILE <- "./data/columnNameTables.rds"
NUM_NAMES_TABLE_FILE <- "./data/numNamesTable.rds"
CHAR_NAMES_TABLE_FILE <- "./data/charNamesTable.rds"

ui <- fluidPage(
  titlePanel("Palm Tree Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("char_selector"),
      uiOutput("column_selector_x"),
      uiOutput("column_selector_y")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("plot")),
        tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(file.exists(DATA_FILE))
    readRDS(DATA_FILE)
  })
  
  columnNamesTable <- reactive({
    req(file.exists(COLUMN_NAMES_TABLE_FILE))
    readRDS(COLUMN_NAMES_TABLE_FILE)
  })
  
  numNamesTable <- reactive({
    req(file.exists(NUM_NAMES_TABLE_FILE))
    readRDS(NUM_NAMES_TABLE_FILE)
  })
  
  charNamesTable <- reactive({
    req(file.exists(CHAR_NAMES_TABLE_FILE))
    readRDS(CHAR_NAMES_TABLE_FILE)
  })
  
  output$char_selector <- renderUI({
    selectInput("selected_char",
                "Select Grouping",
                choices = charNamesTable()$select_name,
                selected = "Palm Subfamily")
  })
  
  output$column_selector_x <- renderUI({
    selectInput("selected_column_x", 
                "Select Column for X axis", 
                choices = numNamesTable()$select_name,
                selected = numNamesTable()$select_name[1])
  })
  
  output$column_selector_y <- renderUI({
    selectInput("selected_column_y", 
                "Select Column for Y axis", 
                choices = numNamesTable()$select_name,
                selected = numNamesTable()$select_name[2])
  })
  
  filtered_data <- reactive({
    req(data())
    df <- data()
    df |> 
      select(
        charNamesTable()$data_name[charNamesTable()$select_name == input$selected_char],
        numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_x],
        numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_y]
      ) |> 
      filter(!is.na(!!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_x])),
             !is.na(!!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_y])))
    
  })
  
  output$plot <- renderPlot({
    validate(need(!is.null(input$selected_column_x), "Please wait for data to laod"))
    req(filtered_data())
    if (length(unique(filtered_data()[[1]])) > 30) {
      filtered_data() |> 
        ggplot(
          aes(
            x = !!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_x]),
            y = !!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_y]),
            color = !!sym(charNamesTable()$data_name[charNamesTable()$select_name == input$selected_char])
          )) +
        guides(color = "none") +
        labs(
          x = paste0(input$selected_column_x),
          y = paste0(input$selected_column_y)) +
        geom_point(alpha = 0.3) +
        theme_minimal() 
    } else {
      filtered_data() |> 
        ggplot(
          aes(
            x = !!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_x]),
            y = !!sym(numNamesTable()$data_name[numNamesTable()$select_name == input$selected_column_y]),
            color = !!sym(charNamesTable()$data_name[charNamesTable()$select_name == input$selected_char])
          )) +
        # guides(color = "none") +
        labs(x = paste0(input$selected_column_x),
             y = paste0(input$selected_column_y)) +
        geom_point(alpha = 0.3) +
        theme_minimal() 
    }
    
  })
  
  output$table <- DT::renderDataTable({
    req(filtered_data())
    DT::datatable(filtered_data())
  })
}

shinyApp(ui, server)