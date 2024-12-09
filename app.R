install.packages("shiny")
install.packages("ggplot2")
install.packages("DT")

library(shiny)
library(ggplot2)
library(DT)

# Baca dataset
dataset <- read.csv("C:/Users/Rafi/Documents/Semester 6/Tugas 3/Analisis/weather.csv")

# UI
ui <- fluidPage(
  titlePanel("Visualisasi Data Interaktif"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Pilih Variabel:", choices = names(dataset), selected = "MinTemp"),
      selectInput("plot_type", "Pilih Jenis Plot:", 
                  choices = c("Scatter Plot" = "scatter", 
                              "Line Plot" = "line", 
                              "Bar Plot" = "bar", 
                              "Tabel Data" = "table"))
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.plot_type != 'table'",
        plotOutput("plot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'table'",
        DTOutput("table")
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$var)
    var <- input$var
    plot_type <- input$plot_type
    
    if (plot_type == "scatter") {
      ggplot(dataset, aes_string(x = var, y = "Rainfall")) + geom_point() + theme_minimal()
    } else if (plot_type == "line") {
      ggplot(dataset, aes_string(x = seq_along(dataset[[var]]), y = var)) + geom_line() + theme_minimal()
    } else if (plot_type == "bar") {
      ggplot(dataset, aes_string(x = var)) + geom_bar() + theme_minimal()
    }
  })
  
  output$table <- renderDT({
    datatable(dataset)
  })
}

# Jalankan Aplikasi
shinyApp(ui = ui, server = server)
