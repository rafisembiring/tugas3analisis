#install packages
install.packages("shiny")
install.packages("ggplot2")
install.packages("DT")

#menjalankan packages
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
      selectInput("x_var", "Pilih Variabel untuk Sumbu X:", choices = names(dataset), selected = "MinTemp"),
      selectInput("y_var", "Pilih Variabel untuk Sumbu Y:", choices = names(dataset), selected = "MaxTemp"),
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
    req(input$x_var, input$y_var)
    x_var <- input$x_var
    y_var <- input$y_var
    plot_type <- input$plot_type
    
    if (plot_type == "scatter") {
      ggplot(dataset, aes_string(x = x_var, y = y_var)) + 
        geom_point(color = "blue") + 
        theme_minimal() + 
        labs(x = x_var, y = y_var, title = "Scatter Plot")
    } else if (plot_type == "line") {
      ggplot(dataset, aes_string(x = x_var, y = y_var)) + 
        geom_line(color = "red") + 
        theme_minimal() + 
        labs(x = x_var, y = y_var, title = "Line Plot")
    } else if (plot_type == "bar") {
      ggplot(dataset, aes_string(x = x_var, y = y_var)) + 
        geom_bar(stat = "identity", fill = "lightblue") + 
        theme_minimal() + 
        labs(x = x_var, y = y_var, title = "Bar Plot")
    }
  })
  
  output$table <- renderDT({
    datatable(dataset)
  })
}

# Jalankan Aplikasi
shinyApp(ui = ui, server = server)
