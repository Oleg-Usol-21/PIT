library(shiny)

server <- function(input, output) {
  output$result <- renderText({
    paste("Вы выбрали число:", input$num)
  })
}

shinyServer(server)