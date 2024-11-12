library(shiny)

ui <- fluidPage(
    titlePanel("Простое Shiny приложение"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("num", "Выберите число:", 1, 100, 50)
        ),
        mainPanel(
            textOutput("result")
        )
    )
)

server <- function(input, output) {
    output$result <- renderText({
        paste("Вы выбрали число:", input$num)
    })
}

shinyApp(ui = ui, server = server)