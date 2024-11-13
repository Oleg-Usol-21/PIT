library(shiny)

ui <- fluidPage(
  titlePanel("Shiny приложение"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Выберите число:", 1, 100, 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)