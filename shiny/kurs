update.packages("shinyFiles")
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)

# Убедитесь, что папка "save" существует
if (!dir.exists("save")) {
  dir.create("save")
}

# Функция для загрузки файлов
upload_file <- function(file) {
  if (!is.null(file)) {
    file.copy(file$datapath, file.path("save", file$name))
    return(paste("Файл", file$name, "успешно загружен."))
  } else {
    return("Нет загруженных файлов.")
  }
}

# Функция для получения списка файлов в директории
list_files <- function() {
  return(list.files("save"))
}



ui <- navbarPage(
  "Оглавление",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  # Меню "Файлы" с двумя вкладками
  navbarMenu("Файлы",
            
              tabPanel("Загрузка", tabName = "upload", icon = icon("download"),
                     
                       mainPanel(
                        style = "background-color: #ADD8E6;",
                        h1("Загрузка данных"),
                        p("Загрузите файлы, которые нужно использовать в приложении.")
                        ),
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file", "Выберите файл для загрузки:", 
                                     accept = c(".csv", ".xlsx", ".txt")),
                           actionButton("upload_button", "Загрузить файл"),
                           hr()
                         ),
                         
                         mainPanel(
                           verbatimTextOutput("upload_status"),
                           verbatimTextOutput("download_status")
                         )
                       )
                       
                       
                       ),
                       
             tabPanel("Выгрузка", tabName = "download", icon = icon("download"),
                      mainPanel(
                        style = "background-color: #ADD8E6;",
                        h1("Выгрузка данных"),
                        p("Здесь будет реализована функциональность выгрузки данных.")),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("file_select", "Выберите файл для скачивания:", choices = list_files()),
                          downloadButton("download_button", "Скачать файл") 
                        ),
                        mainPanel(
                          verbatimTextOutput("upload_status"),
                          verbatimTextOutput("download_status")
                        )
                      )
                      
                      
                      )
  ),
  
  # Остальные вкладки
  tabPanel("Редактировать журнал", tabName = "edit-section", icon = icon("pen"),
           mainPanel(
             style = "background-color: #ADD8E6;",
             h1("Редактирование журнала"),
             p("Здесь будет реализована функциональность редактирования журнала.")
           )
  ),
  tabPanel("Статистика (таблица)", tabName = "table-stats", icon = icon("table"),
           mainPanel(
             style = "background-color: #ADD8E6;",
             h1("Статистика (таблица)"),
             p("Здесь будет отображена статистика в табличном виде.")
           )
  ),
  tabPanel("Статистика (график)", tabName = "graph-stats", icon = icon("chart-simple"),
           mainPanel(
             style = "background-color: #ADD8E6;",
             h1("Статистика (график)"),
             p("Здесь будет отображена статистика в виде графика.")
           )
  ),
  tabPanel("Помощь", tabName = "help", icon = icon("circle-question"),
           mainPanel(
             style = "background-color: #ADD8E6;",
             h1("Помощь"),
             p("Здесь будет представлена информация о работе приложения.")
           )
  ),
  tabPanel("О программе", tabName = "about", icon = icon("circle-info"),
           mainPanel(
             style = "background-color: #ADD8E6;",
             h1("О программе"),
             p("Здесь будет представлена информация о программе.")
           )
         )
)



server<-function(input, output, session){
  # Состояние для хранения статуса загрузки
  upload_status <- reactiveVal("")
  
  observeEvent(input$upload_button, {
    upload_status(upload_file(input$file))
    
    # Обновляем выбор файлов после загрузки
    updateSelectInput(session, "file_select", choices = list_files())
  })
  
  output$upload_status <- renderText({
    upload_status()
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      input$file_select
    },
    content = function(file) {
      file_path <- file.path("save", input$file_select)  # полный путь к файлу
      file.copy(file_path, file)
    },
    contentType = "application/octet-stream"
  )
  
  output$download_status <- renderText({
    if (!is.null(input$file_select)) {
      paste("Вы выбрали файл:", input$file_select)
    }
  })
  
}
shinyApp(ui, server)
