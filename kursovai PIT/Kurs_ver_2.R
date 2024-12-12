library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(utf8)
library(data.table)
library(writexl)
library(tidyr)
library(shinythemes)
library(stringr)

# --- 1. Функции загрузки и обработки файлов ---

# Функция загрузки файлов
load_files <- function(path = "save") {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(path, full.names = TRUE)
  files <- files[grep("\\.(csv|txt|xlsx?)$", files, ignore.case = TRUE)]
  return(files)
}

# Функция сохранения данных в файл
save_file <- function(data, filepath, encoding = "UTF-8") {
  ext <- tools::file_ext(tolower(filepath))
  tryCatch({
    switch(ext,
           txt = write_delim(data, filepath, delim = ";", na = "", col_names = TRUE, locale = locale(encoding = encoding)),
           csv = write_csv(data, file = filepath, na = "", locale = locale(encoding = encoding)),
           xlsx = write_xlsx(data, path = filepath),
           stop("Неподдерживаемый формат")
    )
  }, error = function(e) {
    stop(paste("Ошибка сохранения:", e$message))
  })
}

# Функция загрузки данных
load_data <- function(filepath, encoding = "UTF-8") {
  ext <- tools::file_ext(tolower(filepath))
  tryCatch({
    data <- switch(ext,
                   txt = read_delim(filepath, delim = ";", locale = locale(encoding = encoding), trim_ws = TRUE),
                   csv = read_csv(filepath, locale = locale(encoding = encoding),show_col_types = FALSE),
                   xlsx = read_excel(filepath),
                   stop("Неподдерживаемый формат")
    )
    data
  }, error = function(e) {
    stop(paste("Ошибка загрузки:", e$message))
  })
}

# Обработка данных
process_data <- function(data) {
  unwanted_cols <- c("class", "name")
  subject_cols <- setdiff(names(data), unwanted_cols)
  if (length(subject_cols) == 0) stop("Нет столбцов с оценками.")
  data[, subject_cols] <- lapply(data[, subject_cols], function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x), 0, x)
  })
  data_long <- data %>% pivot_longer(cols = subject_cols, names_to = "Предмет", values_to = "Оценка") %>%
    filter(!is.na(Оценка))
  if(nrow(data_long)==0) stop("Нет валидных оценок.")
  by_class_subject <- data_long %>%
    group_by(class, Предмет, Оценка) %>%
    summarize(Count = n()) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    group_by(class, Предмет) %>%
    mutate(Median = median(Оценка), Mean = mean(Оценка)) %>%
    ungroup()
    by_subject <- data_long %>%
    group_by(Предмет, Оценка) %>%
    summarize(Count = n()) %>%
    mutate(Percent = Count / sum(Count) * 100) %>%
    group_by(Предмет) %>%
    mutate(Median = median(Оценка), Mean = mean(Оценка)) %>%
    ungroup()
  list(by_class_subject = by_class_subject, by_subject = by_subject)
}

# --- 2. UI ---
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  title = tagList(img(src = "10.png", height = 50, width = 250)),
  tags$head(tags$style(HTML("
    .navbar-default { background-color: #337ab7; border-color: #337ab7; }
    .navbar-default .navbar-brand { color: white; }
    .navbar-default .navbar-nav > li > a { color: white; }
    .info-box { background-color: #AFEEEE; padding: 20px; border-radius: 5px; box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); }
  "))),
  tabPanel("Приветствие и история файлов",
           fluidRow(
             column(6, wellPanel(h2("Журнал оценок"), p("Загрузка, редактирование и анализ данных."))),
             column(6, uiOutput("fileHistory"))
           ),
           fileInput("uploadFile", "Выберите файл", accept = c(".csv", ".txt", ".xlsx")),
           uiOutput("uploadProgress")),
  tabPanel("Редактировать журнал",
           uiOutput("fileSelector"),
           DTOutput("editTable"),
           actionButton("addRow", "Добавить строку"),
           actionButton("deleteRow", "Удалить строку"),
           actionButton("saveChanges", "Сохранить изменения"), # Возвращена кнопка
           wellPanel(
             selectInput("outputFormat", "Формат:", choices = c("CSV", "TXT", "XLSX")),
             selectInput("encoding", "Кодировка:", choices = c("UTF-8", "UTF-16LE", "UTF-16BE", "Latin1", "ASCII", "Windows-1251", "MacRoman")),
             downloadButton("downloadEditedFile", "Скачать")
           )),
  tabPanel("Статистика (Таблица)",
           uiOutput("fileSelectorStats"),
           DTOutput("statsTable"),
           DTOutput("subjectStatsTable")),
  tabPanel("Статистика (Графики)",
           uiOutput("fileSelectorPlot"),
           plotOutput("classSubjectStatsPlot"),
           plotOutput("subjectStatsPlot")),
  tabPanel("Помощь",
           tabsetPanel(
             # Определяем CSS классы (лучше вынести в отдельный файл CSS для больших проектов)
             tags$head(tags$style(HTML("
             .center-text {
               text-align: center;
             }
             .center-image {
               display: flex;
               justify-content: center;
               align-items: center;
             }
             .center-image img {
               max-width: 100%;
               height: auto;
             }
           "))),
             
             tabPanel("Загрузка", wellPanel(
               h4("Загрузка файла", class = "center-text"),
               p("Здесь вы ознакомитесь с системой загрузки файлов в электронный журнал.", class = "center-text"),
               p("Для загрузки перейдите во вкладку \"Приветствие и история файлов\".", class = "center-text"),
               p("Нажмите кнопку \"Обзор\" и выберите нужный файл.", class = "center-text"),
               div(class = "center-image", img(src = "privet1.png", width = "25%", height = "auto")),
               p("Загрузка файла", class = "center-text"),
               p("Загружаемые файлы делятся на три типа:", class = "center-text"),
               div(class = "center-image", img(src = "privet2.png", width = "25%", height = "auto")),
               p("Форматы файлов", class = "center-text"),
               p("Загрузка файлов других форматов невозможна из-за сложности считывания данных.", class = "center-text"),
               p("После загрузки в истории файлов отобразятся: имя файла, дата и время загрузки.", class = "center-text"),
               div(class = "center-image", img(src = "privet3.png", width = "25%", height = "auto")),
               p("История загруженного файла", class = "center-text"),
               p("Поздравляем! Файл загружен. Переходите к редактированию.", class = "center-text")
             )),
             
             
             tabPanel("Редактирование", wellPanel(
               h4("Редактирование", class = "center-text"),
               p("Здесь вы ознакомитесь с работой вкладки \"Редактировать журнал\".", class = "center-text"),
               p("Сначала выберите файл для работы.", class = "center-text"),
               div(class = "center-image", img(src = "redak1.png", width = "25%", height = "auto")),
               p("Выбор файла", class = "center-text"),
               p("После выбора откроется окно с данными файла.", class = "center-text"),
               div(class = "center-image", img(src = "redak2.png", width = "25%", height = "auto")),
               p("Рабочая область", class = "center-text"),
               p("В таблице отображаются: ФИО ученика (name), класс (class), предметы (objects). Оценки расположены ниже.", class = "center-text"),
               p("Кнопки \"Добавить строку\", \"Удалить строку\", \"Сохранить изменения\" и постраничная навигация расположены ниже таблицы.", class = "center-text"),
               h5("Функционал:", class = "center-text"),
               p("1. Добавить строку: добавляет новую строку в конец списка.", class = "center-text"),
               p("2. Удалить строку: удаляет выделенную строку (подсвечивается синим).", class = "center-text"),
               p("3. Сохранить изменения: сохраняет изменения. Сохраняйте изменения после каждого изменения, так как система запоминает только последние изменения.", class = "center-text"),
               div(class = "center-image", img(src = "redak3.png", width = "50%", height = "auto")),
               p("Преобразователь", class = "center-text")
             )),
             
             
             tabPanel("Статистика", wellPanel(
               h4("Статистика", class = "center-text"),
               p("Здесь представлены данные после редактирования: класс, предмет, оценка, количество, процент, медиана, среднее значение.", class = "center-text"),
               p("Два основных раздела:", class = "center-text"),
               p("1. По классам и предметам:", class = "center-text"),
               div(class = "center-image", img(src = "static1.png", width = "30%", height = "auto")),
               p("Меню раздела", class = "center-text"),
               p("2. По предметам:", class = "center-text"),
               div(class = "center-image", img(src = "static2.png", width = "30%", height = "auto")),
               p("Меню раздела", class = "center-text"),
               p("Также доступны графики: столбчатые и круговые.", class = "center-text"),
               div(class = "center-image", img(src = "static3.png", width = "30%", height = "auto")),
               p("Столбчатые графики", class = "center-text"),
               div(class = "center-image", img(src = "static4.png", width = "30%", height = "auto")),
               p("Круговые графики", class = "center-text")
             )),
             
             
             tabPanel("Форматы", wellPanel(
               h4("Форматы файлов", class = "center-text"),
               p("Поддерживаются форматы: TXT, XLSX, CSV.", class = "center-text"),
               p("Выбор этих форматов обусловлен удобством работы с данными.", class = "center-text")
             )),
             
             
             tabPanel("Ошибки", wellPanel(
               h4("Обработка ошибок", class = "center-text"),
               p("Возможные ошибки связаны с некорректным форматом данных в файле.", class = "center-text"),
               p("Например, CSV-файлы с данными в одной строке могут вызвать ошибки при обработке.", class = "center-text"),
               p("Рекомендуется использовать табличный формат CSV для корректной работы.", class = "center-text")
             ))
             )
  ),
  tabPanel("О программе",
           fluidRow(
             column(4, img(src = 'fin3.png', height = 300, width = 300)),
             column(8, div(class = "info-box", HTML("<h2>О программе</h2><p>Разработчик: Зинаков Никита Андреевич</p><p>Контакты:</p><p>VK: <a href='https://vk.com/id301204653'>Усольцев Олег</a></p><p>Telegram: <a href='https://t.me/Darks_SM'>https://t.me/Darks_SM</a></p>")))
           ))
)

# --- 3. Server ---
server <- function(input, output, session) {
  uploadedData <- reactiveVal(NULL)
  fullTableData <- reactiveValues(data = NULL)
  fileHistoryData <- reactiveVal(data.frame(Файл = character(), Дата = character(), Время = character()))
  newRows <- reactiveVal(data.frame())
  selectedFile <- reactive(input$selectedFile)
  
  observeEvent(input$uploadFile, {
    req(input$uploadFile)
    filePath <- input$uploadFile$datapath
    fileName <- input$uploadFile$name
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Загрузка...", value = 0)
    tryCatch({
      file.copy(filePath, file.path("save", fileName), overwrite = TRUE)
      file_info <- file.info(file.path("save", fileName))
      new_row <- data.frame(File = fileName, Date = format(file_info$mtime, "%Y-%m-%d"), Time = format(file_info$mtime, "%H:%M:%S"))
      fileHistoryData(rbind(fileHistoryData(), new_row))
      showNotification(paste("Файл", fileName, "загружен!"), type = "message")
      progress$set(value = 1)
    }, error = function(e) {
      showNotification(paste("Ошибка загрузки:", e$message), type = "error")
      progress$set(value = 0)
    })
  })
  
  output$uploadProgress <- renderUI({
    req(input$uploadFile)
    # Индикатор прогресса (можно улучшить)
  })
  
  output$fileHistory <- renderUI({
    fhData <- fileHistoryData()
    if (nrow(fhData) > 0) {
      fileHistoryTable <- datatable(fhData, selection = "single", escape = FALSE, options = list(dom = 't', rownames = FALSE))
      tagList(h3("История файлов:"), fileHistoryTable)
    } else {
      p("История пуста.")
    }
  })
  
  observeEvent(input$fileHistory_rows_selected, {
    selected_row <- input$fileHistory_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      fhData <- fileHistoryData()
      selected_file <- file.path("save", fhData[selected_row, "File"])
      if (file.exists(selected_file)) {
        tryCatch({
          data <- load_data(selected_file)
          fullTableData$data <- data
        }, error = function(e) {
          showNotification(paste("Ошибка открытия файла:", e$message), type = "error", duration = NULL)
        })
      } else {
        showNotification("Файл не найден!", type = "error", duration = NULL)
      }
    }
  })
  
  output$fileSelector <- renderUI({
    files <- load_files()
    if (length(files) > 0) {
      selectInput("selectedFile", "Выберите файл:", choices = files)
    } else {
      p("Нет файлов.")
    }
  })
  
  observeEvent(input$selectedFile, {
    filePath <- input$selectedFile
    if (file.exists(filePath)) {
      tryCatch({
        data <- load_data(filePath)
        fullTableData$data <- data
      }, error = function(e) {
        showNotification(paste("Ошибка загрузки файла:", e$message), type = "error")
      })
    }
  })
  
  output$editTable <- renderDT({
    req(fullTableData$data)
    datatable(fullTableData$data, editable = TRUE, options = list(lengthChange = FALSE, dom = 'Bfrtip', buttons = c('csv', 'excel')))
  })
  
  observeEvent(input$saveChanges, {
    req(input$selectedFile, fullTableData$data)
    filePath <- input$selectedFile
    edited_data <- tryCatch({
      if (!is.null(input$editTable_cell_edit)) {
        edited_rows <- input$editTable_cell_edit$row
        edited_cols <- input$editTable_cell_edit$col
        edited_vals <- input$editTable_cell_edit$value
        for (i in seq_along(edited_rows)) {
          fullTableData$data[edited_rows[i], edited_cols[i]] <- edited_vals[i]
        }
        fullTableData$data
      } else {
        showNotification("Изменений нет.", type = "warning")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste("Ошибка обработки изменений:", e$message), type = "error")
      return(NULL)
    })
    if (is.null(edited_data) || nrow(edited_data) == 0) {
      showNotification("Таблица пуста.", type = "warning")
      return()
    }
    tryCatch({
      save_file(edited_data, filePath, encoding = input$encoding)
      showNotification("Изменения сохранены!", type = "message")
    }, error = function(e) {
      showNotification(paste("Ошибка сохранения:", e$message), type = "error")
    })
  })
  
  output$downloadEditedFile <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$selectedFile), ".", tolower(input$outputFormat))
    },
    content = function(file) {
      ext <- tolower(input$outputFormat)
      data <- fullTableData$data
      tryCatch({
        switch(ext,
               csv = write.csv(data, file, na = "", fileEncoding = input$encoding),
               txt = write.table(data, file, sep = ";", na = "", row.names = FALSE, quote = FALSE, fileEncoding = input$encoding),
               xlsx = write_xlsx(data, file)
        )
      }, error = function(e) {
        showNotification(paste("Ошибка скачивания:", e$message), type = "error")
      })
    }
  )
  
  
  observeEvent(input$addRow, {
    if(is.null(fullTableData$data)){
      showNotification("Загрузите данные!", type = "warning")
      return()
    }
    req(fullTableData$data)
    new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(fullTableData$data)))
    colnames(new_row) <- colnames(fullTableData$data)
    fullTableData$data <- rbind(fullTableData$data, new_row)
  })
  
  observeEvent(input$deleteRow, {
    req(input$editTable_rows_selected, fullTableData$data)
    rows_to_delete <- input$editTable_rows_selected
    if (length(rows_to_delete) > 0) {
      fullTableData$data <- fullTableData$data[-rows_to_delete, , drop = FALSE]
    }
  })
  
  # Рендеринг UI для выбора файла для статистики
  output$fileSelectorStats <- renderUI({
    files <- load_files() # Список файлов
    # Проверка на наличие файлов
    if (length(files) > 0) {
      selectInput("selectedFileStats", "Выберите файл для статистики:", choices = files) # Вывод селектора файлов
    } else {
      p("Нет загруженных файлов для статистики.") # Если файлов нет
    }
  })
  
  # Рендеринг таблицы со статистикой по классам и предметам
  output$statsTable <- renderDT({
    req(input$selectedFileStats) # Проверка на наличие выбранного файла
    tryCatch({
      # Загрузка и обработка данных
      data <- load_data(input$selectedFileStats)
      processed_data <- process_data(data)
      datatable(processed_data$by_class_subject) # Вывод таблицы
    }, error = function(e) {
      # Вывод сообщения об ошибке
      showNotification(paste("Ошибка обработки статистики:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Рендеринг таблицы со статистикой по предметам
  output$subjectStatsTable <- renderDT({
    req(input$selectedFileStats) # Проверка на наличие выбранного файла
    tryCatch({
      # Загрузка и обработка данных
      data <- load_data(input$selectedFileStats)
      processed_data <- process_data(data)
      datatable(processed_data$by_subject) # Вывод таблицы
    }, error = function(e) {
      # Вывод сообщения об ошибке
      showNotification(paste("Ошибка обработки статистики:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Рендеринг UI для выбора файла для графиков
  output$fileSelectorPlot <- renderUI({
    files <- load_files() # Список файлов
    # Проверка на наличие файлов
    if (length(files) > 0) {
      selectInput("selectedFilePlot", "Выберите файл для графиков:", choices = files) # Вывод селектора файлов
    } else {
      p("Нет загруженных файлов для графиков.") # Если файлов нет
    }
  })
  
  # Рендеринг графика средней оценки по классам и предметам
  output$classSubjectStatsPlot <- renderPlot({
    req(input$selectedFilePlot) # Проверка на наличие выбранного файла
    tryCatch({
      # Загрузка и обработка данных
      data <- load_data(input$selectedFilePlot)
      processed_data <- process_data(data)
      # Построение графика
      ggplot(processed_data$by_class_subject, aes(x = class, y = Mean, fill = factor(Оценка))) + #Изменено для отображения оценок
        geom_col(position = "dodge") +  
        labs(title = "Средние оценки по классам и предметам", x = "Класс", y = "Средняя оценка", fill = "Оценка") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }, error = function(e) {
      # Вывод сообщения об ошибке
      showNotification(paste("Ошибка построения графика:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Рендеринг графика распределения оценок по предметам
  output$subjectStatsPlot <- renderPlot({
    req(input$selectedFilePlot) # Проверка на наличие выбранного файла
    tryCatch({
      # Загрузка и обработка данных
      data <- load_data(input$selectedFilePlot)
      processed_data <- process_data(data)
      
      # Подготовка данных для круговой диаграммы
      plot_data <- processed_data$by_subject %>%
        group_by(Предмет, Оценка) %>%
        summarise(Count = sum(Count)) %>%
        mutate(Percent = Count / sum(Count) * 100) %>%
        ungroup()
      
      # Построение круговой диаграммы
      p <- ggplot(plot_data, aes(x = "", y = Percent, fill = factor(Оценка))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y", start = 0) +
        geom_text(aes(label = paste0(round(Percent, 1), "%")), 
                  position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Процентное распределение оценок по предметам", fill = "Оценка") +
        theme_void() +
        theme(legend.position = "bottom") # Легенда внизу
      
      # Динамическое определение количества столбцов для facet_wrap
      p + facet_wrap(~ Предмет, ncol = length(unique(plot_data$Предмет)))
      
    }, error = function(e) {
      # Вывод сообщения об ошибке
      showNotification(paste("Ошибка построения графика:", e$message), type = "error")
      return(NULL)
    })
  })
  
  if (!dir.exists("save")) dir.create("save")
}

shinyApp(ui, server)