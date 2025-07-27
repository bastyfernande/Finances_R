library(haven)
library(tidyverse)
library(readxl)
library(ggplot2)
library(quantmod)
library(stats)
library(zoo)
library(xts)
library(tidyquant)
library(shiny)
library(flexdashboard)
library(shinythemes)
library(renv)
library(stringr)
library(purrr)
library(shinydashboard)
library(usethis)
library(renv)
init()
[1] "C:/Users/basti/OneDrive/Escritorio/R for finances/Finances_R/Entrega 03"
#Descarga de datos
calcular_retornos_semanales <- function(ticker) {
  tryCatch({
    datos <- getSymbols(ticker, src = "yahoo", from = "2023-01-01", auto.assign = FALSE)
    df <- data.frame(datos)
    df$date <- as.Date(rownames(df))
    col_name <- paste0(ticker, ".Close")
    df <- df %>% select(date, !!sym(col_name))
    colnames(df)[2] <- "Close"
    
    df <- df %>%
      mutate(log_precios = log(Close),
             Retornos = c(NA, diff(log_precios)),
             semana = floor_date(date, "week")) %>%
      group_by(semana) %>%
      summarise(
        retorno_promedio = mean(Retornos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ticker = ticker,
        retorno_simple = exp(retorno_promedio) - 1
      )
    
    df
  }, error = function(e) NULL)
}
get_datos_diarios <- function(ticker) {
  tryCatch({
    datos <- getSymbols(ticker, src = "yahoo", from = "2023-01-01", auto.assign = FALSE)
    df <- data.frame(datos)
    df$date <- as.Date(rownames(df))
    col_name <- paste0(ticker, ".Close")
    df <- df %>% select(date, !!sym(col_name))
    colnames(df)[2] <- "Close"
    
    df <- df %>%
      mutate(
        ticker = ticker,
        log_precios = log(Close),
        Retornos = c(NA, diff(log_precios)),
        semana = floor_date(date, "week")
      )
    df
  }, error = function(e) NULL)
}

# Listado de tickers
tickers <- read_xlsx("S&P 500 Companies (Standard and Poor 500).xlsx")
labels <- tickers$Symbol
#Obtener los datos diarios de cada ticker
diarios_USA <- map_dfr(labels, get_datos_diarios)
diarios_CL <- map_dfr(labels_chilenos, get_datos_diarios)

#Aplicar la función a cada ticker (SP500)
resultados <- map_dfr(labels, calcular_retornos_semanales)
ultimos <- resultados %>%
  group_by(ticker) %>%
  filter(semana == max(semana)) %>%
  ungroup()
ultimos %>% arrange(desc(retorno_promedio))
#Listado de tickers chilenos
tickers_chilenos <- read_xlsx("Empresas_Chilenas_Yahoo_con_Datos_Financieros.xlsx")
labels_chilenos <- tickers_chilenos$Tickets
#Aplicar la función a cada ticker chileno
resultados_chilenos <- map_dfr(labels_chilenos, calcular_retornos_semanales)
ultimos_chilenos <- resultados_chilenos %>%
  group_by(ticker) %>% 
  filter(semana == max(semana)) %>%
  ungroup()
TOP10_CL <- ultimos_chilenos %>% arrange(desc(retorno_promedio)) %>% slice_head(n= 10) 
TOP10_USA <- ultimos %>% arrange(desc(retorno_promedio)) %>% slice_head(n= 10) 
# Ahora podemos graficarlos

ggplot(TOP10_USA, aes(x = reorder(ticker, retorno_promedio), y = retorno_promedio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 USA: Retorno Promedio", x = "Ticker", y = "Retorno (%)") +
  theme_minimal()


# Crear la interfaz de usuario
# Suponiendo que ya cargaste tus datasets: TOP10_CL y TOP10_USA

ui <- fluidPage(
  titlePanel("Dashboard de Rendimiento Semanal de Acciones"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pais", "Selecciona región:", choices = c("Chile", "USA")),
      checkboxInput("mostrar_todo", "Mostrar todas las acciones", value = FALSE)
    ),
    mainPanel(
      plotOutput("grafico_top"),
      tableOutput("tabla_top")
    )
  )
)

server <- function(input, output) {
  datos_seleccionados <- reactive({
    if (input$pais == "Chile") {
      if (input$mostrar_todo) {
        ultimos_chilenos
      } else {
        TOP10_CL
      }
    } else {
      if (input$mostrar_todo) {
        ultimos
      } else {
        TOP10_USA
      }
    }
  })
  
  output$grafico_top <- renderPlot({
    df <- datos_seleccionados()
    ggplot(df, aes(x = reorder(ticker, retorno_promedio), y = retorno_promedio, fill = ticker)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Ticker", y = "Retorno Promedio Semanal", title = paste("Top Acciones -", input$pais)) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$tabla_top <- renderTable({
    datos_seleccionados() %>% select(ticker, semana, retorno_promedio, retorno_simple)
  })
}

shinyApp(ui, server)
# Guardar el entorno de trabajo
renv::snapshot()
