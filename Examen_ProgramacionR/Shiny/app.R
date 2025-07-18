# app.R
# Install if needed: install.packages(c("shiny", "tidyverse", "plotly", "readxl", "here", "tools", "DT", "lubridate", "bslib", "shinycssloaders", "data.table"))


rm(list = ls())  # Limpiar entorno de trabajo para evitar conflictos
gc()             # Liberar memoria no utilizada


# Carga de librerías necesarias para la aplicación Shiny
library(shiny)
library(tidyverse) # Incluye dplyr, ggplot2, readr, tibble, stringr
library(plotly)    # Para gráficos interactivos
library(readxl)    # Para leer archivos Excel
library(here)      # Para gestionar rutas de archivos de forma robusta
library(tools)     # Para funciones de manejo de rutas de archivos
library(DT)        # Para tablas interactivas
library(lubridate) # Para manejo de fechas
library(bslib)     # Para temas de Shiny
library(shinycssloaders) # Para spinners de carga
library(data.table) # Para carga y procesamiento eficiente de datos grandes

# --- Definición de funciones ---

#' @title Cargar archivo de raíz del proyecto
#' @description Carga archivos de texto (.txt, .csv) o Excel (.xlsx, .xls) en un data.table para eficiencia.
#'    Usa fread para TXT/CSV (rápido para datos grandes) y read_excel para Excel.
#' @param nombre_archivo Nombre del archivo (relativo a la raíz del proyecto).
#' @param asignar_globalmente Si TRUE, asigna el data.table al entorno global (no recomendado en Shiny).
#' @param encoding Codificación del archivo (por defecto, "UTF-8").
#' @param ... Argumentos adicionales para fread o read_excel.
#' @return data.table con los datos cargados o NULL si ocurre un error.
cargar_archivo <- function(nombre_archivo, asignar_globalmente = FALSE, encoding = "UTF-8", ...) {
  ruta_archivo <- here::here(nombre_archivo)
  if (!file.exists(ruta_archivo)) {
    warning("El archivo '", ruta_archivo, "' no existe en el proyecto.")
    return(NULL)
  }
  
  nombre_base <- basename(ruta_archivo)
  # Generar un nombre de objeto limpio
  nombre_objeto <- tools::file_path_sans_ext(nombre_base) %>%
    stringr::str_replace_all("[^a-zA-Z0-9_]", "_") %>%
    stringr::str_replace_all("^_+|_+$", "") %>%
    { if (stringr::str_detect(., "^[0-9]|^$")) paste0("datos_", .) else . }
  if (nchar(nombre_objeto) == 0) nombre_objeto <- "datos_genericos"
  
  extension <- tolower(tools::file_ext(ruta_archivo))
  is_excel <- extension %in% c("xls", "xlsx")
  
  datos_cargados <- tryCatch({
    if (is_excel) {
      as.data.table(readxl::read_excel(ruta_archivo, ...))
    } else {
      fread(file = ruta_archivo, encoding = encoding, sep = "auto", ...)
    }
  }, error = function(e) {
    warning("Error al cargar '", nombre_base, "': ", e$message)
    NULL
  })
  
  if (is.null(datos_cargados)) return(NULL)
  
  if (asignar_globalmente) {
    assign(nombre_objeto, datos_cargados, envir = .GlobalEnv)
    message("Archivo '", nombre_base, "' cargado como '", nombre_objeto, "'.")
  }
  
  return(datos_cargados)
}

# --- Carga de datos inicial (una vez al inicio) ---
data_sii <- cargar_archivo("Data_SII.txt")
nivel_act_eco <- cargar_archivo("Nivel_Act_eco.txt")

if (is.null(data_sii) || is.null(nivel_act_eco)) {
  stop("Error: No se pudieron cargar los archivos de datos.")
}

# Convertir fechas y pre-join (usando data.table para eficiencia)
data_sii[, fec_actividao := ymd(fec_actividao)]

# Join eficiente con data.table (clave en 'clase')
setkey(data_sii, clase)
setkey(nivel_act_eco, clase)
data_completa_raw <- nivel_act_eco[data_sii, on = "clase", nomatch = 0]  # Inner join, o usa allow.cartesian=TRUE si grande

# Fallback para descripciones NA
data_completa_raw[is.na(gls_seccion), gls_seccion := as.character(seccion)]
data_completa_raw[is.na(gls_division), gls_division := as.character(division)]
data_completa_raw[is.na(gls_clase), gls_clase := as.character(clase)]

# Regiones únicas para filtro
unique_regions <- sort(unique(data_completa_raw$region))

# --- Interfaz de Usuario (UI) ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel(div(tags$img(src="https://www.sii.cl/mismenu/menu/sii_logo_horizontal.svg", height = "50px", style = "margin-right: 10px;"), "Dashboard de Actividades Económicas del SII")),
  sidebarLayout(
    sidebarPanel(
      h3("Opciones de Visualización"),
      selectInput("group_by", "Agrupar Actividades por:", 
                  choices = c("Sección (e.g., A - Agricultura)" = "seccion",
                              "División (2 dígitos)" = "division",
                              "Clase (4 dígitos)" = "actividad")),
      dateRangeInput("date_range", "Filtrar por Fecha de Actividad:",
                     start = min(data_completa_raw$fec_actividao, na.rm = TRUE),
                     end = max(data_completa_raw$fec_actividao, na.rm = TRUE),
                     format = "dd/mm/yyyy"),
      selectizeInput("regions", "Filtrar por Región(es):", choices = unique_regions, multiple = TRUE, selected = unique_regions),
      helpText("Filtre por fecha y región para analizar la distribución de actividades económicas (CIIU). Para datasets muy grandes (>10M filas), considere pre-agregar datos o usar una base de datos como SQLite.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Actividades Económicas",
                 h4("Distribución por Clasificación CIIU"),
                 withSpinner(plotlyOutput("ciiu_plot", height = "400px")),
                 h4("Tabla de Distribución CIIU"),
                 withSpinner(DT::dataTableOutput("ciiu_table")),
                 downloadButton("download_ciiu_table", "Descargar Tabla CIIU")
        ),
        tabPanel("Distribución Regional",
                 h4("Distribución por Región"),
                 withSpinner(plotlyOutput("region_plot", height = "400px")),
                 h4("Tabla de Distribución Regional"),
                 withSpinner(DT::dataTableOutput("region_table")),
                 downloadButton("download_region_table", "Descargar Tabla Regional")
        ),
        tabPanel("Tendencias Temporales",
                 h4("Actividades Económicas a lo Largo del Tiempo"),
                 withSpinner(plotlyOutput("time_series_plot", height = "400px")),
                 downloadButton("download_time_series_data", "Descargar Datos Temporales")
        ),
        tabPanel("Acerca de",
                 h3("Información del Dashboard"),
                 p("Mejorado para manejar datos grandes con data.table y caching. Fuente: SII Chile. Última actualización: Julio 2025.")
        )
      )
    )
  )
)

# --- Lógica del Servidor ---
server <- function(input, output, session) {
  
  # Datos filtrados (con cache para performance)
  filtered_data <- reactive({
    validate(need(nrow(data_completa_raw) > 0, "No hay datos disponibles."))
    data_completa_raw[fec_actividao %between% input$date_range & region %in% input$regions]
  }) %>% bindCache(input$date_range, input$regions)
  
  # Agrupamiento CIIU
  ciiu_grouped_data <- reactive({
    grupo <- input$group_by
    col_agrup <- switch(grupo, "seccion" = "gls_seccion", "division" = "gls_division", "actividad" = "gls_clase", grupo)
    filtered_data()[, .(Categoria = get(col_agrup), `Número de Empresas` = .N), by = col_agrup][order(-`Número de Empresas`)]
  }) %>% bindCache(input$group_by, filtered_data())
  
  # Agrupamiento Regional
  region_grouped_data <- reactive({
    filtered_data()[, .(`Número de Empresas` = .N), by = .(Región = region)][order(-`Número de Empresas`)]
  }) %>% bindCache(filtered_data())
  
  # Datos Temporales (por año)
  time_series_data <- reactive({
    filtered_data()[, .(Año = year(fec_actividao), `Número de Empresas` = .N), by = .(Año)][order(Año)]
  }) %>% bindCache(filtered_data())
  
  # Gráfico CIIU
  output$ciiu_plot <- renderPlotly({
    plot_data <- head(ciiu_grouped_data(), 30)
    plot_data[, Categoria_abrev := substr(Categoria, 1, 30)]
    
    p <- ggplot(plot_data, aes(x = reorder(Categoria_abrev, -`Número de Empresas`), y = `Número de Empresas`, 
                               text = paste("Categoría:", Categoria, "<br>Empresas:", `Número de Empresas`))) +
      geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + 
      labs(title = paste("Distribución por", input$group_by, "CIIU"), x = input$group_by, y = "Empresas") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  # Tabla CIIU
  output$ciiu_table <- DT::renderDataTable(DT::datatable(ciiu_grouped_data(), options = list(pageLength = 10, searching = TRUE), rownames = FALSE))
  
  output$download_ciiu_table <- downloadHandler(filename = function() {paste("ciiu_", Sys.Date(), ".csv", sep="")}, content = function(file) {fwrite(ciiu_grouped_data(), file)})
  
  # Gráfico Regional
  output$region_plot <- renderPlotly({
    plot_data <- region_grouped_data()
    plot_data[, Región_abrev := substr(Región, 1, 30)]
    
    p <- ggplot(plot_data, aes(x = reorder(Región_abrev, -`Número de Empresas`), y = `Número de Empresas`, 
                               text = paste("Región:", Región, "<br>Empresas:", `Número de Empresas`))) +
      geom_bar(stat = "identity", fill = "darkgreen") + theme_minimal() + 
      labs(title = "Distribución por Región", x = "Región", y = "Empresas") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  # Tabla Regional
  output$region_table <- DT::renderDataTable(DT::datatable(region_grouped_data(), options = list(pageLength = 10, searching = TRUE), rownames = FALSE))
  
  output$download_region_table <- downloadHandler(filename = function() {paste("regional_", Sys.Date(), ".csv", sep="")}, content = function(file) {fwrite(region_grouped_data(), file)})
  
  # Gráfico Temporal
  output$time_series_plot <- renderPlotly({
    p <- ggplot(time_series_data(), aes(x = Año, y = `Número de Empresas`, text = paste("Año:", Año, "<br>Empresas:", `Número de Empresas`))) +
      geom_line(color = "blue") + geom_point(color = "red") + theme_minimal() + 
      labs(title = "Tendencias de Actividades por Año", x = "Año", y = "Número de Empresas")
    ggplotly(p, tooltip = "text")
  })
  
  output$download_time_series_data <- downloadHandler(filename = function() {paste("temporal_", Sys.Date(), ".csv", sep="")}, content = function(file) {fwrite(time_series_data(), file)})
}

# Ejecutar la app
shinyApp(ui = ui, server = server)
