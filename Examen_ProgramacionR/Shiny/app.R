# app.R

# Carga de librerías necesarias para la aplicación Shiny
library(shiny)
library(tidyverse) # Incluye dplyr, ggplot2, readr, tibble, stringr
library(plotly)    # Para gráficos interactivos
library(readxl)    # Para leer archivos Excel
library(here)      # Para gestionar rutas de archivos de forma robusta
library(tools)     # Para funciones de manejo de rutas de archivos

# --- Definición de funciones ---

#' @title Cargar archivo de raíz del proyecto
#' @description Carga archivos de texto (.txt, .csv) o Excel (.xlsx, .xls) en un tibble.
#'   Incluye detección automática de delimitadores y manejo de codificaciones.
#' @param nombre_archivo Nombre del archivo (relativo a la raíz del proyecto).
#' @param asignar_globalmente Si TRUE, asigna el tibble al entorno global con un nombre limpio.
#'   En aplicaciones Shiny, generalmente se recomienda FALSE.
#' @param encoding Codificación del archivo (por defecto, "UTF-8").
#' @param delim Delimitador manual para archivos de texto (NULL para detección automática).
#' @param ... Argumentos adicionales para read_delim o read_excel.
#' @return Tibble con los datos cargados o NULL si ocurre un error o el archivo no existe.
cargar_archivo <- function(nombre_archivo, asignar_globalmente = FALSE, encoding = "UTF-8", delim = NULL, ...) {
  ruta_archivo <- here::here(nombre_archivo)
  if (!file.exists(ruta_archivo)) {
    warning("El archivo '", ruta_archivo, "' no existe en el proyecto.")
    return(NULL)
  }
  
  nombre_base <- basename(ruta_archivo)
  # Generar un nombre de objeto limpio y válido para R
  nombre_objeto <- tools::file_path_sans_ext(nombre_base) %>%
    stringr::str_replace_all("[^a-zA-Z0-9_]", "_") %>% # Reemplazar caracteres no válidos con guiones bajos
    stringr::str_replace_all("^_+|_+$", "") %>% # Eliminar guiones bajos al inicio o final
    { if (stringr::str_detect(., "^[0-9]|^$")) paste0("datos_", .) else . } # Prepend "datos_" si empieza con número o está vacío
  if (nchar(nombre_objeto) == 0) {
    nombre_objeto <- "datos_genericos"
    warning("Nombre de objeto generado inválido. Usando 'datos_genericos'.")
  }
  
  extension <- tolower(tools::file_ext(ruta_archivo))
  is_excel <- extension %in% c("xls", "xlsx")
  
  # Función interna para detectar el delimitador de archivos de texto
  detectar_delimitador <- function(ruta, posibles = c(",", "\t", ";", "|")) {
    lineas <- readLines(ruta, n = 5, encoding = encoding, warn = FALSE) # Leer las primeras 5 líneas
    conteos <- sapply(posibles, function(d) sum(stringr::str_count(lineas, stringr::fixed(d))))
    if (all(conteos == 0)) return(",") # Si no se encuentra ningún delimitador, asumir coma
    posibles[which.max(conteos)] # Devolver el delimitador más frecuente
  }
  
  datos_cargados <- tryCatch({
    if (is_excel) {
      readxl::read_excel(ruta_archivo, ...)
    } else {
      delim_final <- if (is.null(delim)) detectar_delimitador(ruta_archivo) else delim
      readr::read_delim(
        file = ruta_archivo,
        delim = delim_final,
        locale = readr::locale(encoding = encoding), # Especificar codificación
        escape_double = FALSE,
        trim_ws = TRUE,
        show_col_types = FALSE, # No mostrar mensajes de tipos de columna
        ...
      )
    }
  }, error = function(e) {
    warning("Error al cargar '", nombre_base, "': ", e$message)
    NULL # Retornar NULL en caso de error
  })
  
  if (is.null(datos_cargados)) return(NULL)
  
  datos_tibble <- tibble::as_tibble(datos_cargados) # Convertir a tibble
  
  # Asignar al entorno global si se especifica (no recomendado en Shiny)
  if (asignar_globalmente) {
    assign(nombre_objeto, datos_tibble, envir = .GlobalEnv)
    message("Archivo '", nombre_base, "' cargado como '", nombre_objeto, "'.")
  }
  
  return(datos_tibble)
}

#' @title Analizar distribuciones de actividades y regiones
#' @description Genera gráficos y tablas de distribución para actividades económicas y regiones.
#' @param datos Un tibble con los datos a analizar.
#' @param agrupar_ciiu Columna a usar para agrupar las actividades CIIU ("seccion", "division", "actividad").
#' @param color_ciiu Color para las barras del gráfico CIIU.
#' @param color_region Color para las barras del gráfico de región.
#' @param tema_ggplot Tema de ggplot2 a aplicar (e.g., `theme_minimal()`).
#' @param retorno Tipo de retorno deseado ("graficos", "tablas", "ambos").
#' @return Una lista conteniendo los gráficos y/o tablas de distribución.
analizar_distribuciones <- function(datos, agrupar_ciiu = "seccion",
                                    color_ciiu = "steelblue", color_region = "darkgreen",
                                    tema_ggplot = theme_minimal(), retorno = "graficos") {
  
  # Calcular distribución por CIIU
  dist_ciiu <- datos %>%
    dplyr::count(.data[[agrupar_ciiu]]) %>%
    dplyr::arrange(dplyr::desc(n))
  
  # Calcular distribución por Región
  dist_region <- datos %>%
    dplyr::count(region) %>%
    dplyr::arrange(dplyr::desc(n))
  
  graficos <- NULL
  if (retorno %in% c("graficos", "ambos")) {
    # Gráfico de distribución por CIIU
    grafico_ciiu <- ggplot2::ggplot(dist_ciiu, ggplot2::aes(x = reorder(.data[[agrupar_ciiu]], -n), y = n)) +
      ggplot2::geom_bar(stat = "identity", fill = color_ciiu) +
      tema_ggplot +
      ggplot2::labs(
        title = paste("Distribución de Actividades por", ifelse(agrupar_ciiu == "seccion", "Sección", "División/Clase"), "CIIU"),
        x = ifelse(agrupar_ciiu == "seccion", "Sección CIIU", "División/Clase CIIU"),
        y = "Número de Empresas"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    # Gráfico de distribución por Región
    grafico_region <- ggplot2::ggplot(dist_region, ggplot2::aes(x = reorder(region, -n), y = n)) +
      ggplot2::geom_bar(stat = "identity", fill = color_region) +
      tema_ggplot +
      ggplot2::labs(
        title = "Distribución de Empresas por Región",
        x = "Región",
        y = "Número de Empresas"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    graficos <- list(ciiu = grafico_ciiu, region = grafico_region)
  }
  
  tablas <- list(ciiu = dist_ciiu, region = dist_region)
  
  # Retornar según el parámetro 'retorno'
  if (retorno == "tablas") {
    list(tablas = tablas)
  } else if (retorno == "graficos") {
    list(graficos = graficos)
  } else { # "ambos"
    list(graficos = list(ciiu = grafico_ciiu, region = grafico_region),
         tablas = list(ciiu = dist_ciiu, region = dist_region))
  }
}

# --- Carga de datos inicial para la aplicación Shiny ---
# Los datos se cargan una única vez al inicio de la aplicación.
# Se usa asignar_globalmente = FALSE ya que Shiny maneja sus propios objetos reactivos.
data_sii <- cargar_archivo("Data_SII.txt", asignar_globalmente = FALSE)
nivel_act_eco <- cargar_archivo("Nivel_Act_eco.txt", asignar_globalmente = FALSE)

# Verificar si los archivos se cargaron correctamente
if (is.null(data_sii) || is.null(nivel_act_eco)) {
  stop("Error: No se pudieron cargar uno o ambos archivos de datos (Data_SII.txt, Nivel_Act_eco.txt). Asegúrese de que estén en el directorio correcto y que los permisos sean adecuados.")
}

# Unir las bases para tener las descripciones de CIIU en Data_SII
# Asumiendo que 'actividad' en Data_SII se corresponde con 'clase' en Nivel_Act_eco
# y que 'seccion' y 'division' también se pueden unir o usar directamente.
# Se realiza un left_join para mantener todas las filas de data_sii.
data_completa <- data_sii %>%
  left_join(nivel_act_eco, by = c("actividad" = "clase"), suffix = c("", "_desc")) %>%
  # Coalesce para asegurar que las columnas de descripción (gls_seccion, etc.) no sean NA
  # si la unión no encuentra una coincidencia, usando el código original como fallback.
  mutate(
    gls_seccion = coalesce(gls_seccion, as.character(seccion)),
    gls_division = coalesce(gls_division, as.character(division)),
    gls_clase = coalesce(gls_clase, as.character(actividad))
  )

# --- Interfaz de Usuario (UI) ---
ui <- fluidPage(
  # Título del panel
  titlePanel("Dashboard de Actividades Económicas del SII"),
  
  # Diseño de la página con una barra lateral y un panel principal
  sidebarLayout(
    # Panel lateral para controles de usuario
    sidebarPanel(
      h3("Opciones de Visualización"), # Encabezado para las opciones
      # Selector para elegir cómo agrupar las actividades económicas
      selectInput("group_by",
                  "Agrupar Actividades por:",
                  choices = c("Sección" = "seccion",
                              "División" = "division",
                              "Clase" = "actividad")), # 'actividad' en Data_SII es la clase CIIU
      hr(), # Línea horizontal para separar elementos
      # Texto de ayuda para el usuario
      helpText("Este dashboard interactivo muestra la distribución de empresas por actividad económica (según clasificación CIIU) y por región, utilizando datos del Servicio de Impuestos Internos (SII) de Chile.")
    ),
    
    # Panel principal para mostrar los resultados (gráficos y tablas)
    mainPanel(
      # Pestañas para organizar el contenido
      tabsetPanel(
        # Pestaña para la distribución de actividades económicas
        tabPanel("Actividades Económicas",
                 h4("Distribución por Clasificación CIIU"),
                 plotlyOutput("ciiu_plot"), # Gráfico interactivo de CIIU
                 h4("Tabla de Distribución CIIU"),
                 tableOutput("ciiu_table")), # Tabla de distribución CIIU
        # Pestaña para la distribución regional
        tabPanel("Distribución Regional",
                 h4("Distribución por Región"),
                 plotlyOutput("region_plot"), # Gráfico interactivo de región
                 h4("Tabla de Distribución Regional"),
                 tableOutput("region_table")), # Tabla de distribución regional
        # Pestaña de información general
        tabPanel("Acerca de",
                 h3("Información del Dashboard"),
                 p("Desarrollado como parte del Examen de Programación en R."),
                 p("Fuente de Datos: Servicio de Impuestos Internos (SII), Chile."),
                 p("Última Actualización de Datos: Mayo 2025.")
        )
      )
    )
  )
)

# --- Lógica del Servidor ---
server <- function(input, output) {
  
  # Renderizar el gráfico interactivo de distribución CIIU
  output$ciiu_plot <- renderPlotly({
    req(data_completa) # Asegura que data_completa exista antes de intentar renderizar
    
    grupo_seleccionado <- input$group_by # Obtener la selección del usuario
    
    # Determinar la columna a usar para el agrupamiento, priorizando las descripciones
    col_agrupacion <- switch(grupo_seleccionado,
                             "seccion" = "gls_seccion",
                             "division" = "gls_division",
                             "actividad" = "gls_clase")
    
    # Fallback: si la columna de descripción no existe (ej. por un error en la unión), usar la columna original
    if (!(col_agrupacion %in% colnames(data_completa))) {
      col_agrupacion <- grupo_seleccionado
      warning(paste("No se encontró la columna de descripción para", grupo_seleccionado, ". Usando la columna original."))
    }
    
    # Calcular la distribución para el gráfico
    dist_ciiu_plot_data <- data_completa %>%
      count(.data[[col_agrupacion]]) %>% # Contar ocurrencias por la columna seleccionada
      arrange(desc(n)) %>% # Ordenar de forma descendente
      head(30) # Limitar a las 30 categorías más frecuentes para mejor visualización
    
    # Crear el gráfico de barras con ggplot2
    p <- ggplot(dist_ciiu_plot_data, aes(x = reorder(.data[[col_agrupacion]], -n), y = n,
                                         # Texto para el tooltip de plotly
                                         text = paste("Categoría:", .data[[col_agrupacion]], "<br>Empresas:", n))) +
      geom_bar(stat = "identity", fill = "steelblue") + # Barras de identidad con color azul
      theme_minimal() + # Tema minimalista de ggplot2
      labs(
        title = paste("Distribución de Empresas por", names(input$group_by[input$group_by == grupo_seleccionado]), "CIIU"),
        x = paste(names(input$group_by[input$group_by == grupo_seleccionado]), "CIIU"),
        y = "Número de Empresas"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje X
    
    ggplotly(p, tooltip = "text") # Convertir a gráfico interactivo con plotly
  })
  
  # Renderizar la tabla de distribución CIIU
  output$ciiu_table <- renderTable({
    req(data_completa)
    
    grupo_seleccionado <- input$group_by
    col_agrupacion <- switch(grupo_seleccionado,
                             "seccion" = "gls_seccion",
                             "division" = "gls_division",
                             "actividad" = "gls_clase")
    
    if (!(col_agrupacion %in% colnames(data_completa))) {
      col_agrupacion <- grupo_seleccionado
    }
    
    data_completa %>%
      count(.data[[col_agrupacion]]) %>%
      arrange(desc(n)) %>%
      rename(Categoria = 1, `Número de Empresas` = n) # Renombrar columnas para una mejor presentación en la tabla
  })
  
  # Renderizar el gráfico interactivo de distribución regional
  output$region_plot <- renderPlotly({
    req(data_completa)
    dist_region_plot_data <- data_completa %>%
      count(region) %>% # Contar ocurrencias por región
      arrange(desc(n)) # Ordenar de forma descendente
    
    # Crear el gráfico de barras con ggplot2
    p <- ggplot(dist_region_plot_data, aes(x = reorder(region, -n), y = n,
                                           # Texto para el tooltip de plotly
                                           text = paste("Región:", region, "<br>Empresas:", n))) +
      geom_bar(stat = "identity", fill = "darkgreen") + # Barras de identidad con color verde oscuro
      theme_minimal() + # Tema minimalista de ggplot2
      labs(
        title = "Distribución de Empresas por Región",
        x = "Región",
        y = "Número de Empresas"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje X
    
    ggplotly(p, tooltip = "text") # Convertir a gráfico interactivo con plotly
  })
  
  # Renderizar la tabla de distribución regional
  output$region_table <- renderTable({
    req(data_completa)
    data_completa %>%
      count(region) %>%
      arrange(desc(n)) %>%
      rename(Región = region, `Número de Empresas` = n) # Renombrar columnas para una mejor presentación
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
