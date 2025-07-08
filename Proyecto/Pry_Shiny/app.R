#install.packages(shiny)
#install.packages("shinydashboard")
#install.packages("readr")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(vcd)
#Datos

# Cargar Nivel_Act_Eco.csv con nombres de columnas corregidos
niv_act_eco <- read_csv2("Nivel_Act_Eco.csv", col_types = cols(
  Sec = col_character(),
  Gls_Sec = col_character(),
  Div = col_character(),
  Gls_Div = col_character(),
  Cls = col_character(),
  Gls_Cls = col_character()
))


Nom_Seccion <- read_csv2("Nom_Seccion.csv", col_types = cols(
  Sec = col_character(),
  Gls_Sec = col_character()
))
data_sii <- read_csv("Data_SII.csv", col_types = cols(
  Seccion = col_character(),
  Division = col_character(),
  Clase = col_character(),
  Reg = col_character(),
  FEC_ACT_ECO = col_date(format = "%Y-%m-%d")
))

# ---- Crear Lista Jerárquica Eficientemente ----
# Estructura: lista[Sector][División][Clase]

list_act_eco <- niv_act_eco %>%
  mutate(
    id_sec = paste(Sec, Gls_Sec, sep = ": "),
    id_div = paste(Div, Gls_Div, sep = ": "),
    id_cls = paste(Cls, Gls_Cls, sep = ": ")
  ) %>%
  nest(cls_data = c(Cls, Gls_Cls, id_cls)) %>%
  group_by(Sec, Gls_Sec, id_sec, Div, Gls_Div, id_div) %>%
  summarise(cls_list = list(cls_data), .groups = "drop") %>%
  nest(div_data = c(Div, Gls_Div, id_div, cls_list)) %>%
  group_by(Sec, Gls_Sec, id_sec) %>%
  summarise(div_list = list(setNames(div_data, div_data$id_div)), .groups = "drop") %>%
  { setNames(.$div_list, .$id_sec) }



# UI
ui <- dashboardPage(
  title = "Dashboard",
  skin = "yellow",
  dashboardHeader(title = "Poyecto Shiny"),
  dashboardSidebar(sidebarMenu(
    id = "sidebarID",
    menuItem("Sector por Región"),
    selectInput("seccion", "Seleccion del Sector Económico:", choices = setNames(Nom_Seccion$Sec, Nom_Seccion$Gls_Sec), selected = "A"),
    menuItem("Segunda ventana", id = "chartsID")
  )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel(
        
        title = "Gráfico Sector por Región",
        box(
          width = 6,
          status = "primary",
          plotOutput("Grf_Sec_Reg")
        )
      ),
      tabPanel("Summary", tableOutput("summaryTable")),
      tabPanel("Datos")
    )
  )
)
# Server
server <- function(input, output) {
  # Filtro data_sii bpor seccion
  fil_data_sii <- reactive({
    data_sii %>% filter(Seccion == input$seccion)
  })
  
  # Glosa de la seccion
  # Get Gls_Sec for the selected Sec
  gls_sec <- reactive({
    req(input$seccion)
    Nom_Seccion %>%
      filter(Sec == input$seccion, !is.na(Sec)) %>%
      pull(Gls_Sec) %>%
      first()  # Take first value if multiple matches
  })
  
  # Genera bar plot
  output$Grf_Sec_Reg <- renderPlot({
    plot_data_sii <- fil_data_sii() %>%
      group_by(Reg) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    ggplot(plot_data_sii, aes(x = reorder(Reg, -Count), y = Count, fill = Reg)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste(gls_sec()),
        x = "Region",
        y = "Total"
      )
  })
  
  # Tabla summary
  output$summaryTable <- renderTable({
    fil_data_sii() %>%
      group_by(Seccion, Reg) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
  })
  
}


# Run the app
shinyApp(ui, server)