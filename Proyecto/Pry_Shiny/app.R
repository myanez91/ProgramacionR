#install.packages(shiny)
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)


ui<-dashboardPage(title= "Dashboard", skin= "green",
                  dashboardHeader(title="PROYECTO"
                                  ),
                  
                  
                  dashboardSidebar(
                  
                  
                    sidebarMenu(id="sidebarID",
                                menuItem("Primera ventana"),
                                menuSubItem("Primera sub-ventana"),
                                menuItem("Segunda ventana",id = "chartsID",
                                         menuSubItem("Sub-ventana1"),
                                         menuSubItem("Sub-ventana2"),
                                         menuSubItem("Sub-ventana3",icon =icon("apple-pay"))
                                )
                    )
                    
                    
                  ),
                  dashboardBody(
                    
                    tabItems(tabItem(tabName = "datos", 
                                     DT::dataTableOutput("datos")
                                     
                                     
                    ),
                    tabItem(tabName = "montos", 
                            plotOutput("montos"))
                    
                    
                    ))
)


server <- function(input, output) { 
}

shinyApp(ui, server)