library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Test",
    titleWidth = 500),
  
  dashboardSidebar(
    sidebarMenu(id = "Menu1",
                sidebarMenuOutput("Menu"))),
  
  dashboardBody(
    shinyjs::useShinyjs(), # required to enable Shinyjs
    tabItems(
      
      tabItem(tabName = "HF_Page1",
              box(title = "A. People who live in the house", width = NULL, solidHeader = TRUE, status = "primary",
                  uiOutput("HF_Page1"),
                  actionButton("add_btn", "Add a person"),
                  actionButton("rm_btn", "Remove last person"),
                  textOutput("counter"))),
      
      tabItem(tabName = "HF_Page2",
              box(title = "B. Responses", width = NULL, solidHeader = TRUE, status = "primary",
                  DT::dataTableOutput("persons", width = 300), tags$hr()))
      
    ) # tabItems
  ) # dashboardBody
) # dashboardPage

