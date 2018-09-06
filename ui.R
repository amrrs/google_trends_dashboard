library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title="By Fish"),
  
  dashboardSidebar(
    br(),
    
    
    h6(" Search Term(s)",style="text-align:center;color:#FFA319;font-size:150%"),
    
    helpText("Give one or more terms that you want R to retrieve data from the Google Trends API.
             Use comma to separate terms", style="text-align:center"),
    
    textInput('terms',''),
    
    
    selectInput("geography", 
                label = tags$h4(strong(em("Geography")),style="text-align:center;color:#FFA319;font-size:150%"),
                choices = c("Worldwide"),
                selected = "Worldwide"),           
    selectInput("period", 
                label = tags$h4(strong(em("Time Period")),style="text-align:center;color:#FFA319;font-size:150%"),
                choices = c("2004-present",
                            "Past30Days",
                            "Past90Days",
                            "Past12Months",
                            "2011",
                            "2012",
                            "2013",
                            "2014",
                            "2015"
                ),
                selected = "2004-present"),
    
    checkboxInput("corr", 
                  label = strong("Correlation",style="text-align:center;color:#FFA319;font-size:150%")),
    br(),
    
    tags$h1(submitButton("Update!"),style="text-align:center"),
    helpText("To get results, click the 'Update!' button",style="text-align:center"),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    
    
    
  ),
  
  
  #####
  ##  Main Panel
  #### help ====        
  dashboardBody(    
    fluidRow(
      br(),
      h5(em(strong("Google Trends Analytics", style="color:darkblue;font-size:210%")),align = "center"),
      
      plotOutput("myplot"),
      br(),
      plotOutput("myplot3"),
      plotOutput("myplot2")
      
      
    )
  ))