fields <- c("value_i", "fname_1", "lname_1", "sex1", "birth_year1", "spouse1", "mother1", "father1", "time1_1", "time1_2")

# Save a response
saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("persons")) {
    persons <<- rbind(persons, data)
  } else {
    persons <<- data
  }
}

loadData <- function() {
  if (exists("persons")) {
    persons
  }
}

server <- shinyServer(function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  output$Menu <- renderMenu({
    
    sidebarMenu(
      menuItem(strong("House Form"), tabName = "HF", icon = icon("home"), selected = TRUE),
      menuSubItem("Page 1", tabName = "HF_Page1"),
      menuSubItem("Page 2", tabName = "HF_Page2"),
      menuSubItem("Page 3", tabName = "HF_Page3"),
      menuItem(strong("Individual Form"), tabName = "IF", icon = icon("user")),
      menuSubItem("Page 1", tabName = "IF_Page1"),
      menuSubItem("Page 2", tabName = "IF_Page2"),
      menuItem(strong("Close application"), tabName = "Close", icon = icon("remove"))
      
    ) # sidebarMenu
    
  }) # renderMenu
  
  # Track the number of each person
  counter <- reactiveValues(n = 0)
  
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
    saveData(formData())
  })
  
  observeEvent(input$rm_btn, {
    if (counter$n > 0)
      counter$n <- counter$n - 1
  })
  
  # Print counter value
  output$counter <- renderPrint(print(counter$n))
  
  # render a number of topic ui elements based on the counter
  topics <- reactive({
    n <- counter$n
    if (n > 0)
      lapply(seq_len(n), topic_ui)
  })
  

  
  observeEvent(input$add_btn,{
    observe(
      if(is.null(input$fname_1) || input$fname_1 == ""){
        disable("add_btn")
      }
      else{
        enable("add_btn")
      }
    )
    
  })
  # Rendering the UI
  output$HF_Page1 <- renderUI(topics())
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Add button is clicked, save the form data
  observeEvent(input$add_btn, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$persons <- DT::renderDataTable({
    input$add_btn
    loadData()
  })
  
  # Render table of people recorded
  output$HF_Page2 <- renderUI(
    DT::dataTableOutput("persons", width = 300), tags$hr())
  
})

topic_ui <- function(i) {
  
  box(title = paste("Person", i), width = NULL, solidHeader = FALSE, status = "primary",
      column(width = 6,
             
             div(style = "display:inline-block", print(h3(i))),
             div(style = "display:inline-block", textInput("fname_1", "First name", value = "", width = '250px')),
             div(style = "display:inline-block", textInput("lname_1", "Last name", value = "", width = '250px')),
             div(style = "display:inline-block", selectInput("sex1", "Sex", choices = list("M" = "1", "F" = "2"),
                                                             selected = "", width = '55px')),
             div(style = "display:inline-block", textInput("birth_year1", "Birth year", value = "", width = '125px'))),
      
      column(width = 4,
             
             div(style = "display:inline-block", textInput("spouse1", "Spouse's line number", value = "", width = '150px')),
             div(style = "display:inline-block", textInput("mother1", "Mother's line number", value = "", width = '150px')),
             div(style = "display:inline-block", textInput("father1", "Father's line number", value = "", width = '150px'))),
      
      column(width = 2,
             
             checkboxInput("time1_1", label = "Half time", FALSE),
             
             bsTooltip("time1_1",
                       "Test Tooltip1"), placement = "bottom", trigger = "hover",
             
             checkboxInput("time1_2", label = "More than half time", FALSE),
             
             bsTooltip("time1_2",
                       "Test Tooltip2"), placement = "bottom", trigger = "hover")
      
  ) # box
  
}