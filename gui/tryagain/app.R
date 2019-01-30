setwd("/home/oleg/Downloads/final/gui/shiny.R")
# app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(    uiOutput("model_type")     ),
    fluidRow(    textInput("userinput", "user text", "I eat")    ), 
    fluidRow(    tableOutput("predictions")    )
  ) #end dashboardbody
)


server <- function(input, output) {
  models <- load.models()
  
  #  print(names(models))
  output$predictions <- renderTable({
    print("predict.word("%&% input$userinput %&% ")")
    predict.word(models[["model."%&%input$model_type2]], input$userinput)
    #data.frame(x=input$model_type)
  })
  output$model_type <- renderUI({selectInput("model_type2", "Model", 
                                             gsub("model\\.", "", names(models)), 
                                             "backoff",F)})
}
shinyApp(ui, server)