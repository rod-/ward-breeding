library(shiny)

shinyUI(fluidPage(

  titlePanel("War Dragons Best Breeding Options"),
    helpText("This is where i can put some helpful text, i guess."),
  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
      column(3, wellPanel(uiOutput("ui"))),
fluidRow(    column(3, wellPanel(
    selectInput('input_types', 'Choose Partial Colors', choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Purple","Blue"), selectize=TRUE)))

),
helpText("This might be another good position for text"),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),
         helpText("Disclaimer")



)
)
)
#            selectInput("n",label = "Picture",c(1:5))))
#imageOutput("testimage"))),
