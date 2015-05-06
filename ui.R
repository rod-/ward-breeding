library(shiny)

shinyUI(fluidPage(

  titlePanel("[Unofficial] War Dragons Best Breeding Options"),
    helpText("Created by Rod-"),
  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
      column(3, wellPanel(uiOutput("ui"))),
fluidRow(    column(3, wellPanel(
    selectInput('input_types', 'Choose Partial Colors', choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Purple","Blue"), selectize=TRUE)))

),
helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),
         helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")



)
)
)
#            selectInput("n",label = "Picture",c(1:5))))
#imageOutput("testimage"))),
