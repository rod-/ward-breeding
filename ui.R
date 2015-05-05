library(shiny)

shinyUI(fluidPage(

  titlePanel("War Dragons Best Breeding Options"),

  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
      column(3, wellPanel(uiOutput("ui"))),
      column(3, wellPanel(uiOutput("ui2"))),
fluidRow(    column(3, wellPanel(
    selectInput("input_type","Choose a Partial Color",c("Purple","Blue","Red","Orange","Green")))),
    column(3,wellPanel(
        selectInput("input_type2","Second Partial Color?",c("Blue","Purple","Orange","Green","Red"))))
),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))
)


)
)
)
#            selectInput("n",label = "Picture",c(1:5))))
#imageOutput("testimage"))),
