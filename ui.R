
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("War Dragons Best Breeding Options"),

  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
      column(3, wellPanel(uiOutput("ui"))),
      column(3, wellPanel(uiOutput("ui2"))),
fluidRow(    column(3, wellPanel(
    selectInput("input_type","Choose a Partial Color",c("Blue","Red","Purple","Orange","Green")))),
    column(3,wellPanel(
        selectInput("input_type2","Second Partial Color?",c("Purple","Blue","Orange","Green","Red"))))
),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))
)
  #want a switch between colors and the various names to pop up


)
)
)
#    column(3, wellPanel(
#            selectInput("n",label = "Picture",c(1:5)))),
#column(3, wellPanel(sliderInput(inputId = "numoutput",label = "How Many Results",min = 1,max=15,value = 5,round = TRUE))),
#column(3, wellPanel(imageOutput("testimage"))),
#column(3, tags$p("Best_Option:")),