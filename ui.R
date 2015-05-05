
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("War Dragons Best Breeding Options"),
fluidRow(
    column(3, wellPanel(
        checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
    column(3, wellPanel(
        selectInput("input_type","Choose a Partial Color",c("Red","Purple","Blue","Orange","Green")))),
#    column(3, wellPanel(
#            selectInput("n",label = "Picture",c(1:5)))),
    column(3, wellPanel(uiOutput("ui"))),
    #column(3, wellPanel(sliderInput(inputId = "numoutput",label = "How Many Results",min = 1,max=15,value = 5,round = TRUE))),
    #column(3, wellPanel(imageOutput("testimage"))),
    #column(3, tags$p("Best_Option:")),
    dataTableOutput("resulttable")
    )
  #want a switch between colors and the various names to pop up


)
)
  # Show a plot of the generated distribution



##I don't really like the clutteredness of this interface:  You can apparently do dynamic UIs so i could do an "allred" yes/no that would expand if no.

###highest_current_color dropdown?