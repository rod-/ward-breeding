library(shiny)
load("ShinyBreeddata.Rdata")
shinyUI(fluidPage(

  titlePanel("[Unofficial] War Dragons Best Breeding Options"),
    helpText("Created by Rod-"),
  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green")))),
      column(3, wellPanel(uiOutput("ui"))),
        fluidRow(    column(3, wellPanel(
            selectInput('input_types', 'Choose Partial Colors', choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Purple","Blue"), selectize=TRUE))),
            column(3,wellPanel(checkboxGroupInput('show_vars', 'Columns to show:', names(DragonID), selected = '')))
),
helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),
fluidRow(column(12,wellPanel(dataTableOutput("dragstat")))),
         helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")



)
)
)
#            selectInput("n",label = "Picture",c(1:5))))
#imageOutput("testimage"))),
#deadweight currently = c(mrad23,shaun99,weakeneddrake,tontojohn,pull,soosan,keyhuang,yubbaman)
