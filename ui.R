library(shiny)
load("ShinyBreeddata.Rdata")
shinyUI(fluidPage(
  titlePanel("[Unofficial] War Dragons Best Breeding Options"),
    helpText("Created by Rod-"),
  fluidRow(column(3,wellPanel(
      checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),selected = c("Red","Purple")))),

        fluidRow(    column(3, wellPanel(
            selectInput('input_types', 'Choose Partial Colors', choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Blue"), selectize=TRUE))),
            column(3, wellPanel(uiOutput("ui")))
            #            column(3,wellPanel(checkboxGroupInput('show_vars', 'Columns to show:', names(DragonID), selected = '')))
),
helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),
#fluidRow(column(12,wellPanel(dataTableOutput("dragstat")))), #if i want to also use dragstat for something.  It's probably a better idea to do graphs of it.
         helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")

),
tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
              <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
              <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
              <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal - The safer, easier way to pay online!\">
              <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
              </form>"))

)
)
#            selectInput("n",label = "Picture",c(1:5))))
#imageOutput("testimage"))),

