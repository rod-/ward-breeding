library(shiny)
load("ShinyBreeddata.Rdata")
shinyUI(
  fluidPage(
    navbarPage("WarDragons Best Breeding Options",
               tabPanel("Main",
                        fluidRow(column(3,wellPanel(checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),
                                                                       selected = c("Red","Purple","Blue")))),
                                 fluidRow(    column(3, wellPanel(selectInput('input_types', 'Choose Partial Colors',
                                                                              choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),
                                              fluidRow(column(3, wellPanel(uiOutput("ui")))))),
                        helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
                        fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),
                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems"),
                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal - The safer, easier way to pay online!\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>"))
                        ),
                tabPanel("Beta",
                         fluidRow(column(3,wellPanel(
                           checkboxGroupInput("fullgroupsBeta","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),selected = c("Red","Purple","Blue")))),
                           fluidRow(    column(3, wellPanel(selectInput('input_typesBeta', 'Choose Partial Colors', 
                                                                        choices=c("Purple","Blue","Red","Orange","Green"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),
                             fluidRow(column(3, wellPanel(uiOutput("uibeta"))))),
                           fluidRow(column(3,wellPanel(checkboxInput('skipgreen','Ignore Green Dragons?',value=TRUE)))),
                           fluidRow(column(12,wellPanel(dataTableOutput("resbeta")))))),
               #                            fluidRow(column(2, wellPanel( sliderInput("rval", "RedResearchValue:", 
               #                                                                         min = 0, max = 1, value = 0.1, step= 0.1))),
               #                                            column(2,wellPanel( sliderInput("pval", "PurpResearchValue:" ,
               #                                                                               min = 0, max = 1, value = 0.1, step= 0.1))),
               #                                               column(2,wellPanel( sliderInput("bval", "BlueResearchValue:", 
               #                                                                               min = 0, max = 1, value = 0.1, step= 0.1))),
               #                                               column(2,wellPanel( sliderInput("oval", "OrangeResearchValue:", 
               #                                                                               min = 0, max = 1, value = 0.1, step= 0.1))),
               #                                               column(2,wellPanel( sliderInput("gval", "GreenResearchValue:", 
               #                                                                               min = 0, max = 1, value = 0.1, step= 0.1))
               #                                                                                                    )),
               #                            helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
               #                            fluidRow(column(12,wellPanel(dataTableOutput("resulttable"))))),
               #                            tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
               #               <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
               #               <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
               #               <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal - The safer, easier way to pay online!\">
               #               <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
               #               </form>")),
               #                                              helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")
               #                          ),
               tabPanel("Introduction",
                        fluidRow(
                          column(6,
                                 includeHTML("README.html")
                          )
                        )
               )
               )
  ))