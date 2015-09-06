library(shiny)
load("ShinyBreeddata160.Rdata")
shinyUI(
  fluidPage(
    navbarPage("WarDragons Best Breeding Options",
               tabPanel("Main",
                        fluidRow(column(3,wellPanel(checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),
                                                                       selected = c("Red","Purple","Blue")),checkboxInput('empirical','Use empirical drop rates',FALSE))),
                                 fluidRow(    column(3, wellPanel(selectInput('input_types', 'Choose Partial Colors',
                                                                              choices=c("Purple","Blue","Red","Orange","Green","Gold"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),
                                              fluidRow(column(3, wellPanel(uiOutput("ui")))))),
                        helpText("The best pairing with the currently-selected set of dragons is the top line in the table below"),
#                        helpText("Note that gold dragons are currently misnamed:  Bander = Bander, Zephyr=Caladbolg, Zerka = Firactus"),
                        fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),

                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems"),
                        helpText("Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com"),
                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate to our development and hosting\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>"))
                        ),
                tabPanel("Beta",

                           fluidRow(    column(3, wellPanel(selectInput('input_typesBeta', 'What color is the dragon you want?',
                                                                        choices=c("Purple","Blue","Red","Orange","Green","Gold"), multiple=TRUE,selected = c("Green"), selectize=TRUE))),

                             fluidRow(column(3, wellPanel(uiOutput("uibeta"))))),
                           fluidRow(column(3,wellPanel(selectInput('skipgreen','Which dragons do you want to ignore?',choices=c("Caladbolg","Firactus","Bander","Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin",
                                                                                                                                "Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo",
                                                                                                                                "Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus",
                                                                                                                                "Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius"),
                                                                                                                                selectize =TRUE,multiple=TRUE,selected=c("Caladbolg","Firactus","Bander","Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin"))))),
                           fluidRow(column(12,wellPanel(dataTableOutput("resbeta")))),
            #                                                                                                    )),
                                           helpText("Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com"),

                                           tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                              <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                              <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                              <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate to our development and hosting\">
                              <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                              </form>")),
                                                             helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")
                                         ),
               tabPanel("Introduction",
                        fluidRow(
                          column(6,
                                 includeHTML("README.html")
                          )
                        )
               )
               )
  ))

###example callback function to change text color
#"fnRowCallback": function( nRow, aData, iDisplayIndex, iDisplayIndexFull ) {
#    /* numbers less than or equal to 0 should be in red text */
#        if ( parseFloat(aData[4]) <= 0 ) {
#            jQuery('td:eq(4)', nRow).addClass('redText');
#        }
#    return nRow;
#},
#now my problem is that i want to write and apply a function (isgreen)
#check out https://github.com/trestletech/shinyTable