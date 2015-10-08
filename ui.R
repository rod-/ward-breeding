library(shiny)
load("ShinyBreeddata160.Rdata")
shinyUI(
  fluidPage(
    navbarPage("War Dragons Best Breeding Options",

               tabPanel("Best Pairing",
                        fluidRow(column(3,wellPanel(checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),
                                                                       selected = c("Red","Purple","Blue")),checkboxInput('empirical','Use empirical drop rates',FALSE))),
                                 fluidRow(    column(3, wellPanel(selectInput('input_types', 'Choose Partial Colors',
                                                                              choices=c("Purple","Blue","Red","Orange","Green","Gold"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),
                                              fluidRow(column(3, wellPanel(uiOutput("ui")))))),
                        helpText("Choose the best pairings of dragons in the game War Dragons by Pocket Gems.  Input your dragons and see the best paths to progress below"),

                        fluidRow(column(12,wellPanel(dataTableOutput("resulttable")))),

                        helpText("Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com"),
                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems"),

                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate to our development and hosting\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>")),
                        tags$meta(HTML("name=\"description\" content=\"War Dragons (by pocket gems) breeding. Search these tables for the best options\""))
                        ),
               tabPanel("How to use",
                        fluidRow(
                            column(6,
                                   includeHTML("README.html")
                            )
                        )
               ),
                tabPanel("Target a Dragon",

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
               tabPanel("Build Your Base",
                        fluidRow(column(3,wellPanel(numericInput("ptarget","Input your target level",value = 84,max=199),
                                checkboxGroupInput('buildresearch',label = "",choices=c("Red","Blue","Orange","Green"),selected = "Red"))),
                                 fluidRow(column(3, wellPanel(numericInput("bldrlevel", "Your builder level",value=14,max=19))),
                                          column(3, wellPanel(numericInput("storlevel","Your storage level", value=14,max=24))),
                                          column(3, wellPanel(sliderInput("tower1","Tower#1 level", value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower2","#2",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower3","#3",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower4","#4",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower5","#5",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower6","#6",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower7","#7",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower8","",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower9","",value=16,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower10","",value=16,min=1,max=25,ticks = FALSE)
                                                              ))),
                                              fluidRow(column(3, wellPanel(uiOutput("leveler"))))),
                helpText("UnderConstruction")
               )
  ))
)
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