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
                                      </form>"))
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
                        fluidRow(column(3, wellPanel(uiOutput("leveler")))),
            hr(),
            tags$img(HTML("<img src=\"/1h.jpg\" alt=\"1hour\" width=\"60\" height=\"70\" />")),
            helpText("Strategy descriptions:"),helpText("     Highest:  Always Build the highest-level towers possible, prioritizing storage/builder upgrades when it increases max tower level."),
                     helpText("     Fastest: Build whatever tower gives the best exp/second (but don't level builder unless absolutely required)"),
                        fluidRow(column(2,wellPanel(numericInput("ptarget","Input your target level",value = 84,max=199))),
                                 column(2, wellPanel(numericInput("bldrlevel", "Builder level",value=14,max=19))),
                                 column(2, wellPanel(numericInput("storlevel","Storage level", value=14,max=24))),
                                 column(2, wellPanel(numericInput("plevel","Current Level",value=50,max=199))),
                                 column(2, wellPanel(selectInput("strategy",label="Building Strategy",choices=c("Highest","Fastest"),selected="Highest"))),
                        column(2, wellPanel(checkboxGroupInput('buildresearch',label = "Build Speed Bonuses",choices=c("Red","Blue","Orange","Green","Event10","Event20"),selected = c("Red","Blue","Orange"))))),
                                          hr(),
                                    helpText("Input your top towers' levels"),

            fluidRow(column(3, wellPanel(sliderInput("tower1","Tower#1 level", value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower4","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower7","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower10","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower13","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower16","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower19","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower22","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower25","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower28","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower31","",value=11,min=1,max=25,ticks = FALSE),
                                         sliderInput("tower34","",value=11,min=1,max=25,ticks = FALSE))),
                                          column(3,wellPanel(
                                          sliderInput("tower2","Tower#2 level",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower5","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower8","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower11","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower14","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower17","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower20","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower23","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower26","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower29","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower32","",value=11,min=1,max=25,ticks = FALSE),
                                          sliderInput("tower35","",value=11,min=1,max=25,ticks = FALSE))),
                                          column(3, wellPanel(sliderInput("tower3","Tower#3 level", value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower6","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower9","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower12","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower15","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower18","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower21","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower24","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower27","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower30","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower33","",value=11,min=1,max=25,ticks = FALSE),
                                                              sliderInput("tower36","",value=11,min=1,max=25,ticks = FALSE)))
                                          ))
               )
  )
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