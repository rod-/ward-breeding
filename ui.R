library(shiny)
library(DT)
load("ShinyBreeddata160.Rdata")
shinyUI(
  fluidPage(
    navbarPage("War Dragons Best Breeding Options",

               tabPanel("Best Pairing",
                        fluidRow(column(3,wellPanel(checkboxGroupInput("fullgroups","Check your completed colors",c("Red","Purple","Blue","Orange","Green"),
                                                                       selected = c("Red","Purple","Blue")),checkboxInput('empirical','Use empirical drop rates',FALSE))),
                                 fluidRow(    column(3, wellPanel(selectInput('input_types', 'Choose Partial Colors',
                                                                              choices=c("Purple","Blue","Red","Orange","Green","Gold"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),
                                              fluidRow(column(3, wellPanel(uiOutput("ui"))),column(3,wellPanel(selectInput('researchinterests','Choose interested research colors',choices=c("Purple","Blue","Red","Orange","Green","Gold"),multiple=TRUE,selected=c(""),selectize=TRUE)))))),
                        helpText("Choose the best pairings of dragons in the game War Dragons by Pocket Gems.  Input your dragons and see the best paths to progress below"),

                        fluidRow(column(12,wellPanel(DT::dataTableOutput("resulttable")))),

                        HTML("<a href='http://www.amoebastudios.com/dragon/'> Confused?  Try out Amoeba's breeding site.</a>"),
                        helpText(""),
                        HTML("<a href='http://wardragons.pocketgems.com/member/25-rod'> Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com</a>"),

                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems"),

                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate to our development and hosting\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>"))
                        )
               ,
               tabPanel("Target a Dragon",

                        fluidRow(    column(3, wellPanel(selectInput('input_typesBeta', 'What color is the dragon you want?',
                                                                     choices=c("Purple","Blue","Red","Orange","Green","Gold"), multiple=TRUE,selected = c("Orange"), selectize=TRUE))),

                                     fluidRow(column(3, wellPanel(uiOutput("uibeta"))))),
                        fluidRow(column(3,wellPanel(selectInput('skipgreen','Which dragons do you want to ignore?',choices=c("Caladbolg","Firactus","Bander","Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin",
                                                                                                                             "Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo",
                                                                                                                             "Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus",
                                                                                                                             "Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius"),
                                                                selectize =TRUE,multiple=TRUE,selected=c("Caladbolg","Firactus","Bander","Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin"))))),
                        fluidRow(column(12,wellPanel(DT::dataTableOutput("resbeta")))),
                        #                                                                                                    )),
                        HTML("<a href='http://www.amoebastudios.com/dragon/'> Confused?  Try out Amoeba's breeding site.</a>"),
                        helpText(""),
                        HTML("<a href='http://wardragons.pocketgems.com/member/25-rod'> Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com</a>"),


                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"2U6KK7ZRCUZV2\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate to our development and hosting\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>")),
                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")
                        ),
               tabPanel("Build Your Base",
                        fluidRow(htmlOutput("leveler1",inline=TRUE),img(src="img/12h.jpg"),htmlOutput("leveler2",inline=TRUE),img(src="img/3h.jpg"),htmlOutput("leveler3",inline=TRUE),img(src="img/1h.jpg"),htmlOutput("leveler4",inline=TRUE),img(src="img/30m.jpg"),
                                 htmlOutput("leveler5",inline=TRUE),img(src="img/15m.jpg"),htmlOutput("leveler6",inline=TRUE),img(src="img/3m.jpg"),htmlOutput("leveler7",inline=TRUE),img(src="img/1m.jpg"),htmlOutput("leveler8",inline=TRUE),img(src="img/wood.jpg"),htmlOutput("leveler9",inline=TRUE),img(src="img/builder.jpg"),htmlOutput("leveler10",inline=TRUE),img(src="img/storage.jpg")),
                        helpText("Your final base should be at the following levels:"),htmlOutput("leveler11",inline=TRUE),
                        helpText("Note:  VERY EARLY VERSION:  Don't trust these results too much!"),
                        helpText("Above are the expected number of speedups required to build your base to the target level selected below"),
                        helpText("Strategy descriptions:"),
                        helpText("     Highest:  Always Build the highest-level towers possible, prioritizing storage/builder upgrades when it increases max tower level."),
                        helpText("     Fastest: Build whatever tower gives the best exp/second (but don't level builder unless absolutely required - note that this doesn't
                                 check to see if builder NEEDS to be upgraded yet.  I recommend Highest.)"),
                        fluidRow(column(2,wellPanel(sliderInput("ptarget","Input your target level",value = 84,max=199,min=2,ticks=FALSE))),
                                 column(2, wellPanel(sliderInput("bldrlevel", "Current Builder level",value=14,max=17,min=1,ticks=FALSE))),
                                 column(2, wellPanel(sliderInput("storlevel","Current Storage level", value=14,max=23,min=1,ticks=FALSE))),
                                 column(2, wellPanel(sliderInput("plevel","Current Level",value=50,max=145,min=1,ticks=FALSE))),
                                 column(2, wellPanel(selectInput("strategy",label="Building Strategy",choices=c("highest","fastest"),selected="highest"))),
                                 column(2, wellPanel(checkboxGroupInput('buildresearch',label = "Build Speed Bonuses",choices=c("Red","Blue","Orange","Green","Event10","Event25"),selected = c("Red","Blue","Orange"))))),
                        hr(),
                        helpText("Slide the slider to your top towers' levels"),

                        fluidRow(
                            {column(3, wellPanel(
                                sliderInput("tower1","Tower#1 level", value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower4","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower7","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower10","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower13","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower16","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower19","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower22","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower25","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower28","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower31","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower34","",value=1,min=1,max=25,ticks = FALSE)))},#one set of sliders
                            {column(3,wellPanel(
                                sliderInput("tower2","Tower#2 level",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower5","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower8","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower11","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower14","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower17","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower20","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower23","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower26","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower29","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower32","",value=1,min=1,max=25,ticks = FALSE),
                                sliderInput("tower35","",value=1,min=1,max=25,ticks = FALSE)))},#second
                            {  column(3, wellPanel(sliderInput("tower3","Tower#3 level", value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower6","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower9","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower12","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower15","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower18","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower21","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower24","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower27","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower30","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower33","",value=1,min=1,max=25,ticks = FALSE),
                                                   sliderInput("tower36","",value=1,min=1,max=25,ticks = FALSE)))
                            },#third
                            column(3,wellPanel(DT::dataTableOutput('towerdata')))
                        )
               ))
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