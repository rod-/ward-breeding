library(shiny)
library(DT)
load("ShinyBreeddata170.Rdata")
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

                        helpText("Help fund my War Dragons Base or keep this site hosted.  Donate below"),
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
                        fluidRow(column(3,wellPanel(selectInput('skipgreen','Which dragons do you want to ignore?',choices=c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin",
                                                                                                                             "Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo",
                                                                                                                             "Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus",
                                                                                                                             "Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius",
                                                                                                                             "Caladbolg","Firactus","Bander","Basileus","Chthoteuthis","Consurgens","Ferrox","Khrysos","Lumen","Sekoronos","Whalegnawer","Yersinu"),
                                                                selectize =TRUE,multiple=TRUE,selected=c("Karna","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin","Caladbolg","Firactus","Bander","Basileus","Chthoteuthis","Consurgens","Ferrox","Khrysos","Lumen","Sekoronos","Whalegnawer","Yersinu"))))),
                        fluidRow(column(12,wellPanel(DT::dataTableOutput("resbeta")))),
                        #                                                                                                    )),
                        HTML("<a href='http://www.amoebastudios.com/dragon/'> Confused?  Try out Amoeba's breeding site.</a>"),
                        helpText(""),
                        HTML("<a href='http://wardragons.pocketgems.com/member/25-rod'> Feedback+suggestions welcome.  Mail to rod- in game or on wardragons.pocketgems.com</a>"),


                        tags$div(HTML("<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
                                      <input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\">
                                      <input type=\"hidden\" name=\"hosted_button_id\" value=\"28NGSNA79HH4C\">
                                      <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" border=\"0\" name=\"submit\" alt=\"Donate (CAD) to our development and hosting\">
                                      <img alt=\"\" border=\"0\" src=\"https://www.paypalobjects.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\">
                                      </form>")),
                        helpText("Disclaimer:  Not associated with Pocket Gems. Images are property of Pocket Gems")
                        ),
                
            tabPanel("EventSpending",fluidRow(column(3,wellPanel(numericInput('eventlevel',label = "your level",value = 84,min = 1,max=245)))),
                     fluidRow(column(3,wellPanel(numericInput('clockvalue',label="Value of 1 hour speedup in rubies",value=14.58,min=0,max=14.58)))),
                     fluidRow(column(3,wellPanel(numericInput('tokenvalue',label="Value of 1 breeding token",value=0.8,min=0,max=10)))),
                     wellPanel(DT::dataTableOutput("eventoutput"))),
            
            tabPanel("Hunter ShotCounts",
            fluidRow(column(3,wellPanel(checkboxGroupInput("dragcolor","Which Colors are the dragons of interest",c("Red","Purple","Blue","Orange","Green","Gold"),selected = c("Gold")))),
                    (column(3,wellPanel(checkboxGroupInput("dragrarity","Rarity within Tier (1=lowest)",c(1,2,3),selected=c(3))))),
                    (column(3,wellPanel(checkboxGroupInput('oresearch', 'Offensiveresearches',c("Red4%","Red4%","Blue4%","Orange4%","Green4%","Green4%"),selected=c("Red4%","Red4%","Blue4%","Orange4%","Green4%"))))),
                    (column(3,wellPanel(checkboxGroupInput('dresearch','defensiveresearches',c(1.0,1.04,1.08,1.12,1.16,1.20,1.24,1.30,1.60),selected=c(1.20,1.30))))),
            fluidRow(column(12,wellPanel(sliderInput('dlevel','dragon levels',min=1,max=30,value=c(30,30))))),

           (plotOutput("resultplot"))
             )),

               tabPanel("Build Your Base",
                        helpText("Below are the expected number of speedups required to build your base to the target level selected at the bottom of the page"),

                        fluidRow(h5(htmlOutput("leveler1",inline=TRUE),img(src="img/12h.jpg",width="3%"),htmlOutput("leveler2",inline=TRUE),img(src="img/3h.jpg",width="3%"),htmlOutput("leveler3",inline=TRUE),img(src="img/1h.jpg",width="3%"),htmlOutput("leveler4",inline=TRUE),img(src="img/30m.jpg",width="3%"),
                                 htmlOutput("leveler5",inline=TRUE),img(src="img/15m.jpg",width="3%"),htmlOutput("leveler6",inline=TRUE),img(src="img/3m.jpg",width="3%"),htmlOutput("leveler7",inline=TRUE),img(src="img/1m.jpg",width="3%"),htmlOutput("leveler8",inline=TRUE),img(src="img/wood.jpg",width="3%"),htmlOutput("leveler9",inline=TRUE),img(src="img/builder.jpg",width="3%"),htmlOutput("leveler10",inline=TRUE),img(src="img/storage.jpg",width="3%"))),
                        h5(helpText("Your base at the target level should have towers at the following levels:"),htmlOutput("leveler11",inline=TRUE)),
                        helpText("Note:  VERY EARLY VERSION:  Don't trust these results too much!"),
                        helpText("Strategy descriptions:"),
                        helpText("     Highest:  Always Build the highest-level towers possible, prioritizing storage/builder upgrades when it increases max tower level."),
                        helpText("     Fastest: Build whatever tower gives the best exp/second (but don't level builder unless absolutely required - note that this doesn't
                                 check to see if builder NEEDS to be upgraded yet.  I recommend Highest.)"),
                        fluidRow(column(2,wellPanel(numericInput("ptarget","Input your target level",value = 84,max=199,min=2))),
                                 column(2, wellPanel(numericInput("bldrlevel", "Current Builder level",value=14,max=17,min=1))),
                                 column(2, wellPanel(numericInput("storlevel","Current Storage level", value=14,max=23,min=1))),
                                 column(2, wellPanel(numericInput("plevel","Current Level",value=50,max=145,min=1))),
                                 column(2, wellPanel(selectInput("strategy",label="Building Strategy",choices=c("highest","fastest"),selected="highest"))),
                                 column(2, wellPanel(checkboxGroupInput('buildresearch',label = "Build Speed Bonuses",choices=c("Red","Blue","Orange","Green","Event10","Event25"),selected = c("Red","Blue","Orange"))))),
                        hr(),
                        helpText("Input some or all of your towers' levels"),

                        fluidRow(
                            {column(2, wellPanel(
                                numericInput("tower1","Tower#1 level", value=1,min=1,max=25),
                                numericInput("tower4","",value=1,min=1,max=25),
                                numericInput("tower7","",value=1,min=1,max=25),
                                numericInput("tower10","",value=1,min=1,max=25),
                                numericInput("tower13","",value=1,min=1,max=25),
                                numericInput("tower16","",value=1,min=1,max=25),
                                numericInput("tower19","",value=1,min=1,max=25),
                                numericInput("tower22","",value=1,min=1,max=25),
                                numericInput("tower25","",value=1,min=1,max=25),
                                numericInput("tower28","",value=1,min=1,max=25),
                                numericInput("tower31","",value=1,min=1,max=25),
                                numericInput("tower34","",value=1,min=1,max=25)))},#one set of sliders
                            {column(2,wellPanel(
                                numericInput("tower2","Tower#2 level",value=1,min=1,max=25),
                                numericInput("tower5","",value=1,min=1,max=25),
                                numericInput("tower8","",value=1,min=1,max=25),
                                numericInput("tower11","",value=1,min=1,max=25),
                                numericInput("tower14","",value=1,min=1,max=25),
                                numericInput("tower17","",value=1,min=1,max=25),
                                numericInput("tower20","",value=1,min=1,max=25),
                                numericInput("tower23","",value=1,min=1,max=25),
                                numericInput("tower26","",value=1,min=1,max=25),
                                numericInput("tower29","",value=1,min=1,max=25),
                                numericInput("tower32","",value=1,min=1,max=25),
                                numericInput("tower35","",value=1,min=1,max=25)))},#second
                            {  column(2, wellPanel(numericInput("tower3","Tower#3 level", value=1,min=1,max=25),
                                                   numericInput("tower6","",value=1,min=1,max=25),
                                                   numericInput("tower9","",value=1,min=1,max=25),
                                                   numericInput("tower12","",value=1,min=1,max=25),
                                                   numericInput("tower15","",value=1,min=1,max=25),
                                                   numericInput("tower18","",value=1,min=1,max=25),
                                                   numericInput("tower21","",value=1,min=1,max=25),
                                                   numericInput("tower24","",value=1,min=1,max=25),
                                                   numericInput("tower27","",value=1,min=1,max=25),
                                                   numericInput("tower30","",value=1,min=1,max=25),
                                                   numericInput("tower33","",value=1,min=1,max=25),
                                                   numericInput("tower36","",value=1,min=1,max=25)))
                            },#third
                            column(6,wellPanel(DT::dataTableOutput('towerdata')))
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