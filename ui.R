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
#new 5/8/15 update AO data:
#level<-c(29,26,29,28,24,20,28,33,21,23,27,29,18,19,24,23,17,20,19,18,21,20,15,20,17,22,17,23,17,20,21,19,25,15,20,19,18,17,14,17,32,15,14,19,17,17,17,13,13,13)
#name=c(rodminus bigkeeper fatboo zyphon austre shirokin chungi82 loxx riccette tellerin amarok nevyn vandor ragar2015 poipoi juanmaster ellusias mayahem6969 avatarroku firest0rm mordeth vanation jambag unlimitless dracaryx sucho pinefrost pull sladesalvatore jimmy12 brewdoctor ayleth rogermartin weakeneddrake relax tontojohn shaun99 liz314 dracozen whitewalker pmoney athenakitty stargon yubbaman grggge ajb39 feet azabech enkrypton soosan)
#flame=c(587 812 650 591 501 477 455 849 145 540 97 129 297 239 285 310 227 143 263 155 370 642 201 346 169 190 300 200 217 111 268 88 117 157 220 354 57 128 112 59 147 106 31 156 63 62 57 5 34 33)
#medal=c(15000 13000 11000 7765 7500 3552 14000 13000 9995 6920 6748 5499 5421 4756 4258 4164 3674 3587 3543 3265 3254 3221 3041 2949 2928 2922 2755 2642 2554 2543 2489 2452 2391 2300 2284 2001 1798 1748 1727 1650 1642 1626 1557 1444 1176 1035 900 675 600 432)