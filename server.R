
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#
functiongoeshere<-function(files){as.data.table(cbind(files$redvec,files$purvec,files$bluevec,files$orangevec,files$greenvec))[1:5,]}
dothefunction<-function(files){return(c(files$fullgroups,files$incomplete))}
library(shiny)

shinyServer(function(input, output) {
    output$ui<-renderUI({
        if(is.null(input$input_type))
            return()
        switch(input$input_type,
        "Red" = checkboxGroupInput("incomplete","AllReds",choices=c("R1","R2","R3")), #this doesn't work as intended

        "Purple" = checkboxGroupInput("incomplete","ListOfPurps",choices=c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")),

        "Blue" = checkboxGroupInput("incomplete","ListOfBlues",choices=c("All","B1","B","C","D")),

        "Orange" = checkboxGroupInput("incomplete","ListOfOranges",choices=c("All","O1","B","C","D")),

        "Green" = checkboxGroupInput("incomplete","ListOfGreens",choices=c("All","G1","B","C","D"))
)
    })

    output$resulttable<-renderText({(dothefunction(input))}) #for text-only output
 #   output$resulttable<-renderDataTable(functiongoeshere)
}

  )


