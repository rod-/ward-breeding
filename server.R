
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#
functiongoeshere<-function(fnin=c(redvec,purvec,bluevec,orangevec,greenvec)){dothefunction()}
dothefunction<-function(files){return(c(files$redvec,files$purvec,files$bluevec,files$orangevec,files$greenvec))}
library(shiny)

shinyServer(function(input, output) {
    output$ui<-renderUI({
        if(is.null(input$input_type))
            return()
        switch(input$input_type,
        "Red" = checkboxGroupInput("redvec","ListOfReds",choices=c("All","A","B","C","D")),
        "Purple" = checkboxGroupInput("purvec","ListOfPurps",choices=c("All","P1","B","C","D")),
        "Blue" = checkboxGroupInput("bluevec","ListOfBlues",choices=c("All","B1","B","C","D")),
        "Orange" = checkboxGroupInput("orangevec","ListOfOranges",choices=c("All","O1","B","C","D")),
        "Green" = checkboxGroupInput("greenvec","ListOfGreens",choices=c("All","G1","B","C","D"))
)
    })


    output$resulttable<-renderText({(dothefunction(input))})

}

  )


