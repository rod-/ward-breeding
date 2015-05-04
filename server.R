
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#
#Currently missing: 0) Convert the generic names to the real names 1) Convert the true/false string into a binary string & feed it into whattobreed 2) Add more UI elements - threshold for utility, 3) Replace text with pictures


concatlists<-function(files){
    redlist<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","Rtest")
    purplelist<-c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","Ptest")
    bluelist<-c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","Btest")
    orangelist<-c("O1","O2","O3","O4","O5","O6","O7","O8","O9","O10","O11","Otest")
    greenlist<-c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","Gtest")
    listofeverything<-c(redlist,purplelist,bluelist,orangelist,greenlist)
    currentlist<-files$incomplete

        if("Red"%in%files$fullgroups){currentlist<-c(redlist,currentlist)}
        if("Purple"%in%files$fullgroups){currentlist<-c(purplelist,currentlist)}
        if("Blue"%in%files$fullgroups){currentlist<-c(bluelist,currentlist)}
        if("Orange"%in%files$fullgroups){currentlist<-c(orangelist,currentlist)}
        if("Green"%in%files$fullgroups){currentlist<-c(greenlist,currentlist)}

    return(listofeverything%in%currentlist) #reduces list down to a binary vector
}
library(shiny)

shinyServer(function(input, output) {
    output$ui<-renderUI({
        if(is.null(input$input_type))
            return()
        switch(input$input_type,
        "Red" = checkboxGroupInput("incomplete","AllReds",choices=c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")), #this doesn't work as intended

        "Purple" = checkboxGroupInput("incomplete","ListOfPurps",choices=c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")),

        "Blue" = checkboxGroupInput("incomplete","ListOfBlues",choices=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12")),

        "Orange" = checkboxGroupInput("incomplete","ListOfOranges",choices=c("O1","O2","O3","O4","O5","O6","O7","O8","O9","O10","O11","O12")),

        "Green" = checkboxGroupInput("incomplete","ListOfGreens",choices=c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12"))
)
    })

    output$resulttable<-renderText({(concatlists(input))}) #for text-only output

output$testimage<-renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./imagetest',
                                        paste('testimage', input$n, '.jpeg', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$n))

}, deleteFile = FALSE)
#some example code to use when i want to include pre-rendered images in the output.
# remember that i really only want to use 2 images in the final output, so it's not important to do arbitrary numbers of images (or perhaps something like 2*numoutput)
#   output$resulttable<-renderDataTable(functiongoeshere)
}

  )


