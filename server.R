
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#
#Currently missing: 0) Convert the generic names to the real names 1) Convert the true/false string into a binary string & feed it into whattobreed 2) Add more UI elements - threshold for utility, 3) Replace text with pictures

load("ShinyBreeddata.Rdata")
DragonID<-DragonID[c(8:75),]
whattobreed<-function(usefullist,merger,dupeutility=0.0,assumebreedable=1){
  #need to have both DragonID and merger in the current location.  Will change eventually
  #WANT to have a way to value full eggs over fractional eggs but i don't even know how to get that data in the first place yet.
if(length(usefullist)!=68){return(0)}
  lDragonID<-DragonID
  lDragonID$owned<-usefullist
  if(assumebreedable==1){lDragonID$owned[lDragonID$owned==2]<-1}
  possmerger<-merger[lDragonID$owned[match(merger$FirstDragon,lDragonID$displayName)]==1,]
  possmerger<-possmerger[lDragonID$owned[match(possmerger$SecondDragon,lDragonID$displayName)]==1,]
  possmerger$FirstUseful<-1
  possmerger$FirstUseful[lDragonID$owned[match(possmerger$First,lDragonID$displayName)]>=1]<-0
  possmerger$SecondUseful<-1
  possmerger$SecondUseful[lDragonID$owned[match(possmerger$Second,lDragonID$displayName)]>=1]<-0
  possmerger$ThirdUseful<-1
  possmerger$ThirdUseful[lDragonID$owned[match(possmerger$Third,lDragonID$displayName)]>=1]<-0
  possmerger$FourthUseful<-1
  possmerger$FourthUseful[lDragonID$owned[match(possmerger$Fourth,lDragonID$displayName)]>=1]<-0
  possmerger$FifthUseful<-1
  possmerger$FifthUseful[lDragonID$owned[match(possmerger$Fifth,lDragonID$displayName)]>=1]<-0
  possmerger$SixthUseful<-1
  possmerger$SixthUseful[lDragonID$owned[match(possmerger$Sixth,lDragonID$displayName)]>=1]<-0
  lDragonID$fragments<-1
  lDragonID$fragments[lDragonID$displayName%in%c("Gog","Jura","Daemun","Garuda","Sahran","Klax","Kromon","Kobahl","Baldr")]<-5
  lDragonID$fragments[lDragonID$displayName%in%c("Grypp","Viscus","Vazir","Bolt","Viscus")]<-8
  lDragonID$fragments[lDragonID$displayName%in%c("Volos","Kinnara","Arborius")]<-12
  lDragonID$fragments[lDragonID$displayName%in%c("Kelsis","Drude")]<-16
  lDragonID$fragments[lDragonID$displayName%in%c("Ankor")]<-20
  lDragonID$fragments[lDragonID$displayName%in%c("Noss","Hydron")]<-48
  
  #second have to give a value for a 'dupe' egg for research purposes vs a new egg for breeding purposes
  possmerger$FirstFrags<-lDragonID$fragments[match(possmerger$First,lDragonID$displayName)]
  possmerger$SecondFrags<-lDragonID$fragments[match(possmerger$Second,lDragonID$displayName)]
  possmerger$ThirdFrags<-lDragonID$fragments[match(possmerger$Third,lDragonID$displayName)]
  possmerger$FourthFrags<-lDragonID$fragments[match(possmerger$Fourth,lDragonID$displayName)]
  possmerger$FifthFrags<-lDragonID$fragments[match(possmerger$Fifth,lDragonID$displayName)]
  possmerger$SixthFrags<-lDragonID$fragments[match(possmerger$Sixth,lDragonID$displayName)]
  possmerger$OverallUtility<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful+(dupeutility/possmerger$FirstFrags)),
                                           possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful+(dupeutility/possmerger$SecondFrags)),
                                           possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful+(dupeutility/possmerger$ThirdFrags)),
                                           possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful+(dupeutility/possmerger$FourthFrags)),
                                           possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful+(dupeutility/possmerger$FifthFrags)),
                                           possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful+(dupeutility/possmerger$SixthFrags))),na.rm=TRUE)
  #the above was doublecounting the utility of fragments that i don't already have.
  #possmerger$OverallUtility<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful),
  #                                         possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful),
  #                                         possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful),
  #                                         possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful),
  #                                         possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful),
  #                                         possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)),na.rm=TRUE)
  #possmerger$OverallUtility<-possmerger$OverallUtility + ((possmerger[,c(16,17,18,19,20)]==TRUE)*c((possmerger$FirstChance,possmerger$SecondChance,possmerger$ThirdChance,possmerger$FourthChance,possmerger$FifthChance,possmerger$SixthChance)/possmerger$totalchance*dupeutility))
  #  possmerger$OverallUtility<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*possmerger$FirstUseful,possmerger$SecondChance/possmerger$totalchance*possmerger$SecondUseful,possmerger$ThirdChance/possmerger$totalchance*possmerger$ThirdUseful,possmerger$FourthChance/possmerger$totalchance*possmerger$FourthUseful,possmerger$FifthChance/possmerger$totalchance*possmerger$FifthUseful,possmerger$SixthChance/possmerger$totalchance*possmerger$SixthUseful),na.rm=TRUE)
  return(possmerger[order(possmerger$OverallUtility,decreasing=TRUE)[1:10],])
}

concatlists<-function(files){
    redlist<-c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")
    purplelist<-c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Aliorn","Daemun","Garuda","Klax","Arborius","Dominus")
    bluelist<-c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")
    orangelist<-c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")
    greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis")
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
        "Red" = checkboxGroupInput("incomplete","AllReds",choices=c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")), #this doesn't work as intended

        "Purple" = checkboxGroupInput("incomplete","ListOfPurps",choices=c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Aliorn","Daemun","Garuda","Klax","Arborius","Dominus")),

        "Blue" = checkboxGroupInput("incomplete","ListOfBlues",choices=c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")),

        "Orange" = checkboxGroupInput("incomplete","ListOfOranges",choices=c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")),

        "Green" = checkboxGroupInput("incomplete","ListOfGreens",choices=c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis"))
)
    })

#    output$resulttable<-renderText({(concatlists(input))}) #for text-only output
    #  output$resulttable<-renderDataTable({whattobreed(usefullist=as.integer(concatlists(input)),merger=merger)})
  output$resulttable<-renderDataTable({
    DragonID
  })
# output$testimage<-renderImage({
#     # When input$n is 3, filename is ./images/image3.jpeg
#     filename <- normalizePath(file.path('./imagetest',
#                                         paste('testimage', input$n, '.jpeg', sep='')))
# 
#     # Return a list containing the filename and alt text
#     list(src = filename,
#          alt = paste("Image number", input$n))
# 
# }, deleteFile = FALSE)
#some example code to use when i want to include pre-rendered images in the output.
# remember that i really only want to use 2 images in the final output, so it's not important to do arbitrary numbers of images (or perhaps something like 2*numoutput)
#   output$resulttable<-renderDataTable(functiongoeshere)
}

  )


