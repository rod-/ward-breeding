
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#
#Currently missing: 0) Convert the generic names to the real names 1) Convert the true/false string into a binary string & feed it into whattobreed 2) Add more UI elements - threshold for utility, 3) Replace text with pictures

whattobreed<-function(usefullist,dupeutility=0.0,assumebreedable=1){
  #need to have both DragonID and merger in the current location.  Will change eventually
  load("ShinyBreeddata.Rdata") #for whatever reason, this doesn't seem to be working out ; i'll just manually...
  merger<-read.csv("merger.csv")
  DragonID<-data.frame(identifier=c("T1C1WFdragon",       "T1C1SEdragon", "T1C1HIdragon",          "T1C2WIdragon",          "T1C2HEdragon",          "T1C2SFdragon",          "T1C3WEdragon",          "T1C3HFdragon",          "T1C3SIdragon",          "T1GGWFdragon",          "T2C1HIdragon",         
                                    "T2C1WEdragon",          "T2C1SFdragon",          "T2C1HUdragon",          "T2C2WIdragon",          "T2C2SUdragon",          "T2C2HEdragon",          "T2C3WUdragon",          "T2C3SEdragon",          "T2C3HFdragon",         
                                    "T2C3SFdragon",          "T2C3HIdragon",          "T2C3WEdragon",          "T2GGSIdragon",          "T3C1WEdragon",          "T3C1SFdragon",          "T3C1HIdragon",          "T3C1WUdragon",          "T3C1SIdragon",         
                                    "T3C1HEdragon",          "T3C2SEdragon",          "T3C2HUdragon",          "T3C2WFdragon",          "T3C3SUdragon",          "T3C3HFdragon",          "T3C3WIdragon",          "T3C3HIdragon",          "T3GGHEdragon",         
                                    "T4C1SFdragon",          "T4C1HIdragon",          "T4C1WUdragon",          "T4C1SDdragon",          "T4C1HEdragon",          "T4C1HFdragon",          "T4C2WIdragon",          "T4C2SEdragon",          "T4C2HUdragon",         
                                    "T4C2WDdragon",          "T4C3SIdragon",          "T4C3WFdragon",          "T4C3HDdragon",          "T4C3SUdragon",          "T4C3WEdragon",          "T4GGSDdragon",          "T5C1WUdragon",          "T5C1HEdragon",         
                                    "T5C1WDdragon",          "T5C1SFdragon",          "T5C1HIdragon",          "T5C2SEdragon",          "T5C2HDdragon",          "T5C2WIdragon",          "T5C2SUdragon",          "T5C3HFdragon",          "T5C3WFdragon",         
                                    "T5C3SDdragon",          "T5C3HUdragon",          "T5GGWUdragon"),
                       displayName=c( "Draco",          "Leviathan",      "Frigg",          "Zin",            "Hext",          
                                      "Aetrix",         "Hantu",          "Kastor",         "Kinnara",        "Fenrir",         "Trollis",        "Laekrian",       "Merk",           "Dactyl",         "Gog",            "Huli",           "Borg",          
                                      "Vladimir",       "Alikorn",        "Daemun",         "Garuda",         "Klax",           "Arborius",       "Dominus",        "Grypp",          "Jura",           "Kromon",         "Yanari",         "Vazir",         
                                      "Drude",          "Sahran",         "Bolt",          "Kelsis",         "Etzel",          "Kobahl",         "Baldr",          "Viscus",         "Numen",          "Ankor",          "Noss",           "Hydron",        
                                      "Slynx",          "Habrok",         "Volos",          "Amarok",         "Luminark",       "Lucius",         "Bronze",         "Septys",         "Ruma",           "Enki",           "Durga",          "Kolo",         
                                      "Darja",          "Gaspar",         "Karna",          "Naga",           "Nassus",         "Garzev",         "Serabis",        "Urd",            "Ith",            "Elixis",         "Pandi",          "Danzig",        
                                      "Nix",            "Ettin",          "Carsis"       ))
  #WANT to have a way to value full eggs over fractional eggs but i don't even know how to get that data in the first place yet.
if(length(usefullist)!=68){return(0)}
  lDragonID<-DragonID
  lDragonID$owned<-usefullist
  if(assumebreedable==1){lDragonID$owned[lDragonID$owned==2]<-1}
  possmerger<-merger[lDragonID$owned[match(merger$FirstDragon,lDragonID$displayName)]==1,]#do i own the first dragon
  possmerger<-possmerger[lDragonID$owned[match(possmerger$SecondDragon,lDragonID$displayName)]==1,] #do i own the second
  if(length(possmerger$FirstDragon)<=1){return(data.frame(NULL))}
  possmerger$FirstUseful<-1 #do i want the outputs? (assume yes)
  possmerger$FirstUseful[lDragonID$owned[match(possmerger$First,lDragonID$displayName)]>=1]<-0 #if i already own it, i don't!
  possmerger$SecondUseful<-1 #repeat
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
  return(as.data.frame(possmerger[order(possmerger$OverallUtility,decreasing=TRUE)[1:4],c(1,2,3,5,7,9,11,13,28)]))
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

 #   output$resulttable<-renderText({(as.integer(concatlists(input)))}) #for text-only output
#    output$resulttable<-renderText({(DragonID$displayName)}) #for text-only output
#output$resulttable<-renderPrint({whattobreed(usefullist=as.integer(concatlists(input)))})
    output$resulttable<-renderDataTable({whattobreed(usefullist=as.integer(concatlists(input)))})
  
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


