#Currently missing:
# Add more UI elements - threshold for utility,
# Replace text with pictures
# Clearer Column names
# Slider for value of a duplicate
# Fragment Data for num of fragments to make dupeutility practical/useful.
# Color information (light cell background corresponding to the dragon color?)
whattobreed<-function(usefullist,dupeutility=0.0,assumebreedable=1){
  load("ShinyBreeddata.Rdata")

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
# Wouldn't mind just cleaning the actual DragonID table up rather than taking the manual one, but whatever.
# WANT to have a way to value full eggs over fractional eggs but i don't even know how to get that data in the first place yet.
if(length(usefullist)!=68){return(0)}
  DragonID$owned<-usefullist
  if(assumebreedable==1){DragonID$owned[DragonID$owned==2]<-1}
  possmerger<-merger[DragonID$owned[match(merger$FirstDragon,DragonID$displayName)]==1,]#do i own the first dragon
  possmerger<-possmerger[DragonID$owned[match(possmerger$SecondDragon,DragonID$displayName)]==1,] #do i own the second
  if(length(possmerger$FirstDragon)<=1){return(data.frame(NULL))}
  possmerger$FirstUseful<-1 #do i want the outputs? (assume yes)
  possmerger$FirstUseful[DragonID$owned[match(possmerger$First,DragonID$displayName)]>=1]<-0 #if i already own it, i don't!
  possmerger$SecondUseful<-1 #repeat
  possmerger$SecondUseful[DragonID$owned[match(possmerger$Second,DragonID$displayName)]>=1]<-0
  possmerger$ThirdUseful<-1
  possmerger$ThirdUseful[DragonID$owned[match(possmerger$Third,DragonID$displayName)]>=1]<-0
  possmerger$FourthUseful<-1
  possmerger$FourthUseful[DragonID$owned[match(possmerger$Fourth,DragonID$displayName)]>=1]<-0
  possmerger$FifthUseful<-1
  possmerger$FifthUseful[DragonID$owned[match(possmerger$Fifth,DragonID$displayName)]>=1]<-0
  possmerger$SixthUseful<-1
  possmerger$SixthUseful[DragonID$owned[match(possmerger$Sixth,DragonID$displayName)]>=1]<-0
  possmerger$ChanceofNewEgg<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful)+0,
                                           possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful)+0,
                                           possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful)+0,
                                           possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful)+0,
                                           possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful)+0,
                                           possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)+0),na.rm=TRUE)



#   DragonID$fragments<-1
#   DragonID$fragments[DragonID$displayName%in%c("Gog","Jura","Daemun","Garuda","Sahran","Klax","Kromon","Kobahl","Baldr")]<-5
#   DragonID$fragments[DragonID$displayName%in%c("Grypp","Viscus","Vazir","Bolt","Viscus")]<-8
#   DragonID$fragments[DragonID$displayName%in%c("Volos","Kinnara","Arborius")]<-12
#   DragonID$fragments[DragonID$displayName%in%c("Kelsis","Drude")]<-16
#   DragonID$fragments[DragonID$displayName%in%c("Ankor")]<-20
#   DragonID$fragments[DragonID$displayName%in%c("Noss","Hydron")]<-48
####The above data regarding fragments is very incomplete and in need of assistance before i allow the dupeutility measurements to go online###
#   #second have to give a value for a 'dupe' egg for research purposes vs a new egg for breeding purposes
#   possmerger$FirstFrags<-DragonID$fragments[match(possmerger$Egg1,DragonID$displayName)]
#   possmerger$SecondFrags<-DragonID$fragments[match(possmerger$Second,DragonID$displayName)]
#   possmerger$ThirdFrags<-DragonID$fragments[match(possmerger$Third,DragonID$displayName)]
#   possmerger$FourthFrags<-DragonID$fragments[match(possmerger$Fourth,DragonID$displayName)]
#   possmerger$FifthFrags<-DragonID$fragments[match(possmerger$Fifth,DragonID$displayName)]
#   possmerger$SixthFrags<-DragonID$fragments[match(possmerger$Sixth,DragonID$displayName)]
  #possmerger$OverallUtility<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful),
  #                                         possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful),
  #                                         possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful),
  #                                         possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful),
  #                                         possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful),
  #                                         possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)),na.rm=TRUE) #first collect the odds of a new egg. (Useful being binary)
  # valuetoadd<-0
  # valuetoadd<-valuetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*dupeutility)/(possmerger$totalchance*possmerger$FirstFrags)
#valuetoadd<-valuetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*dupeutility)/(possmerger$totalchance*possmerger$SecondFrags)
#valuetoadd<-valuetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*dupeutility)/(possmerger$totalchance*possmerger$ThirdFrags)
#valuetoadd<-valuetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*dupeutility)/(possmerger$totalchance*possmerger$FourthFrags)
#valuetoadd<-valuetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*dupeutility)/(possmerger$totalchance*possmerger$FifthFrags)
#valuetoadd<-valuetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*dupeutility)/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
#possmerger$OverallUtility<-possmerger$OverallUtility+valuetoadd #add the odds of new egg to the odds of a 'research-worthy' egg.

#  return(as.data.frame(possmerger[order(possmerger$ChanceofNewEgg,decreasing=TRUE),c(1,2,3,5,7,9,11,13,28)])) #28 is if i do include fragment data.
return(as.data.frame(possmerger[order(possmerger$ChanceofNewEgg,decreasing=TRUE),c(1,2,3,5,7,9,11,13,22)])) #22 is if i don't include fragment data
}
load("ShinyBreeddata.Rdata")
DragonStatDF<-DragonID
concatlists<-function(files){
    redlist<-c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")
    purplelist<-c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius","Dominus")
    bluelist<-c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")
    orangelist<-c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")
    greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis")
    listofeverything<-c(redlist,purplelist,bluelist,orangelist,greenlist)
    currentlist<-c(files$incomplete,files$incompleteB)

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
        if(is.null(input$input_types))
            {return()}
        incompletelist<-NULL
        if("Red"%in%input$input_types){incompletelist<-c(incompletelist,"Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")}
        if("Purple"%in%input$input_types){incompletelist<-c(incompletelist,"Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius","Dominus")}
        if("Blue"%in%input$input_types){incompletelist<-c(incompletelist,"Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")}
        if("Orange"%in%input$input_types){incompletelist<-c(incompletelist,"Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")}
        if("Green"%in%input$input_types){incompletelist<-c(incompletelist,"Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis")}
        selectInput('incomplete', 'Dragons in Partial colors', choices=c(Choose='',incompletelist), multiple=TRUE, selectize=TRUE)})
    output$resulttable<-renderDataTable({whattobreed(usefullist=as.integer(concatlists(input)))},options=list(pageLength=5,lengthMenu=list(c(1,5,10,-1),c('1','5','10','all')))) #makes a table output.
        output$dragstat<-renderDataTable({DragonStatDF},options=list(pageLength=1,lengthMenu=list(c(1,-1),c('1','all'))))


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
#}
# )


#would like to do cooler things with this:
#Take the first guy, take the second guy, just their images, then between them put the 6 possible outcomes' pictures, transparancy equal to their utility (0/1) and size equal to likelihood.
#Set a slider that allows you to skip a few outcomes if you aren't interested in the first one.
