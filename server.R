#Currently missing:
# Add more UI elements - threshold for utility,
# Replace text with pictures
# Clearer Column names
# Slider for value of a duplicate
# Fragment Data for num of fragments to make dupeutility practical/useful.
# Color information (light cell background corresponding to the dragon color?)
whattobreed<-function(usefullist,dupeutility=c(rep(0.1,5)),assumebreedable=1){
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
DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,0,1,1,1,1,5,1,5,5,1,1,5,5,12,0,8,5,5,5,8,16,5,8,16,16,5,5,8,0,20,48,48,1,20,12,60,48,20,20,NA,NA,48,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
DragonID$fragments[is.na(DragonID$fragments)]<-0 #dont want NA math going weird, but i do prefer NAs to 0s for unknown values.
####The above data regarding fragments is very incomplete and in need of assistance before i allow the dupeutility measurements to go online###
   #second have to give a value for a 'dupe' egg for research purposes vs a new egg for breeding purposes
   possmerger$FirstFrags<-DragonID$fragments[match(possmerger$First,DragonID$displayName)]
   possmerger$SecondFrags<-DragonID$fragments[match(possmerger$Second,DragonID$displayName)]
   possmerger$ThirdFrags<-DragonID$fragments[match(possmerger$Third,DragonID$displayName)]
   possmerger$FourthFrags<-DragonID$fragments[match(possmerger$Fourth,DragonID$displayName)]
   possmerger$FifthFrags<-DragonID$fragments[match(possmerger$Fifth,DragonID$displayName)]
   possmerger$SixthFrags<-DragonID$fragments[match(possmerger$Sixth,DragonID$displayName)]
return(as.data.frame(possmerger[order(possmerger$ChanceofNewEgg,decreasing=TRUE),c(1,2,3,5,7,9,11,13,22)])) #22 is if i don't include fragment data
}
whattobreedbeta<-function(usefullist,dupeutility=c(rep(0.1,5)),assumebreedable=1){
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
  DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,0,1,1,1,1,5,1,5,5,1,1,5,5,12,0,8,5,5,5,8,16,5,8,16,16,5,5,8,0,20,48,48,1,20,12,60,48,20,20,NA,NA,48,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  DragonID$fragments[is.na(DragonID$fragments)]<-0 #dont want NA math going weird, but i do prefer NAs to 0s for unknown values.
  ####The above data regarding fragments is very incomplete and in need of assistance before i allow the dupeutility measurements to go online###
  #second have to give a value for a 'dupe' egg for research purposes vs a new egg for breeding purposes
  possmerger$FirstFrags<-DragonID$fragments[match(possmerger$First,DragonID$displayName)]
  possmerger$SecondFrags<-DragonID$fragments[match(possmerger$Second,DragonID$displayName)]
  possmerger$ThirdFrags<-DragonID$fragments[match(possmerger$Third,DragonID$displayName)]
  possmerger$FourthFrags<-DragonID$fragments[match(possmerger$Fourth,DragonID$displayName)]
  possmerger$FifthFrags<-DragonID$fragments[match(possmerger$Fifth,DragonID$displayName)]
  possmerger$SixthFrags<-DragonID$fragments[match(possmerger$Sixth,DragonID$displayName)]
  #    possmerger$OverallUtility<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful),
  #                                            possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful),
  #                                            possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful),
  #                                            possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful),
  #                                            possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful),
  #                                            possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)),na.rm=TRUE) #first collect the odds of a new egg. (Useful being binary)
  #determine the color of the various unwanted eggs 
  rvaluetoadd<-0
  rvaluetoadd<-rvaluetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*isred(possmerger$First)*dupeutility[1])/(possmerger$totalchance*possmerger$FirstFrags)
  rvaluetoadd<-rvaluetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*isred(possmerger$Second)*dupeutility[1])/(possmerger$totalchance*possmerger$SecondFrags)
  rvaluetoadd<-rvaluetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*isred(possmerger$Third)*dupeutility[1])/(possmerger$totalchance*possmerger$ThirdFrags)
  rvaluetoadd<-rvaluetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*isred(possmerger$Fourth)*dupeutility[1])/(possmerger$totalchance*possmerger$FourthFrags)
  rvaluetoadd<-rvaluetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*isred(possmerger$Fifth)*dupeutility[1])/(possmerger$totalchance*possmerger$FifthFrags)
  rvaluetoadd<-rvaluetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*isred(possmerger$Sixth)*dupeutility[1])/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
  
  pvaluetoadd<-0
  pvaluetoadd<-pvaluetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*ispurp(possmerger$First)*dupeutility[2])/(possmerger$totalchance*possmerger$FirstFrags)
  pvaluetoadd<-pvaluetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*ispurp(possmerger$Second)*dupeutility[2])/(possmerger$totalchance*possmerger$SecondFrags)
  pvaluetoadd<-pvaluetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*ispurp(possmerger$Third)*dupeutility[2])/(possmerger$totalchance*possmerger$ThirdFrags)
  pvaluetoadd<-pvaluetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*ispurp(possmerger$Fourth)*dupeutility[2])/(possmerger$totalchance*possmerger$FourthFrags)
  pvaluetoadd<-pvaluetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*ispurp(possmerger$Fifth)*dupeutility[2])/(possmerger$totalchance*possmerger$FifthFrags)
  pvaluetoadd<-pvaluetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*ispurp(possmerger$Sixth)*dupeutility[2])/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
  
  bvaluetoadd<-0
  bvaluetoadd<-bvaluetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*isblue(possmerger$First)*dupeutility[3])/(possmerger$totalchance*possmerger$FirstFrags)
  bvaluetoadd<-bvaluetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*isblue(possmerger$Second)*dupeutility[3])/(possmerger$totalchance*possmerger$SecondFrags)
  bvaluetoadd<-bvaluetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*isblue(possmerger$Third)*dupeutility[3])/(possmerger$totalchance*possmerger$ThirdFrags)
  bvaluetoadd<-bvaluetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*isblue(possmerger$Fourth)*dupeutility[3])/(possmerger$totalchance*possmerger$FourthFrags)
  bvaluetoadd<-bvaluetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*isblue(possmerger$Fifth)*dupeutility[3])/(possmerger$totalchance*possmerger$FifthFrags)
  bvaluetoadd<-bvaluetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*isblue(possmerger$Sixth)*dupeutility[3])/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
  
  ovaluetoadd<-0
  ovaluetoadd<-ovaluetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*isorange(possmerger$First)*dupeutility[4])/(possmerger$totalchance*possmerger$FirstFrags)
  ovaluetoadd<-ovaluetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*isorange(possmerger$Second)*dupeutility[4])/(possmerger$totalchance*possmerger$SecondFrags)
  ovaluetoadd<-ovaluetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*isorange(possmerger$Third)*dupeutility[4])/(possmerger$totalchance*possmerger$ThirdFrags)
  ovaluetoadd<-ovaluetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*isorange(possmerger$Fourth)*dupeutility[4])/(possmerger$totalchance*possmerger$FourthFrags)
  ovaluetoadd<-ovaluetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*isorange(possmerger$Fifth)*dupeutility[4])/(possmerger$totalchance*possmerger$FifthFrags)
  ovaluetoadd<-ovaluetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*isorange(possmerger$Sixth)*dupeutility[4])/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
  
  gvaluetoadd<-0
  gvaluetoadd<-gvaluetoadd+(possmerger$FirstChance*(!possmerger$FirstUseful)*isgreen(possmerger$First)*dupeutility[5])/(possmerger$totalchance*possmerger$FirstFrags)
  gvaluetoadd<-gvaluetoadd+(possmerger$SecondChance*(!possmerger$SecondUseful)*isgreen(possmerger$Second)*dupeutility[5])/(possmerger$totalchance*possmerger$SecondFrags)
  gvaluetoadd<-gvaluetoadd+(possmerger$ThirdChance*(!possmerger$ThirdUseful)*isgreen(possmerger$Third)*dupeutility[5])/(possmerger$totalchance*possmerger$ThirdFrags)
  gvaluetoadd<-gvaluetoadd+(possmerger$FourthChance*(!possmerger$FourthUseful)*isgreen(possmerger$Fourth)*dupeutility[5])/(possmerger$totalchance*possmerger$FourthFrags)
  gvaluetoadd<-gvaluetoadd+(possmerger$FifthChance*(!possmerger$FifthUseful)*isgreen(possmerger$Fifth)*dupeutility[5])/(possmerger$totalchance*possmerger$FifthFrags)
  gvaluetoadd<-gvaluetoadd+(possmerger$SixthChance*(!possmerger$SixthUseful)*isgreen(possmerger$Sixth)*dupeutility[5])/(possmerger$totalchance*possmerger$SixthFrags) #collect the chance that your 20 token roll will get you a 'complete' duplicate egg.
  valuetoadd<-gvaluetoadd+ovaluetoadd+bvaluetoadd+pvaluetoadd+rvaluetoadd
  possmerger$ResearchValue<-valuetoadd #add the odds of new egg to the odds of a 'research-worthy' egg.
  
  #  return(as.data.frame(possmerger[order(possmerger$ChanceofNewEgg,decreasing=TRUE),c(1,2,3,5,7,9,11,13,22,29)])) #29 is if i do include fragment data.
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
isgreen<-function(list){
  greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis")
  return(list%in%greenlist)
}
isblue<-function(list){
  bluelist<-c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")
  return(list%in%bluelist)
}
isorange<-function(list){
  orangelist<-c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")
  return(list%in%orangelist)
}
ispurp<-function(list){
  purplelist<-c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius","Dominus")
  return(list%in%purplelist)
}
isred<-function(list){
  redlist<-c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")
  return(list%in%redlist)
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
    output$resulttable<-renderDataTable({whattobreed(usefullist=as.integer(concatlists(input)),
                                        dupeutility=c(0.1,0.1,0.1,0.1,0.1))},
                                        #             dupeutility = c(input$rval,input$pval,input$bval,input$oval,input$gval))},
                                        options=list(pageLength=5,lengthMenu=list(c(1,5,10,-1),c('1','5','10','all')))) #makes a table output.
        output$dragstat<-renderDataTable({DragonStatDF},options=list(pageLength=1,lengthMenu=list(c(1,-1),c('1','all'))))


    #})
# output$uibeta<-renderUI({
#   if(is.null(input$input_types))  {return()}
#   incompletelist<-NULL
#   if("Red"%in%input$input_types){incompletelist<-c(incompletelist,"Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara","Fenrir")}
#   if("Purple"%in%input$input_types){incompletelist<-c(incompletelist,"Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius","Dominus")}
#   if("Blue"%in%input$input_types){incompletelist<-c(incompletelist,"Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus","Numen")}
#   if("Orange"%in%input$input_types){incompletelist<-c(incompletelist,"Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo","Darja")}
#   if("Green"%in%input$input_types){incompletelist<-c(incompletelist,"Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis")}
#   selectInput('incomplete', 'Dragons in Partial colors', choices=c(Choose='',incompletelist), multiple=TRUE, selectize=TRUE)})
#   
output$resulttable<-renderDataTable({whattobreed(usefullist=as.integer(concatlists(input)),
                                    dupeutility = c(input$rval,input$pval,input$bval,input$oval,input$gval))},
                                    options=list(pageLength=5,lengthMenu=list(c(1,5,10,-1),c('1','5','10','all')))) #makes a table output.
})
  #output$dragstat<-renderDataTable({DragonStatDF},options=list(pageLength=1,lengthMenu=list(c(1,-1),c('1','all'))))

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
