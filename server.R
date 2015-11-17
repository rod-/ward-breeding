#helpText("Note that gold dragons are currently misnamed:  Bander = Bander, Zephyr=Caladbolg, Zerka = Firactus"),
#Currently missing:
# Add more UI elements - threshold for utility,
# Replace text with pictures
# Clearer Column names
# Slider for value of a duplicate
# Fragment Data for num of fragments to make dupeutility practical/useful.
# Color information (light cell background corresponding to the dragon color?)
whattobreed<-function(usefullist,dupeutility=c(rep(0.1,5)),assumebreedable=1,empirical=FALSE,outcolumns=c(1,2,3,5,7,9,11,13,22)){
  load("ShinyBreeddata170.Rdata")
  merger<-merger2

  colnames(merger)[1:2]<-c("DragonA","DragonB")
  DragonID<-DragonID2[DragonID2$displayName%in%(levels(factor(c(as.character(merger$DragonA),as.character(merger$DragonB))))),]#dont want useless junk
  DragonID<-DragonID[-c(1:4),]#the first 5 entries fuck everything up being redundant and legacy
    if(length(usefullist)!=length(DragonID$identifier)){return(0)}#the number of dragons now..
  DragonID$owned<-usefullist

  DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,1,1,1,1,5,1,5,5,1,1,5,5,12,8,5,5,5,8,16,5,8,16,16,5,5,8,20,48,48,1,20,12,60,48,20,20,48,20,48,60,20,40,96,72,40,40,40,96,72,72,72,96,96,96,96,96,170,170,170,96,96,96,96,250,250,250,250,250)

      if(assumebreedable==1){DragonID$owned[DragonID$owned==2]<-1}
  possmerger<-merger[DragonID$owned[match(merger$DragonA,DragonID$displayName)]==1,]#do i own the first dragon
  possmerger<-possmerger[DragonID$owned[match(possmerger$DragonB,DragonID$displayName)]==1,] #do i own the second
  if(length(possmerger$DragonA)<=1){return(data.frame(NULL))}
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
  if(empirical==TRUE){
    isnerfed<-function(list){
      orangelist<-c("Noss","Hydron","Amarok","Luminark","Septys","Enki","Durga")
      return(list%in%orangelist)
    }
    possmerger$FirstChance[isnerfed(possmerger$First)]<-possmerger$FirstChance[isnerfed(possmerger$First)]*2/3
    possmerger$SecondChance[isnerfed(possmerger$Second)]<-possmerger$SecondChance[isnerfed(possmerger$Second)]*2/3
    possmerger$ThirdChance[isnerfed(possmerger$Third)]<-possmerger$ThirdChance[isnerfed(possmerger$Third)]*2/3
    possmerger$FourthChance[isnerfed(possmerger$Fourth)]<-possmerger$FourthChance[isnerfed(possmerger$Fourth)]*2/3
    possmerger$FifthChance[isnerfed(possmerger$Fifth)]<-possmerger$FifthChance[isnerfed(possmerger$Fifth)]*2/3
    possmerger$SixthChance[isnerfed(possmerger$Sixth)]<-possmerger$SixthChance[isnerfed(possmerger$Sixth)]*2/3
    isnerfedhard<-function(list){
      orangelist<-c("Amarok","Durga")
      return(list%in%orangelist)
    }
    possmerger$FirstChance[isnerfedhard(possmerger$First)]<-possmerger$FirstChance[isnerfedhard(possmerger$First)]*3/4
    possmerger$SecondChance[isnerfedhard(possmerger$Second)]<-possmerger$SecondChance[isnerfedhard(possmerger$Second)]*3/4
    possmerger$ThirdChance[isnerfedhard(possmerger$Third)]<-possmerger$ThirdChance[isnerfedhard(possmerger$Third)]*3/4
    possmerger$FourthChance[isnerfedhard(possmerger$Fourth)]<-possmerger$FourthChance[isnerfedhard(possmerger$Fourth)]*3/4
    possmerger$FifthChance[isnerfedhard(possmerger$Fifth)]<-possmerger$FifthChance[isnerfedhard(possmerger$Fifth)]*3/4
    possmerger$SixthChance[isnerfedhard(possmerger$Sixth)]<-possmerger$SixthChance[isnerfedhard(possmerger$Sixth)]*3/4
    #recalculate totalchance
        possmerger$totalchance<-rowSums(cbind(possmerger$FirstChance,possmerger$SecondChance,possmerger$ThirdChance,possmerger$FourthChance,possmerger$FifthChance,possmerger$SixthChance),na.rm=TRUE)}

  possmerger$NewEggRate<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful)+0,
                                           possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful)+0,
                                           possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful)+0,
                                           possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful)+0,
                                           possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful)+0,
                                           possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)+0),na.rm=TRUE)
  {
  isgold<-function(list){
    goldlist<-c("Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")
    return(list%in%goldlist)
  }
  isgreen<-function(list){
      greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis","Hugin","Munin")
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
  possmerger$RedPer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*isred(possmerger$First)*(possmerger$FirstUseful==0),
                                 1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*isred(possmerger$Second)*(possmerger$FirstUseful==0),
                                 1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*isred(possmerger$Third)*(possmerger$FirstUseful==0),
                                 1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*isred(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                 1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*isred(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                 1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*isred(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
  possmerger$BluePer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*isblue(possmerger$First)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*isblue(possmerger$Second)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*isblue(possmerger$Third)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*isblue(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*isblue(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*isblue(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
  possmerger$PurplePer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*ispurp(possmerger$First)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*ispurp(possmerger$Second)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*ispurp(possmerger$Third)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*ispurp(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*ispurp(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*ispurp(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
  possmerger$OrangePer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*isorange(possmerger$First)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*isorange(possmerger$Second)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*isorange(possmerger$Third)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*isorange(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*isorange(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*isorange(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
  possmerger$GreenPer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*isgreen(possmerger$First)*(possmerger$FirstUseful==0),
                                                         1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*isgreen(possmerger$Second)*(possmerger$FirstUseful==0),
                                                         1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*isgreen(possmerger$Third)*(possmerger$FirstUseful==0),
                                                         1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*isgreen(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                                         1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*isgreen(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                                         1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*isgreen(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
    possmerger$GoldPer1000<-suppressWarnings(rowSums(cbind(1/(DragonID$fragments[match((possmerger$First),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FirstChance/possmerger$totalchance)*isgold(possmerger$First)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Second),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SecondChance/possmerger$totalchance)*isgold(possmerger$Second)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Third),DragonID$displayName,nomatch="Leviathan")])*(possmerger$ThirdChance/possmerger$totalchance)*isgold(possmerger$Third)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fourth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FourthChance/possmerger$totalchance)*isgold(possmerger$Fourth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Fifth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$FifthChance/possmerger$totalchance)*isgold(possmerger$Fifth)*(possmerger$FirstUseful==0),
                                                        1/(DragonID$fragments[match((possmerger$Sixth),DragonID$displayName,nomatch="Leviathan")])*(possmerger$SixthChance/possmerger$totalchance)*isgold(possmerger$Sixth)*(possmerger$FirstUseful==0)),na.rm=TRUE))
}#calculate the research egg values for each guy.
  #      possmerger$GreenEggRate<-rowSums(cbind())
  possmerger[,c(23:28)]<-possmerger[,c(23:28)]*50
      returnval<-as.data.frame(possmerger[order(possmerger$NewEggRate,decreasing=TRUE),outcolumns])
    rownames(returnval)<-NULL
  return(returnval) #22 is if i don't include fragment data
}
whobreedsx<-function(ownedlist,dragonx="Amarok",owned=FALSE,skiplist=NULL){
  if(is.null(dragonx)){return(0)}
  if(is.null(ownedlist)){return(0)}
  load("ShinyBreeddata170.Rdata")
  merger<-merger2
  wlist<-merger
  colnames(wlist)[1:2]<-c("DragonA","DragonB")
  if(owned==TRUE){
    wlist<-wlist[ownedlist%in%merger$DragonA] #make sure you have both of the breedingpair
    wlist<-wlist[ownedlist%in%wlist$DragonB]
  }
  #dont use self
  wlist<-wlist[wlist$DragonA!=dragonx,]
  wlist<-wlist[wlist$DragonB!=dragonx,]
  wlist<-subset(wlist,First==dragonx|Second==dragonx|Third==dragonx|Fourth==dragonx|Fifth==dragonx|Sixth==dragonx)
  if(length(wlist)==0){return(0)}
  if(!is.null(skiplist)){
    wlist<-wlist[!(wlist$DragonA%in%skiplist),]
    wlist<-wlist[!(wlist$DragonB%in%skiplist),]
  }
  #do a quick calculation of the odds of getting dragonx
  #deal with NAs

  suppressWarnings(wlist[is.na(wlist)]<-0) #fixes the numerics

  wlist[is.na(wlist)]<-"Draco" #fixes the factors.  Makes it weird with draco but whatever
  levels(wlist$First)[levels(wlist$First)=="Draco"]<-""
  levels(wlist$Second)[levels(wlist$Second)=="Draco"]<-""
  levels(wlist$Third)[levels(wlist$Third)=="Draco"]<-""
  levels(wlist$Fourth)[levels(wlist$Fourth)=="Draco"]<-""
  levels(wlist$Fifth)[levels(wlist$Fifth)=="Draco"]<-""
  levels(wlist$Sixth)[levels(wlist$Sixth)=="Draco"]<-""
  wlist$DesiredOdds<-(wlist$FirstChance/wlist$totalchance*(wlist$First==dragonx))+(wlist$SecondChance/wlist$totalchance*(wlist$Second==dragonx))+(wlist$ThirdChance/wlist$totalchance*(wlist$Third==dragonx))+
    (wlist$FourthChance/wlist$totalchance*(wlist$Fourth==dragonx))+(wlist$FifthChance/wlist$totalchance*(wlist$Fifth==dragonx))+(wlist$SixthChance/wlist$totalchance*(wlist$Sixth==dragonx))
  #return pair and odds
  retval<-wlist[order(wlist$DesiredOdds,decreasing=TRUE),c(1,2,3,5,7,9,11,13,16)]
  rownames(retval)<-NULL
  return(retval)
}
overleveler<-function(mybase,builder,storage,strategy="highest",plevel=1,goal=84,buildtimer=0.8){
    #set subgoals: hit those levels.  add those towers.
#some error prevention/mitigation
    if(is.null(plevel)){return(0)}
    if(is.null(goal)){return(0)}
    if(is.null(builder)){return(0)}
    if(is.null(storage)){return(0)}
    if(any(is.na(mybase))){return(0)}
    if(max(mybase)>25){return(0)}
    load("levelerdata.Rdata")
    #clean the weird format a bit
    half$upgradeCost<-as.numeric(gsub(pattern = "piercing:",replacement = "",x = half$upgradeCost))
    StorageUpgrades$maxStorageData<-as.numeric(gsub(pattern="food:[0-9]+\\|piercing:",replacement="",x=StorageUpgrades$maxStorageData))
    pbuilder=as.numeric(as.character(half$requiredBuilderLevel))
    StorageUpgrades$levelRequired<-as.numeric(as.character(StorageUpgrades$levelRequired))

    subgoals<-c(1,4,7,12,17,21,25,28,31,40,42,46,50,56,61,64,67,73,79,85,91,94)#these are the levels where you change max towers
    towerlevels<-c(1,1,4,4,4,4,7,7,12,17,17,17,21,25,28,31,40,42,42,46,50,56,61,64,67,73,79,85,91,94)#these are the levels where you change max towers
    pstorage=NULL#required storage levels for various towers
    II<-0
    for(I in half$upgradeCost){II<-II+1;pstorage[II]<-(min(which(StorageUpgrades$maxStorageData[2:34]>I)))}
    newlevel=plevel
    totaltime<-0;totalwood<-0

    #loop over subgoals
    allsubgoals<-c(subgoals[which(subgoals>plevel&subgoals<goal)],goal)
    newbuilder<-builder
    newstore<-storage
    newbase<-mybase
    totalqueue<-vector()
    for(I in allsubgoals){
    result<-leveler(newbase,newbuilder,newstore,plevel=newlevel,goal=I,buildtimer=buildtimer,strategy=strategy)
    newlevel<-I
    newbase<-unlist(result[4])
    maxtower<-sum(towerlevels<(newlevel+1))
    if(builder<17){
    newbuilder<-pbuilder[maxtower]}
    else{(newbuilder=17)}
    if(storage<21){
    newstore<-pstorage[maxtower]}
    else{(newstore=storage)}
    newwood<-unlist(result[3])
    newtime<-unlist(result[2])
    totalwood<-totalwood+newwood
    totaltime<-totaltime+newtime
    totalqueue<-c(totalqueue,unlist(result[1]))
    }
    bonustime<-0
    if(newstore>storage){bonustime<-speedupconvert(sum(as.double(as.character(StorageUpgrades$upgradeTimeInSeconds[((storage+1):newstore)+1]))),buildtimer=buildtimer)}
    if(newbuilder>builder){bonustime<-bonustime+speedupconvert(sum(as.double(as.character(BuilderUpgrades$upgradeTimeInSeconds[((builder+1):newbuilder)+1]))),buildtimer)}
    #don't add in bonustime until the very last.
    finalresult<-c(totaltime+bonustime,makedisplayable(totalwood),newbuilder,newstore,newbase)
    return(finalresult)
}
makedisplayable<-function(number){
    if(number/1e6>1){return(paste0(signif(number,digits=3)/1e6,"M"))}
    if(number/1e6<1){return(paste0(signif(number,digits=3)/1e3, "K"))}

}
leveler<-function(mybase,builder,storage,strategy="highest",plevel=1,goal=84,buildtimer=0.8){
    #load in the initial dataframes
#    load("TowerStats.rData")
    load("levelerdata.Rdata")
    #and probably need to fix the towerstats to make them numeric vectors
    half$upgradeCost<-as.double(gsub(pattern = "piercing:",replacement = "",x=half$upgradeCost))
    half$upgradeReward<-as.double(gsub(pattern="experience:",replacement="",x=half$upgradeReward))
    #set up the goals and initial conditions
    totaltime<-c(0,0,0,0,0,0,0)#as speedups:  1 min, 3 min, 15 min, 30 min, 1hr, 3hr, 12hr
    totalwood<-0
    goalexp<-as.integer(as.character(exp$requiredXp[goal+2]))
    currentexp<-as.integer(as.character(exp$requiredXp[plevel+2]))
    expincrease<-c(half$upgradeReward[1:30],0)
    timeincrease<-as.double(as.character(half$upgradeTimeInSeconds))[1:30]
    woodcost<-half$upgradeCost[1:30]
    #strategies
    if(strategy=="fastest"){
        upgradepriority<-order(half$upgradeReward[1:30]/as.double(as.character(half$upgradeTimeInSeconds[1:30])),decreasing = TRUE)
    }
    else if(strategy=="highest"){
        upgradepriority<-c(30:1)
    }
    #converts time (in seconds) into the optimal number of speedups required (preference to high-hour ones!)
    maxpossible<-max(which(ispossible(builder,storage,plevel)[1:30]>0))
    if(sum(mybase<maxpossible)==0){return(list(0,c(0,0,0,0,0,0,0),0,rep(0,36)))}
    cupgradepriority<-upgradepriority[upgradepriority<=maxpossible]
    outputscript<-vector()
    maxposexp<-function(mybase,maxpossible){
        if(max(mybase)>30){warning("Check your input:Levels above Maximum!")}
        totalexp<-0
                for(Z in mybase){
            totalexp<-sum(expincrease[(Z+1):maxpossible])+totalexp
        }
    return(totalexp)}
    if(is.numeric(goalexp)&is.numeric(currentexp)){
        if((maxposexp(mybase,maxpossible)+currentexp)<goalexp){return(list(0,c(0,0,0,0,0,0,0),0,rep(0,36)))}
        loopcounter<-0
        while(currentexp<goalexp){
        loopcounter<-loopcounter+1
                for(I in 1:length(cupgradepriority)){
            numpriority<-sum(mybase==(cupgradepriority[I]-1))
            if(numpriority>0){
                currentexp<-currentexp+expincrease[cupgradepriority[I]]
                totaltime<-totaltime+speedupconvert(timeincrease[cupgradepriority[I]],buildtimer)
                totalwood<-totalwood+woodcost[cupgradepriority[I]]
                mybase[which(mybase==(cupgradepriority[I]-1))[1]]<-(cupgradepriority[I])
                outputscript<-c(outputscript,cupgradepriority[I]-1)
                break
                }
        }
    if(loopcounter>10000){break}
        }

    }
    return(list(outputscript,totaltime,totalwood,mybase))
    #wish to convert the total time into speedups


}
ispossible<-function(builder,storage,plevel){
    pbuilder=c(0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,13,14,16,17,19,20,20,20,20)
    pstorage=c(1,1,1,1,1,2,3,3,4,4,5,5,6,7,8,8,9,10,12,14,16,19,21,22,23,25,27,29,31,32)
    possibiles<-(builder>=pbuilder)*(storage>=pstorage)
    storagelevels=c(0,7,12,17,21,25,28,31,34,37,40,42,44,46,48,50,52,54,56,58,61,64,67,70,73,76,79,82,85,88,91,94,97)
    builderlevels=c(4,4,7,12,18,23,28,33,38,41,45,48,51,53,56,58,61,63,66,68,71)
    maxpossible<-c(sum(builderlevels<plevel+1),sum(storagelevels<plevel+1))
    return(c(possibiles,maxpossible))
}
speedupconvert<-function(time,buildtimer){
    time<-time*buildtimer
    twelve<-as.integer(time/43200)
    remainder<-time-(twelve*43200)
    three<-as.integer(remainder/10800)
    remainder<-remainder-(three*10800)
    one<-as.integer(remainder/3600)
    remainder<-remainder-(one*3600)
    half<-as.integer(remainder/1800)
    remainder<-remainder-(half*1800)
    quarter<-as.integer(remainder/900)
    remainder<-remainder-(quarter*900)
    threem<-as.integer(remainder/180)
    remainder<-remainder-(threem*180)
    last<-as.integer(remainder/60)
    return(c(twelve,three,one,half,quarter,threem,last))
}
calctimer<-function(reslist){
#    column(2, wellPanel(checkboxGroupInput('buildresearch',label = "Build Speed Bonuses",choices=c("Red","Blue","Orange","Green","Event10","Event25"),selected = c("Red","Blue","Orange"))))),
base<-.05*sum(c("Red","Blue","Orange","Green")%in%reslist)
bonus<-max(0.25*"Event25"%in%reslist,0.1*"Event10"%in%reslist)
return(1-(base+bonus))
}
load("ShinyBreeddata170.Rdata")
#DragonStatDF<-DragonID
concatlists<-function(files){
  redlist<-c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara")
  purplelist<-c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius")
  bluelist<-c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus")
  orangelist<-c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo")
  greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin")
  goldlist<-c("Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")
  listofeverything<-c(redlist,purplelist,bluelist,orangelist,greenlist,goldlist)
  currentlist<-c(files$incomplete,files$incompleteB)

  if("Red"%in%files$fullgroups){currentlist<-c(redlist,currentlist)}
  if("Purple"%in%files$fullgroups){currentlist<-c(purplelist,currentlist)}
  if("Blue"%in%files$fullgroups){currentlist<-c(bluelist,currentlist)}
  if("Orange"%in%files$fullgroups){currentlist<-c(orangelist,currentlist)}
  if("Green"%in%files$fullgroups){currentlist<-c(greenlist,currentlist)}
  if("Gold"%in%files$fullgroups){currentlist<-c(goldlist,currentlist)}

  return(listofeverything%in%currentlist) #reduces list down to a binary vector
}
isgold<-function(list){
  goldlist<-c("Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")
  return(list%in%goldlist)
}
isgreen<-function(list){
  greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Carsis","Hugin","Munin")
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
convertinterests<-function(list){
    default<-c(1,2,3,5,7,9,11,13,22)
added<-c("Red","Blue","Purple","Orange","Green","Gold")%in%list*c(23:28)
added<-added[added!=0]
if(length(added)>0){
return(c(default,added))
}
else(return(default))}
library(shiny)
library(DT)
shinyServer(function(input, output) {
    redlist<-c("Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara")
    purplelist<-c("Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius")
    bluelist<-c("Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus")
    orangelist<-c("Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo")
    greenlist<-c("Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin")
    goldlist<-c("Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")
    listofeverything<-c(redlist,purplelist,bluelist,orangelist,greenlist,goldlist)

  output$ui<-renderUI({
    if(is.null(input$input_types))
    {return(0)}
    incompletelist<-NULL
    if("Red"%in%input$input_types){incompletelist<-c(incompletelist,"Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara")}
    if("Purple"%in%input$input_types){incompletelist<-c(incompletelist,"Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius")}
    if("Blue"%in%input$input_types){incompletelist<-c(incompletelist,"Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus")}
    if("Orange"%in%input$input_types){incompletelist<-c(incompletelist,"Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo")}
    if("Green"%in%input$input_types){incompletelist<-c(incompletelist,"Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin")}
    if("Gold"%in%input$input_types){incompletelist<-c(incompletelist,"Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")}

        selectInput('incomplete', 'Dragons in Partial colors', choices=c(Choose='',incompletelist), multiple=TRUE, selectize=TRUE)})

    output$uibeta<-renderUI({
    if(is.null(input$input_typesBeta))  {return(0)}
    incompletelist<-NULL
    if("Red"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Draco","Leviathan","Frigg","Zin","Hext","Aetrix","Hantu","Kastor","Kinnara")}
    if("Purple"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Trollis","Laekrian","Merk","Dactyl","Gog","Huli","Borg","Vladimir","Alikorn","Daemun","Garuda","Klax","Arborius")}
    if("Blue"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Grypp","Jura","Kromon","Yanari","Vazir","Drude","Sahran","Bolt","Kelsis","Etzel","Kobahl","Baldr","Viscus")}
    if("Orange"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Ankor","Noss","Hydron","Slynx","Habrok","Volos","Amarok","Luminark","Lucius","Bronze","Septys","Ruma","Enki","Durga","Kolo")}
    if("Green"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Gaspar","Karna","Naga","Nassus","Garzev","Serabis","Urd","Ith","Elixis","Pandi","Danzig","Nix","Ettin","Hugin","Munin")}
    if("Gold"%in%input$input_typesBeta){incompletelist<-c(incompletelist,"Caladbolg","Firactus","Bander","Ferrox","Lumen","Basileus","Yersinu","Whalegnawer","Consurgens","Sekoronos","Khrysos","Chthoteuthis")}
    selectInput('chosendragon', 'Dragon you want to breed', choices=c(Choose='',incompletelist), multiple=TRUE, selectize=TRUE,selected = "Amarok")

  })
    load("levelerdata.Rdata")
  output$towerdata<-DT::renderDataTable({data.frame(minLevel=c(1,1,4,4,4,7,12,12,17,17,21,21,25,28,31,31,34,37,42,46,50,56,61,64,67,73,79,85,91,94),exp=gsub(x=half$upgradeReward[1:30],pattern="experience:",replacement=""),wood=gsub(x=half$upgradeCost[1:30],pattern="piercing:",replacement=""))},options=list(searching=FALSE,lengthChange=FALSE,paging=FALSE,info=FALSE))
  output$resulttable<-DT::renderDataTable({datatable(whattobreed(usefullist=as.integer(concatlists(input)),
                                                                 dupeutility = c(input$rval,input$pval,input$bval,input$oval,input$gval),empirical=input$empirical,outcolumns = convertinterests(input$researchinterests)),
                                                     options=list(pageLength=5,lengthMenu=list(c(1,5,10,-1),c('1','5','10','all')),info=FALSE))%>%formatStyle(c(1:8),
                                                    Color=styleEqual(listofeverything,values = c(rep('#FE2E2E',9),rep('#8000FF',13),rep('#0040FF',13),rep('#FF8000',15),rep('green',15),rep('goldenrod',12))))})
  output$resbeta<-DT::renderDataTable({datatable(whobreedsx(ownedlist = c(rep(1,68)),dragonx = input$chosendragon,skiplist = input$skipgreen),
                                  options=list(pageLength=5,lengthMenu=list(c(1,5,10,-1),c('1','5','10','all')),info=FALSE))%>%formatStyle(c(1:8),
                     Color=styleEqual(listofeverything,values = c(rep('#FE2E2E',9),rep('#8000FF',13),rep('#0040FF',13),rep('#FF8000',15),rep('green',15),rep('goldenrod',12))))
       })

  leveler<-reactive(overleveler(mybase = c(input$tower1,input$tower2,input$tower3,input$tower4,input$tower5,input$tower6,input$tower7,input$tower8,input$tower9,input$tower10,input$tower11,input$tower12,input$tower13,input$tower14,input$tower15,input$tower16,input$tower17,input$tower18,input$tower19,input$tower20,input$tower21,input$tower22,input$tower23,input$tower24,input$tower25,input$tower26,input$tower27,input$tower28,input$tower29,input$tower30,input$tower31,input$tower32,input$tower33,input$tower34,input$tower35,input$tower36),
                                builder=input$bldrlevel,storage=input$storlevel,strategy=input$strategy,plevel=input$plevel,goal=input$ptarget,buildtimer=calctimer(input$buildresearch)))
  output$leveler1<-renderText(leveler()[1])
  output$leveler2<-renderText(leveler()[2])
  output$leveler3<-renderText(leveler()[3])
  output$leveler4<-renderText(leveler()[4])
  output$leveler5<-renderText(leveler()[5])
  output$leveler6<-renderText(leveler()[6])
  output$leveler7<-renderText(leveler()[7])
  output$leveler8<-renderText(leveler()[8])
  output$leveler9<-renderText(leveler()[9])
  output$leveler10<-renderText(leveler()[10])
  output$leveler11<-renderText(leveler()[11:46])


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