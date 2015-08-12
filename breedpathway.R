#iteratively breed with 'infinite' tokens:
#need to load the "right" DragonID2:

load("ShinyBreeddata2.Rdata")
merger<-merger2
DragonID<-DragonID2[DragonID2$displayName%in%(levels(factor(c(as.character(merger$FirstDragon),as.character(merger$SecondDragon))))),]#dont want useless junk
DragonID<-DragonID[-c(1:5),]#the first 5 entries fuck everything up being redundant and legacy
DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,1,1,1,1,5,1,5,5,1,1,5,5,12,8,5,5,5,8,16,5,8,16,16,5,5,8,20,48,48,1,20,12,60,48,20,20,48,20,48,60,20,40,96,72,40,40,40,96,72,72,96,96,96,96,96,96)

#start with some dragons:
initialowned<-c(1,1,1,0,0,rep(0,60)) #or whatever you initially start with...
ownedlist<-initialowned
nextd<-bestdeck(ownedlist)
#nextd<-whattobreed(ownedlist)[1,] #that's what's next
#need some way to indicate fragsowned.
fragsowned<-initialowned
#pretend spending function
#assume that everything ignores variance, decimals are okay.
pretendspending<-function(deck,fragsowned,DragonID){
#see which ones are new
newdragons<-deck[c(3,5,7,9,11,13)][!is.na(deck[c(3,5,7,9,11,13)])][deck[c(16:21)][!is.na(deck[c(3,5,7,9,11,13)])]==1]
newodds<-deck[c(4,6,8,10,12,14)][!is.na(deck[c(3,5,7,9,11,13)])][deck[c(16:21)][!is.na(deck[c(3,5,7,9,11,13)])]==1]/deck$totalchance
fragsneeded<-DragonID$fragments[DragonID$displayName%in%newdragons]-fragsowned[DragonID$displayName%in%newdragons]
tokenspent<-min(fragsneeded/newodds)*20
fragsearned<-newodds*tokenspent/20
fragsowned[match(newdragons,DragonID[,2])]<-fragsowned[match(newdragons,DragonID[,2])]+(fragsearned/DragonID$fragments[match(newdragons,DragonID[,2])])
#print(c(tokenspent))#,fragsearned,newdragons))
fragsowned<-round(fragsowned,digits=3)
return(fragsowned)
}

#i want to return the following for the first:  Leviathan:  cost = 45.8.

bestdeck<-function(usefullist,dupeutility=c(rep(0.1,5)),assumebreedable=1,empirical=FALSE){
    load("ShinyBreeddata2.Rdata")
    merger<-merger2
    DragonID<-DragonID2[DragonID2$displayName%in%(levels(factor(c(as.character(merger$FirstDragon),as.character(merger$SecondDragon))))),]#dont want useless junk
    DragonID<-DragonID[-c(1:5),]#the first 5 entries fuck everything up being redundant and legacy
    DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,1,1,1,1,5,1,5,5,1,1,5,5,12,8,5,5,5,8,16,5,8,16,16,5,5,8,20,48,48,1,20,12,60,48,20,20,48,20,48,60,20,40,96,72,40,40,40,96,72,72,96,96,96,96,96,96)
        if(length(usefullist)!=length(DragonID$identifier)){return(0)}#the number of dragons now..
    DragonID$owned<-usefullist
    if(assumebreedable==1){DragonID$owned[DragonID$owned>=1]<-1
    DragonID$owned[DragonID$owned<1]<-0}#handles the "fractional dragon ownership" issue from the pretendspending method
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
    possmerger$ChanceofNewEgg<-rowSums(cbind(possmerger$FirstChance/possmerger$totalchance*(possmerger$FirstUseful)+0,
                                             possmerger$SecondChance/possmerger$totalchance*(possmerger$SecondUseful)+0,
                                             possmerger$ThirdChance/possmerger$totalchance*(possmerger$ThirdUseful)+0,
                                             possmerger$FourthChance/possmerger$totalchance*(possmerger$FourthUseful)+0,
                                             possmerger$FifthChance/possmerger$totalchance*(possmerger$FifthUseful)+0,
                                             possmerger$SixthChance/possmerger$totalchance*(possmerger$SixthUseful)+0),na.rm=TRUE)
    if(max(possmerger$ChanceofNewEgg)==0){return(0)}
    return(as.data.frame(possmerger[order(possmerger$ChanceofNewEgg,decreasing=TRUE),])[1,]) #22 is if i don't include fragment data
}
BreedTree<-function(initialowned=c(1,1,1,0,0,rep(0,60))){
#    initialowned<-c(1,1,1,0,0,rep(0,60)) #or whatever you initially start with...
    ownedlist<-initialowned
    nextd<-bestdeck(ownedlist)
    fragsowned<-initialowned
    while(length(nextd)>0){
        newolist<-pretendspending(nextd,fragsowned=fragsowned,DragonID=DragonID)
        #print(c(as.vector(nextd$FirstDragon),as.vector(nextd$SecondDragon)))
        fragsowned<-newolist
        nextd<-bestdeck(fragsowned)
        #fordebuggingpurposes
    }
}


#How do i implement goal-seeking into this?  I basically want to say that XYZA are the only important dragons and to ignore anything that's weaker.
#but i'd also like to say that getting X+Y is better than X alone.
#step 1: Allow whobreedsx to take multiple inputs. (Check!)