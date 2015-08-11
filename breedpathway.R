#iteratively breed with 'infinite' tokens:
#need to load the "right" DragonID2:

load("ShinyBreeddata2.Rdata")
merger<-merger2
DragonID<-DragonID2[DragonID2$displayName%in%(levels(factor(c(as.character(merger$FirstDragon),as.character(merger$SecondDragon))))),]#dont want useless junk
DragonID<-DragonID[-c(1:5),]#the first 5 entries fuck everything up being redundant and legacy
DragonID$fragments<-c(1,1,1,1,1,1,1,1,8,1,1,1,1,5,1,5,5,1,1,5,5,12,8,5,5,5,8,16,5,8,16,16,5,5,8,20,48,48,1,20,12,60,48,20,20,48,20,48,60,20,40,96,72,40,40,40,96,72,72,96,96,96,96,96,96)

#start with some dragons:
initialowned<-c(1,0,1,0,1,rep(0,60)) #or whatever you initially start with...

nextd<-whattobreed(initialowned)[1,] #that's what's next
#need some way to indicate fragsowned.

