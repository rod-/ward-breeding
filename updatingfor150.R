#Update the merger2 database and stuff
load("Shiny")
Dragon2<-read.csv("/Users/rod/Documents/ward151/Dragon.csv")
Deck2<-read.csv("C:/Users/rod/Documents/ward151/Deck.csv")
egg2<-read.csv("C:/Users/rod/Documents/ward151/DragonEgg.csv")
cross2<-read.csv("C:/Users/rod/Documents/ward151/CrossbreedingPairs.csv")
#test<-read.csv("C:/Users/rod/Documents/ward151/Deck.csv",header=FALSE,skip=2,sep =",")

#manually replace the Deck.csv file's :s and |s with ,s because i dont know how to make read.table be less bitchy.
test<-read.table("C:/Users/rod/Documents/ward151/Deck.csv",sep=",",fill=NA,skip=1)

DragonID2<-Dragon2[,c(1,3)]
merger2<-cbind(egg2[,c(2,3)],test[match(substr(egg2$possibleRawEggDistribution,1,8),test$V1),c(2:13)])
colnames(merger2)[3]<-"outcomes"
merger2<-merger2[2:length(merger2[,1]),]
merger2$secondDragonIdentifier<-DragonID2$displayName[match(merger2$secondDragonIdentifier,DragonID2$identifier)]
merger2$outcomes<-DragonID2$displayName[match(merger2$outcomes,DragonID2$identifier)]
merger2$V4<-DragonID2$displayName[match(merger2$V4,DragonID2$identifier)]
merger2$V6<-DragonID2$displayName[match(merger2$V6,DragonID2$identifier)]
merger2$V8<-DragonID2$displayName[match(merger2$V8,DragonID2$identifier)]
merger2$V10<-DragonID2$displayName[match(merger2$V10,DragonID2$identifier)]
merger2$V12<-DragonID2$displayName[match(merger2$V12,DragonID2$identifier)]
colnames(merger2)<-c("FirstDragon","SecondDragon","First","FirstChance","Second","SecondChance","Third","ThirdChance","Fourth","FourthChance","Fifth","FifthChance","Sixth","SixthChance")
merger2$FirstDragon<-DragonID2$displayName[match(merger2$FirstDragon,DragonID2$identifier)]
merger2$FirstChance<-as.double(merger2$FirstChance)
merger2$SecondChance<-as.double(merger2$SecondChance)
merger2$ThirdChance<-as.double(merger2$ThirdChance)
merger2$FourthChance<-as.double(merger2$FourthChance)
merger2$FifthChance<-as.double(merger2$FifthChance)
merger2$SixthChance<-as.double(merger2$SixthChance)
merger2$totalchance<-rowSums(merger2[,c(4,6,8,10,12,14)],na.rm=TRUE)
save(list=c("merger2","DragonID2"),file="ShinyBreeddata3.Rdata")
