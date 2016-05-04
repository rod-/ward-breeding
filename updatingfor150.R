#Update the merger2 database and stuff
library("shiny")
setwd("C:/Users/rod/Documents/ward224/")
Dragon2<-read.csv("Dragon.csv")
Deck<-read.csv("Deck.csv")
egg2<-read.csv("DragonEgg.csv")
#test<-read.csv("Deck.csv",header=FALSE,skip=2,sep =",")
#manually replace the Deck.csv file's :s and |s with ,s because i dont know how to make read.table be less bitchy.
test<-read.table("Deck.csv",sep=",",fill=NA)

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
merger2$FirstChance<-as.double(as.character(merger2$FirstChance))
merger2$SecondChance<-as.double(as.character(merger2$SecondChance))
merger2$ThirdChance<-as.double(as.character(merger2$ThirdChance))
merger2$FourthChance<-as.double(as.character(merger2$FourthChance))
merger2$FifthChance<-as.double(as.character(merger2$FifthChance))
merger2$SixthChance<-as.double(as.character(merger2$SixthChance))
merger2$totalchance<-rowSums(merger2[,c(4,6,8,10,12,14)],na.rm=TRUE)
#clear NAs
merger2<-merger2[-grep("NA",rownames(merger2)),]
setwd("/Users/rod/Documents/ward/shinybreed/ward-breeding/")
#for 220 only

save(list=c("merger2","DragonID2"),file="ShinyBreeddata220.Rdata")
#need to update the levelerdata as well

BuilderUpgrades<-read.csv("BuilderUpgrades.csv")
exp<-read.csv("Level.csv")
StorageUpgrades<-read.csv("StorageUpgrades.csv")
half<-read.csv("ArcherTowerUpgrades.csv")
half<-half[2:36,]
save(list=c("exp","StorageUpgrades","half","BuilderUpgrades"),file="levelerdata220.rData")
