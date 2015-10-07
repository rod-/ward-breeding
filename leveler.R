#script to level most efficiently (and count the time)
#Tool to calculate the experience pathways.


#load code from home computer.  Need the following:  Exp, towerstats.  StorageUpgrades. Builderhut.

#input:  36 towers.  Builder level.  Storage level.
#the ability to choose a tower TYPE for each tower is also possible but perhaps not valueable.

#Various optimization strategies:
# Reach level X the fastest.
# Level my towers the highest (and reach level X)

#report-out:  A "script" for which upgrades to build, in order.  A "cost" column and a "total cost" column , both as visuals (number+wood, number+speedups)

#In the "level x the fastest" case, you need to do a check to see if raising your storage is worth it.
#Run the build path algorithm with levels > your storage forbidden for a baseline.
#Run it a second time with storage upgrades done as soon as it matters (eg: at levels 10,12,14,16,19,21,22,23)
#also need data on the builder hut levels.  Relevant levels are 8,9,10,11,13,14,16,17 for 18:25  Include eggs in costs :/ (Search again for wood/food icons, speedups, and eggs)
# for completeness sake:  builder=c(0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,13,14,16,17) storage=c(1,1,1,1,1,2,3,3,4,4,5,5,6,7,8,8,9,10,12,14,16,19,21,22,23)

leveler<-function(mybase,builder,storage,strategy="highest",plevel=1,goal=84){
    #set up the goals and initial conditions
    totaltime<-0
    totalexp<-exp$requiredXp[goal+2]
    currentexp<-exp$requiredXp[plevel+2]

    #starting base
if(strategy=="fastest"){
    upgradepriority<-order(half$upgradeReward[1:25]/half$upgradeTimeInSeconds[1:25],decreasing = TRUE)
}
else if(strategy=="highest"){
    upgradepriority<-c(25:1)
}
ispossible<-function(builder,storage,plevel){
    pbuilder=c(0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,13,14,16,17)
    pstorage=c(1,1,1,1,1,2,3,3,4,4,5,5,6,7,8,8,9,10,12,14,16,19,21,22,23)
    possibiles<-builder>pbuilder*storage>pstorage
    return(possibiles)
    }

expincrease<-half$upgradeReward[1:25]
timeincrease<-half$upgradeTimeInSeconds[1:25]

while(totalexp<1499097){
for(I in 1:25){
numpriority<-sum(mybase==(upgradepriority[I]-1))
if(numpriority>0){
totalexp<-totalexp+expincrease[upgradepriority[I]]
totaltime<-totaltime+timeincrease[upgradepriority[I]]
mybase[which(mybase==(upgradepriority[I]-1))[1]]<-(upgradepriority[I])
I<-1
}

}
}
tottime<-0
tottime<-tottime+half$upgradeTimeInSeconds[25]*sum(25-acebase>0)
tottime<-tottime+half$upgradeTimeInSeconds[24]*sum(25-acebase>1)
tottime<-tottime+half$upgradeTimeInSeconds[23]*sum(25-acebase>2)
tottime<-tottime+half$upgradeTimeInSeconds[22]*sum(25-acebase>3)
tottime<-tottime+half$upgradeTimeInSeconds[21]*sum(25-acebase>4)
tottime<-tottime+half$upgradeTimeInSeconds[20]*sum(25-acebase>5)
tottime<-tottime+half$upgradeTimeInSeconds[19]*sum(25-acebase>6)
tottime<-tottime+half$upgradeTimeInSeconds[18]*sum(25-acebase>7)
tottime<-tottime+half$upgradeTimeInSeconds[17]*sum(25-acebase>8)
tottime<-tottime+half$upgradeTimeInSeconds[16]*sum(25-acebase>9)
tottime<-tottime+half$upgradeTimeInSeconds[15]*sum(25-acebase>10)
tottime<-tottime+half$upgradeTimeInSeconds[14]*sum(25-acebase>11)

tottime*0.8/60/60/12
}