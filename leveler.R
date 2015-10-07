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

#Need to do a weird 'subgoal' script i think to get what i want: which is just calling leveler a few times with different goals

overleveler<-function(mybase,builder,storage,strategy="highest",plevel=1,goal=84,buildtimer=0.8){
 #set subgoals: hit those levels.  add those towers.
    newlevel=plevel
    subgoals<-c(1,1,4,4,4,7,12,12,17,17,21,21,25,28,31,31,34,37,42,46,50,56,61,64,67)#these are the levels where you change max towers
    result<-leveler(mybase,builder,storage,plevel=plevel,goal=subgoals[which(subgoals>plevel)[1]])
    newbase<-unlist(result[4])
    newbuilder<-ispossible(plevel = subgoals[which(subgoals>plevel)[1]])[26]
    newstore<-ispossible(plevel = subgoals[which(subgoals>plevel)[1]])[27]
    newlevel<-subgoals[which(subgoals>newlevel)]
    totalwood<-unlist(result[3])
    totaltime<-unlist(result[2])
    bonustime<-speedupconvert(sum(as.double(as.character(StorageUpgrades$upgradeTimeInSeconds))[(storage:newstore)+1]))+speedupconvert(sum(as.double(as.character(BuilderUpgrades$upgradeTimeInSeconds))[(builder:newbuilder)]))
    #don't add in bonustime until the very last.
    }

leveler<-function(mybase,builder,storage,strategy="highest",plevel=1,goal=84,buildtimer=0.8){
    #load in the initial dataframes
    load("TowerStats.rData")
    load("levelerdata.Rdata")
    #and probably need to fix the towerstats to make them numeric vectors
    half$upgradeCost<-as.double(gsub(pattern = "piercing:",replacement = "",x=half$upgradeCost))
    half$upgradeReward<-as.double(gsub(pattern="experience:",replacement="",x=half$upgradeReward))
            #set up the goals and initial conditions
    totaltime<-c(0,0,0,0,0,0,0)#as speedups:  1 min, 3 min, 15 min, 30 min, 1hr, 3hr, 12hr
    totalwood<-0
    goalexp<-as.integer(as.character(exp$requiredXp[goal+2]))
    currentexp<-as.integer(as.character(exp$requiredXp[plevel+2]))
    expincrease<-half$upgradeReward[1:25]
    timeincrease<-as.double(as.character(half$upgradeTimeInSeconds))[1:25]
    woodcost<-half$upgradeCost[1:25]
    #strategies
if(strategy=="fastest"){
    upgradepriority<-order(half$upgradeReward[1:25]/half$upgradeTimeInSeconds[1:25],decreasing = TRUE)
}
else if(strategy=="highest"){
    upgradepriority<-c(25:1)
}
#converts time (in seconds) into the optimal number of speedups required (preference to high-hour ones!)
maxpossible<-max(which(ispossible(builder,storage,plevel)[1:25]>0))
cupgradepriority<-upgradepriority[upgradepriority<=maxpossible]
outputscript<-vector()

while(currentexp<goalexp){
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
}
return(list(outputscript,totaltime,totalwood,mybase))
#wish to convert the total time into speedups


}
ispossible<-function(builder,storage,plevel){
    pbuilder=c(0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,13,14,16,17)
    pstorage=c(1,1,1,1,1,2,3,3,4,4,5,5,6,7,8,8,9,10,12,14,16,19,21,22,23)
    possibiles<-(builder>=pbuilder)*(storage>=pstorage)
    storagelevels=c(0,7,12,17,21,25,28,31,34,37,40,42,44,46,48,50,52,54,56,58,61,64,67,70,73)
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