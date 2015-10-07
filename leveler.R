#script to level most efficiently (and count the time)

#starting base

acebase<-c(14,14,12,16,15,14,13,13,15,13,12,16,19,14,21,20,21,21,17,14,20,20,12,12,11,10,10,11,10,10,10,10,9,8)
mybase<-c(21,21,21,19,16,16,21,17,15,15,21,21,21,21,18,21,21,15,16,17,16,16,17,21,21,15,16,16,16,15,17)

acebase<-c(20,18,20,18,18,18,18,18,18,18,18,19,18,19,20,25,25,21,20,20,25,20,020,19,25,18,18,19,18,19,20,18,18,20,20)

upgradepriority<-order(half$upgradeReward[1:25]/half$upgradeTimeInSeconds[1:25],decreasing = TRUE)
expincrease<-half$upgradeReward[1:25]
timeincrease<-half$upgradeTimeInSeconds[1:25]
#go through using upgradepriority and upgrade anything that is priority
totaltime<-0
#totalexp<-546297
totalexp<-935097
#level 23 towers require 21 storage (61) 24 towers require 22(64) and 25 towers require 23(67) those storage upgrades require 11+12+12+13 days
totaltime<-4285440
#23, 25, 10,9,8,24,22,12,11,21,20,13,19,14,15,16,18,17

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
2292/136
