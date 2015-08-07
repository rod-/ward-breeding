## New datatable for towers

#Step 1: Load all the tower txt files (make this a function of a directory so i can just wash/repeat w/version updates)
loadtowertext<-function(directory){
  a<-read.csv("ArcherTowerUpgrades.csv",colClasses="character")
    s<-read.csv("StormTowerUpgrades.csv",colClasses="character")
    l<-read.csv("LightningTowerUpgrades.csv",colClasses="character")
    c<-read.csv("CannonTowerUpgrades.csv",colClasses="character")
    t<-read.csv("TrebuchetUpgrades.csv",colClasses="character")
    r<-read.csv("MageTowerUpgrades.csv",colClasses="character")
    b<-read.csv("MageTowerBlueUpgrades.csv",colClasses="character")
    p<-read.csv("BallistaTowerUpgrades.csv",colClasses="character")
}#initial dir is ward5
#Step2:  melt all this data together into a single molten glob
library(reshape2)
#have to keep the mage towers separate
half<-data.frame(rbind(b[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","powerLevel")],r[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","powerLevel")]))
                 #other half
others<-data.frame(rbind(s[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")],a[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")],
                         l[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")],c[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")],
                         t[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")],p[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","specialAttackPower","attacksPerSecond","attackPower","powerLevel")]))
save(half,others,file = "TowerStats.rData")
load("TowerStats.rData")
#transfer files to/from locations

#fix the "piercing" and "experience" fields, and convert to numeric
others$upgradeCost<-as.numeric(substr(others$upgradeCost,10,16))
others$upgradeReward<-as.numeric(substr(others$upgradeReward,12,17))
half$upgradeCost<-as.numeric(substr(half$upgradeCost,10,16))
half$upgradeReward<-as.numeric(substr(half$upgradeReward,12,17))
#clean up the identifier
half$identifier<-substr(half$identifier,1,nchar(half$identifier)-2)
half$identifier<-(gsub(pattern = "Upgrades",replacement="",x = half$identifier))
half$identifier<-(gsub(pattern = "Tower",replacement="",x = half$identifier))
half$identifier<-as.factor(gsub(pattern = "_",replacement="",x = half$identifier))
others$identifier<-substr(others$identifier,1,nchar(others$identifier)-2)
others$identifier<-(gsub(pattern = "Upgrades",replacement="",x = others$identifier))
others$identifier<-(gsub(pattern = "Tower",replacement="",x = others$identifier))
others$identifier<-as.factor(gsub(pattern = "_",replacement="",x = others$identifier))
#make everything numeric
others$level<-as.numeric(others$level)
others$upgradeTimeInSeconds<-as.numeric(others$upgradeTimeInSeconds)
others$HP<-as.numeric(others$HP)
others$attacksPerSecond<-as.numeric(others$attacksPerSecond)
others$attackPower<-as.numeric(others$attackPower)
others$powerLevel<-as.numeric(others$powerLevel)
half$level<-as.numeric(half$level)
half$upgradeTimeInSeconds<-as.numeric(half$upgradeTimeInSeconds)
half$HP<-as.numeric(half$HP)
half$powerLevel<-as.numeric(half$powerLevel)
#make half and others into bindable dataframes
half$attacksPerSecond<-0
half$attackPower<-0
others$specialAttackPower<-NA #temporary
half<-half[,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel","specialAttackPower")]
# do it
all<-rbind(half,others)
# Step3: recast it Type ~ hp+time+cost+etc

# Step4: Visualization options: Just a generic reactive interface where you can plot any 2 variables against all types or the ratio of any two types against all variables?

# Step4: The interesting analysis parts Make a 'base-building' function.  Questions: What is the DPS of a base that consists of 5 max towers vs one that has the same exp split across 10?  'Level at time T' based
# on building in way 1 vs way 2.