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
others<-data.frame(rbind(s[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")],a[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")],
                         l[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")],c[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")],
                         t[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")],p[-1,c("identifier","level","upgradeCost","upgradeTimeInSeconds","HP","upgradeReward","attacksPerSecond","attackPower","powerLevel")]))
save(half,others,file = "TowerStats.rData")  

# Step3: recast it Type ~ hp+time+cost+etc

# Step4: Visualization options: Just a generic reactive interface where you can plot any 2 variables against all types or the ratio of any two types against all variables?

# Step4: The interesting analysis parts Make a 'base-building' function.  Questions: What is the DPS of a base that consists of 5 max towers vs one that has the same exp split across 10?  'Level at time T' based
# on building in way 1 vs way 2. 