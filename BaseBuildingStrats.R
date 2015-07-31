## New datatable for towers


# Step 1: Load all the tower txt files (make this a function of a directory so i can just wash/repeat w/version updates)
loadtowertext <- function(directory) {
    a <- read.csv("Arrow...")
    s <- read.csv("Storm...")
    l <- read.csv("Lightning...")
    c <- read.csv("Cannon...")
    t <- read.csv("Trebuchet..")
    r <- read.csv("MageTower...")
    b <- read.csv("BlueMage...")
    p <- read.csv("Ballista")
}
# Step2: melt all this data together into a single molten glob

# Step3: recast it Type ~ hp+time+cost+etc

# Step4: Visualization options: Just a generic reactive interface where you can plot any 2 variables against all types or the ratio of any two types against all variables?

# Step4: The interesting analysis parts Make a 'base-building' function.  Questions: What is the DPS of a base that consists of 5 max towers vs one that has the same exp split across 10?  'Level at time T' based
# on building in way 1 vs way 2. 
