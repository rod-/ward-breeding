## beta function to build you the shortest path to a dragon of interest
load("ShinyBreeddata.Rdata")
DragonID <- DragonID[c(8:75), ]
DragonID$fragments <- c(1, 1, 1, 1, 1, 1, 1, 1, 8, 0, 1, 1, 1, 1, 5, 1, 5, 5, 1, 1, 5, 5, 12, 0, 8, 5, 5, 5, 8, 16, 5, 8, 16, 16, 5, 5, 8, 0, 20, 48, 48, 1, 20, 12, 60, 48, 20, 20, 48, 20, 48, 48, 20, 0, 40, 40, 
    40, 40, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96)
pathtoX <- function(ownedlist, dragonx, othernewvalue, skipme = NULL) {
    # First find the objective 'best' way to get X
    whobreedsx <- function(ownedlist, dragonx, owned = FALSE, skiplist = NULL) {
        if (is.null(dragonx)) {
            return(0)
        }
        if (is.null(ownedlist)) {
            return(0)
        }
        load("ShinyBreeddata.Rdata")
        wlist <- merger
        if (owned == TRUE) {
            wlist <- wlist[ownedlist %in% merger$FirstDragon]  #make sure you have both of the breedingpair
            wlist <- wlist[ownedlist %in% wlist$SecondDragon]
        }
        # dont use self
        wlist <- wlist[wlist$FirstDragon != dragonx, ]
        wlist <- wlist[wlist$SecondDragon != dragonx, ]
        wlist <- subset(wlist, First == dragonx | Second == dragonx | Third == dragonx | Fourth == dragonx | Fifth == dragonx | Sixth == dragonx)
        if (length(wlist) == 0) {
            return(0)
        }
        if (!is.null(skiplist)) {
            wlist <- wlist[!(wlist$FirstDragon %in% skiplist), ]
            wlist <- wlist[!(wlist$SecondDragon %in% skiplist), ]
        }
        suppressWarnings(wlist[is.na(wlist)] <- 0)  #fixes the numerics
        wlist[is.na(wlist)] <- "Draco"  #fixes the factors.  Makes it weird with draco but whatever
        wlist$DesiredOdds <- (wlist$FirstChance/wlist$totalchance * (wlist$First == dragonx)) + (wlist$SecondChance/wlist$totalchance * (wlist$Second == dragonx)) + (wlist$ThirdChance/wlist$totalchance * (wlist$Third == 
            dragonx)) + (wlist$FourthChance/wlist$totalchance * (wlist$Fourth == dragonx)) + (wlist$FifthChance/wlist$totalchance * (wlist$Fifth == dragonx)) + (wlist$SixthChance/wlist$totalchance * (wlist$Sixth == 
            dragonx))
        return(wlist[order(wlist$DesiredOdds, decreasing = TRUE), ])
    }
    bestpair <- whobreedsx(ownedlist, dragonx, skiplist = skipme)
    retpair <- c(bestpair[1, 1], bestpair[1, 2])
    rate <- bestpair[1, 16]
    if (all(retpair %in% as.integer(ownedlist))) {
        frags <- DragonID$fragments[match(dragonx, DragonID$displayName)]
        outcost = (1/rate) * 20 * frags
    }
    return(c(retpair, outcost))
}  # I would prefer to return the coin cost as well - basically ([,16]/20)*numfrags but the numfrags data is in need of a tweak
## output is currently as a string of numbers - which i'm happy to use as a motivation to start using pictures instead of boring text. Also need to start with the logic for unowned guys - and tracing the tree
## ever backwards... And using suboptimal (or equally-optimal) options compared to just #1 
