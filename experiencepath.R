#Tool to calculate the experience pathways.


#load code from home computer.  Need the following:  Exp, towerstats.  StorageUpgrades. Builderhut.

#input:  36 towers.  Builder level.  Storage level.

#Various optimization strategies:
    # Reach level X the fastest.
    # Level my towers the highest (and reach level X)

#report-out:  A "script" for which upgrades to build, in order.  A "cost" column and a "total cost" column , both as visuals (number+wood, number+speedups)

#In the "level x the fastest" case, you need to do a check to see if raising your storage is worth it.
    #Run the build path algorithm with levels > your storage forbidden for a baseline.
    #Run it a second time with storage upgrades done as soon as it matters (eg: at levels 10,12,14,16,19,21,22,23)
    #also need data on the builder hut levels.  Relevant levels are 8,9,10,11,13,14,16,17 for 18:25  Include eggs in costs :/ (Search again for wood/food icons, speedups, and eggs)
    # for completeness sake:  builder=c(0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,13,14,16,17) storage=c(1,1,1,1,1,2,3,3,4,4,5,5,6,7,8,8,9,10,12,14,16,19,21,22,23)
