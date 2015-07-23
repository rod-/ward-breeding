#AOrosterchanges

rosterb<-c("ynci","rodminus","chungi82","fatboo","primaites","zyphon","acesoh","lususnaturae","nevyn","strwberrypizza","jutt2222","zhenx","rodplus","moonspawn","bigkeeper","egonzo","icehyunkel","drizztray","lastat","amarok","vanation","chillipeppers","princessjulie","nickademus","dracaryx","brewdoctor","tellerin","snoopchic","aragornblaze","poipoi","drizztlee","meningi","wingslayer","chuckdu","dogecoin","unlimitless","insatiable","queenapps","thegudge","phantasy","rogermartin","baeel","juanmaster","colonelxchilli","bigreddog","evanard","denmiz","mcs117","henryv","colonelchilli")
rosterb[rosterb=="henryv"]<-"thewash"
rosterb[rosterb=="chillipeppers"]<-"arodriguez"
rosterb[rosterb=="baeel"]<-"smokeyou"
rosterb[rosterb=="amarok"]<-"lati"
rosterb[rosterb=="rogermartin"]<-"alfalfa"
rosterb[rosterb=="queenapps"]<-"avatarroku"
rosterb<-c("ynci","rodminus","primaites","zyphon","moonspawn","acesoh","chungi82","fatboo","ironduke","igrimreaper","thewash","nevyn","drizztray","amarok","zhenx","egonzo","strwberrypizza","alfalfa","rodplus","icehyunkel","bigkeeper","drizztlee","lastat","avatarroku","fival","poipoi","nickademus","insatiable","snoopchic","aragornblaze","dracaryx","brewdoctor","princessjulie","traceyv2","arodriguez","lati","wingslayer","meningi","thegudge","tellerin","unlimitless","juanmaster","bigreddog","tasha","dogecoin","sparklefishh","smokeyou","xsilverstormx","xxweathersxx")
#keeps the roster up to date.

#checks individual wars for nonperformers.
warcheck<-function(war,roster=rosterb){return(c(roster[!roster%in%war],war[!war%in%roster]))}

#warlist<-list("war1","war2","war3","war4","war5","war6","war7","war8","war9","war10","war11","war12","wardmata","warberserkers","wargstorm","warsbears","wartoo","wardmata2","warmystix","wareldest","wardmata3","wareldest2","warsbears2","waruntouch","warhounds","warundawn","warsurfer","wareverest","wardravengers","wartides","warmongers","warforder","wartbdragons","wardmata4","adminresponse","waruntouch2","warfear","warsbears4","warfear2","warbears5","wardevils","warhounds2","warnd","warfa","warfa2","warsbears6","warundrag","warunid","warsbears7","warvortex","warrr",
#           "warunid2")
wardf<-list(war1,war2,war3,war4,war5,war6,war7,war8,war9,war10,war11,war12,wardmata,warberserkers,wargstorm,warsbears,wartoo,wardmata2,warmystix,wareldest,wardmata3,wareldest2,warsbears2,waruntouch,warhounds,warundawn,warsurfer,wareverest,wardravengers,wartides,warmongers,warforder,wartbdragons,wardmata4,adminresponse,waruntouch2,warfear,warsbears4,warfear2,warbears5,wardevils,warhounds2,warnd,warfa,warfa2,warsbears6,warundrag,warunid,warsbears7,warvortex,warrr,
                  warunid2,wartherage,wardevils3,warnd2)

checkfn<-function(player,warlist){
  outvec<-NULL
  for(I in 1:length(warlist)){
    outvec<-c(outvec,player%in%warlist[[I]])
  }
  return(outvec)
}
#function to give a vector of war participation across all wars foreach player (so really a matrix led by the name!)

overview<-function(rosterb,warlist){
  fout<-NULL
    for(I in rosterb){
    participation<-checkfn(I,warlist)
#    out<-c(I,participation)
    out<-participation
        fout<-rbind(fout,out)
    }
    fout<-fout*1
    fout<-as.data.frame(fout)
    rownames(fout)<-rosterb
    fout
    }