load("DragonStats.rdata")
DSDF$value[DSDF$variable=="upgradeCost"]<-as.integer(as.character(substr(DSDF$value[DSDF$variable=="upgradeCost"],6,11)))
DSDF$value<-as.integer(as.character(DSDF$value))
maybeok<-dcast(DSDF,var2+level~variable,fun.aggregate = sum)
maybeok$color<-"red"
maybeok$color[maybeok$var2%in%purplelist]<-"purple"
maybeok$color[maybeok$var2%in%bluelist]<-"blue"
maybeok$color[maybeok$var2%in%greenlist]<-"green"
maybeok$color[maybeok$var2%in%orangelist]<-"orange"