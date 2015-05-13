DSDF$value[DSDF$variable=="upgradeCost"]<-as.integer(as.character(substr(DSDF$value[DSDF$variable=="upgradeCost"],6,11)))
DSDF$value<-as.integer(as.character(DSDF$value))