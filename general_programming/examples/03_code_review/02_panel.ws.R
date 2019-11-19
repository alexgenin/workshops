# This function plots moving average with some stuff around it (errbars)
panel.ws <- function(x,y,groups=NULL,subscripts,npts=200,
                     wdw=(max(x)-min(x))/20,col='#0000FF',vlines=NULL,...) {
  
  if ( is.null(groups) ) {
    # setup initial plot, and add the raw data
    panel.xyplot(x,y, groups=groups, subscripts=subscripts, 
                pch=20,col='#00000020')
                
    dat <- .panel.ws.wdw(x,y,npts,wdw,...)
    
    # plot moving average
    panel.xyplot(dat$sref,dat$smeans,subscripts=subscripts,
                type='l')
    # plot sds
    colsd=.transp(col,.5)
    panel.xyplot(dat$sref,dat$smeans+dat$ssds,subscripts=subscripts,
                type='l',lty='dashed')
    panel.xyplot(dat$sref,dat$smeans-dat$ssds,subscripts=subscripts,
                type='l',lty='dashed')
                
    # plot vertical lines
    for (i in 1:length(vlines)) {
      linecols=c('#FF0000','#0000FF','#CCCC00','#00FF00')
      panel.abline(v=vlines[i],col=linecols[i])
    }
    
  } else { # we do have groups, then call panel.superpose
    cat('Not implemented yet\n')
  }
}

# Computes means and sds given wdw
.panel.ws.wdw <- function(x,y,npts,wdw,na.rm=TRUE,...) {
  # Compute moving average and error bars
  means <- rep(0,npts)
  sds <- rep(0,npts)
  refvals <- seq(min(x)-wdw/2,max(x)+wdw/2,length.out=npts);
  # for each point
  for (ix in 1:length(refvals)) {
    refval <- refvals[ix]
    xvals <- abs(x - refval) < wdw/2; # get x values
    if (any(xvals)) {
      # get mean and sds
      means[ix] <- mean(y[xvals],na.rm=na.rm,...)
      sds[ix] <- sd(y[xvals],na.rm=na.rm,...)
    } else { # no values available
      means[ix] <- NA
      sds[ix] <- NA
    }
  }
  
  # probably unnecessary
  sref <- sort(refvals)
  smeans <- means[order(refvals)]
  ssds <- sds[order(refvals)]
  
  return(list(smeans=smeans,ssds=ssds,sref=refvals))
}

# Makes a color transparent 
.transp <- function(col,alpha=.5) {
  charalpha <- as.character(round(alpha*100))
  paste(col,charalpha,sep='')
}
