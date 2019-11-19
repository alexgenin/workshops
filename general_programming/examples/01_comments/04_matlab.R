# 
# Convert a numeric  MATLAB datenum (days since 0000-1-1 00:00) to seconds in 
# the Unix epoch (seconds since 1970-1-1 00:00). Specify a time zone if the 
# input datenum is anything other than the GMT/UTC time zone. 
# credit:  <http://lukemiller.org/index.php/2011/02/converting-
# matlab-and-r-date-and-time-values/>
matlab2POS = function(x,tz = "UTC") {
    require(R.matlab)
    days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
    secs = days * 86400 #86400 seconds in a day
    return(as.POSIXct(secs,origin = "1970-1-1",tz = tz)) #returns POSIXct object
}
