W = unite(Traffic_Violations, DateTime, Date, Time, sep = " ")

W = arrange(W, as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"))

Single = distinct(W, DateTime, .keep_all = T)