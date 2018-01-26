## Combine date and time into one column and arrange df by this new variable
W = Traffic_W %>% 
  unite(DateTime, Date, Time, sep = " ") %>% 
  arrange(as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"))

## Create a second data frame containing unique events (since original data set
## can have multiple observations referring to the same person)

Single = distinct(W, DateTime, .keep_all = T)

## Creat another data frame that only accounts for Accidents

