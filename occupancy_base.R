data_increment <- 5 # minutes
# plot_increment <- 1 # observations, set to FALSE to suppress


library(tidyverse)

thefiles <- list.files("data\\",
                       pattern="*.csv", full.names = TRUE)

dow <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
alldata <- lapply(thefiles, read_csv) %>% 
  do.call(rbind, .) %>%
  filter(!is.na(number))%>%
  mutate(dec_hr=as.numeric(format(time, "%H")) + as.numeric(format(time, "%M"))/60) %>%
  mutate(weekday=factor(dow[as.numeric(format(time, "%u"))],levels=dow)) %>%
  mutate(day=format(time, "%D")) %>%
  mutate(weekdaynum=as.numeric(weekday))


plot(NA, xlim=range(alldata$dec_hr), ylim=c(0,150), 
     xlab="", ylab="", main="", xaxt="n")
axis(side=1, at=1:24, labels=1:24)
daycols <- adjustcolor(rainbow(7), red.f=.8, green.f=.8, blue.f=.8)
for(dayi in sort(unique(alldata$day))) {
  aa <- subset(alldata, day==dayi)
  points(aa$dec_hr, aa$number, col=daycols[aa$weekdaynum], cex=.5)
  lines(aa$dec_hr, aa$number, col=daycols[aa$weekdaynum])
}
grid()
legend("topleft", col=daycols, lty=1, pch=1, legend=dow)



filename <- paste0("data\\occupancy_", 
                   Sys.Date(), ".csv")

fdsa <- readLines("https://portal.rockgympro.com/portal/public/deab289162b917c806673d2ffaea4e47/occupancy?&iframeid=occupancyCounter&fId=")
whichline <- which(fdsa=="    'subLabel' : 'Current climber count',")-1
number <- readr::parse_number(fdsa[whichline])
time <- Sys.time()

i_data <- 2
while(as.numeric(format(time[length(time)], "%H")) < 22) {
  Sys.sleep(60*data_increment)
  
  # figure out how to trycatch the line below vv - just don't do the stuff below it if the network is down
  fdsa <- suppressWarnings(tryCatch(
    readLines("https://portal.rockgympro.com/portal/public/deab289162b917c806673d2ffaea4e47/occupancy?&iframeid=occupancyCounter&fId="),
    error = function(e) NA))
  if(!is.na(fdsa[1])) {
    whichline <- which(fdsa=="    'subLabel' : 'Current climber count',")-1
    number[i_data] <- readr::parse_number(fdsa[whichline])
    time[i_data] <- Sys.time()
    
    thedata <- data.frame(time, number)
    write.csv(thedata, file=filename)
    
    cat(format(Sys.time(), "%T"), "-", number[i_data], "\n")
    thedata_plot <- thedata %>%
      mutate(dec_hr=as.numeric(format(time, "%H")) + as.numeric(format(time, "%M"))/60) %>%
      mutate(weekday="today")
    points(thedata_plot$dec_hr, thedata_plot$number, pch=16, cex=.7)
    i_data <- i_data+1
  } else {
    cat(format(Sys.time(), "%T"), "-", "retrying in 5 min ...", "\n")
  }
}

help(format.POSIXct)
