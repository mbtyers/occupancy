data_increment <- 5 # minutes
plot_increment <- 6 # observations, set to FALSE to suppress


library(tidyverse)

thefiles <- list.files("C:\\Users\\mbtyers\\Documents\\misc\\occupancy\\",
                       pattern="*.csv", full.names = TRUE)
alldata <- lapply(thefiles, read_csv) %>% 
  do.call(rbind, .) %>%
  filter(!is.na(number))

dow <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
theplot <- alldata %>%
  mutate(dec_hr=as.numeric(format(time, "%H")) + as.numeric(format(time, "%M"))/60) %>%
  mutate(weekday=factor(dow[as.numeric(format(time, "%u"))],levels=dow)) %>%
  mutate(day=format(time, "%D")) %>%
  # group_by(day) %>%
  ggplot(aes(x=dec_hr, y=number, col=weekday, group=day)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=1:24) +
  labs(x="", y="") +
  theme_bw()




filename <- paste0("C:\\Users\\mbtyers\\Documents\\misc\\occupancy\\occupancy_", 
                   Sys.Date(), ".csv")

fdsa <- readLines("https://portal.rockgympro.com/portal/public/deab289162b917c806673d2ffaea4e47/occupancy?&iframeid=occupancyCounter&fId=")
whichline <- which(fdsa=="    'subLabel' : 'Current climber count',")-1
number <- readr::parse_number(fdsa[whichline])
time <- Sys.time()

i_data <- 1
i_plot <- 1
while(as.numeric(format(time[length(time)], "%H")) < 22) {
  i_data <- i_data+1
  i_plot <- i_plot+1
  Sys.sleep(60*data_increment)
  
  fdsa <- readLines("https://portal.rockgympro.com/portal/public/deab289162b917c806673d2ffaea4e47/occupancy?&iframeid=occupancyCounter&fId=")
  whichline <- which(fdsa=="    'subLabel' : 'Current climber count',")-1
  number[i_data] <- readr::parse_number(fdsa[whichline])
  time[i_data] <- Sys.time()
  
  thedata <- data.frame(time, number)
  write.csv(thedata, file=filename)
  
  if(plot_increment) {
    cat(i_plot, "of", plot_increment, "-", number[i_data], "\n")
    if(i_plot >= plot_increment) {
      # plot(thedata, las=2)# ylim=range(0,150,number), 
      i_plot <- 0
      
      thedata_plot <- thedata %>%
        mutate(dec_hr=as.numeric(format(time, "%H")) + as.numeric(format(time, "%M"))/60) %>%
        mutate(weekday="today")
      
      print(
      theplot +
        geom_point(data=thedata_plot,
                  mapping=aes(x=dec_hr, y=number, group=weekday),#, col=weekday
                  size=2,col=1))
    }
  }
  
}

help(format.POSIXct)
