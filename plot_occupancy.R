library(tidyverse)

thefiles <- list.files("C:\\Users\\mbtyers\\Documents\\misc\\occupancy\\",
                       pattern="*.csv", full.names = TRUE)
alldata <- lapply(thefiles, read_csv) %>% do.call(rbind, .)

dow <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
alldata %>%
  mutate(dec_hr=as.numeric(format(time, "%H")) + as.numeric(format(time, "%M"))/60) %>%
  mutate(weekday=factor(dow[as.numeric(format(time, "%u"))],levels=dow)) %>%
  mutate(day=format(time, "%D")) %>%
  # group_by(day) %>%
  ggplot(aes(x=dec_hr, y=number, col=weekday)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=1:24) +
  labs(x="", y="") +
  theme_bw()

