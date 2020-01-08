library(gganimate)
library(transformr)
library(tidyverse)
library(SyncMove)

#dat <- gazelleRelocations %>% setNames(c("subject", "gender", "time", "x1", "y1")) %>% mutate(x2=lead(x1), y2=lead(y1)) %>% mutate(time=strptime(time, format = "%Y-%m-%d %H:%M:%S") %>% as.POSIXct)
#Attempt to remove any occurences of NA from dataset
dat <- gazelleRelocations %>% setNames(c("subject", "gender", "time", "x1", "y1")) %>% mutate(time=strptime(time, format = "%Y-%m-%d %H:%M:%S") %>% as.POSIXct) %>% mutate(shorttime = as.Date(strptime(time, format = "%Y-%m-%d %H:%M:%S")))

gazgraph <- ggplot(dat)  + 
  geom_path(aes(x = x1, y = y1, colour = subject), arrow=arrow(length = unit(5, "points")), alpha=0.7) 

gazgraph <- gazgraph + facet_wrap(~subject) + theme_bw() + labs(title = "Mongolian Gazelle Movement", subtitle = "Based on data from William Fagan's lab {frame_along}", x = "Easting Coordinates", y = "Northing Coordinates", caption = "Reference point for the coordinates is unknown.") + transition_reveal(along=time)

#gazgraph <- gazgraph + 
#  facet_wrap(~subject) + 
#  theme_bw() + 
#  transition_time(shorttime) +
#  labs(title = "Date: {frame_time}", subtitle = "Based on data from William Fagan's lab", x = "Easting Coordinates", y = "Northing Coordinates", caption = "Reference point for the coordinates is unknown.") +
#  shadow_mark() +
#  ease_aes('cubic-in-out')

#Main problem with including the dynamic text is that the errors seem to indicate that there are NA values within the info. - https://stackoverflow.com/questions/7355187/error-in-if-while-condition-missing-value-where-true-false-needed

animate(gazgraph)



