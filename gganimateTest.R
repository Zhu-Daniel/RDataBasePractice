library(gganimate)
library(tidyverse)
library(SyncMove)

dat <- gazelleRelocations %>% setNames(c("subject", "gender", "time", "x1", "y1")) %>% mutate(x2=lead(x1), y2=lead(y1)) %>% mutate(time=strptime(time, format = "%Y-%m-%d %H:%M:%S") %>% as.POSIXct)

gazgraph <- ggplot(dat)  + geom_path(aes(x = x1, y = y1, colour = subject), arrow=arrow(length = unit(5, "points")), alpha=0.7) 
gazgraph <- gazgraph + facet_wrap(~subject) + theme_bw() + labs(title = "Mongolian Gazelle Movement", subtitle = "Based on data from William Fagan's lab", x = "Easting Coordinates", y = "Northing Coordinates", caption = "Reference point for the coordinates is unknown.") + transition_reveal(along=time)
animate(gazgraph)
