library(gganimate)
#> Loading required package: ggplot2

# We'll start with a static plot
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()


anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1) + 
  # Slow start and end for a smoother look
  ggtitle('Now showing {closest_state}', subtitle = "Frame {frame} of {nframes}")
anim

