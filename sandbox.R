## The sandbox
library(ggplot2)

# Create a plot
p <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + 
  geom_point() +
  scale_color_discrete(name = "Test") +
  theme_minimal()
p



theme_neyhart <- function(base_size = 12) {
  scale_color_manual(values = palette()) +
    theme_minimal(base_size = base_size)
}


p +   scale_color_manual(values = palette()) +
  theme_minimal(base_size = 14)
