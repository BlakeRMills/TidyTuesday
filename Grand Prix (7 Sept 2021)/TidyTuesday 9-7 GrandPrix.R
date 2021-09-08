#Libraries
library(readr)
library(sysfonts)
library(cowplot)
library(showtext)
library(tidyverse)

#Load Data
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')

#Aesthetics
font_add_google("Yanone Kaffeesatz")
showtext_auto()
font <- "Yanone Kaffeesatz"
background <- "#fcf2de"
TitleColor <- "#de1507"
BlueScale <- c("#1c0a5a", "#0525b1", "#2360af", "#3482c7","#3f9fda", "#439ecc")
axiscolor <- "#ed851c"
subcolor <- "#ed632c"

#Data Frame
MonacoDiff <- results %>% left_join(., races, by= "raceId") %>% 
  filter(name== "Monaco Grand Prix", position != "\\N", positionOrder <= 8) %>% #Filter Monaco and Top 8
  mutate(diff = grid - positionOrder, 
         PositionTitle = factor(positionOrder, labels = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth"))) %>% 
  dplyr::count(diff, PositionTitle)


#Main Plot
Main <- ggplot(data = MonacoDiff, aes(x=fct_rev(PositionTitle), y=diff)) +
  geom_point(aes(size=n, color=diff)) +
  scale_y_continuous(breaks = c(-5, 0, 5, 10, 15)) +
  coord_flip() +
  expand_limits(x=c(1,9))+
  scale_colour_gradientn(colours = BlueScale) +
  scale_size_continuous(range = c(5, 22)) +
  theme_void() + 
  theme(axis.text = element_text(size=100, family = font, color= axiscolor),
        axis.text.y = element_text(hjust=1),
        panel.grid.major.x = element_line(size=1, color="#f5cba2"),
        axis.ticks.x = element_line(size=1, color = axiscolor),
        axis.ticks.length.x=unit(0.25, "cm"),
        plot.background = element_rect(fill=background, color=background),
        legend.position = "none",
        plot.margin = margin(1.25, 0, 2.5, 0.5, unit = "cm")) +
  annotate(geom="curve", x=8.75, y=-1.25, xend=8.35, yend = 0, curvature= -0.35, size = 2, arrow = arrow(length = unit(3, "mm")), color=subcolor) +
  annotate(geom="curve", x=8.55, y= 5.5, xend=8.175, yend = 4, curvature= 0.3, size = 2, arrow = arrow(length = unit(3, "mm")), color=subcolor) +
  annotate(geom="curve", x=6.55, y= -3.25, xend= 6.175, yend = -2.05, curvature= -0.3, size = 2, arrow = arrow(length = unit(3, "mm")), color=subcolor) +
  annotate(geom="rect", xmin=6, xmax=7, ymin=-5.5, ymax=-4.5, fill=background)

#Title
Title <- ggplot() + ggtitle("Monaco Grand Prix Changes from Grid to Final Position") + 
  theme_void() + theme(plot.title = element_text(hjust= 0.5, size=200, color=TitleColor, family=font, face = "bold"),
                       plot.background = element_rect(fill=background, color = background))

#Total and Annotations
ggdraw(plot_grid(Title, Main, ncol = 1, rel_heights = c(0.1, 1))) +
  draw_line(x=c(0, 0.15), y=c(0.991, 0.991), color = TitleColor, size=1.75) +
  draw_line(x=c(0, 0.15), y=c(0.973, 0.973), color = TitleColor, size=1.75) +
  draw_line(x=c(0, 0.15), y=c(0.955, 0.955), color = TitleColor, size=1.75) +
  draw_line(x=c(0.85, 1), y=c(0.991, 0.991), color = TitleColor, size=1.75) +
  draw_line(x=c(0.85, 1), y=c(0.973, 0.973), color = TitleColor, size=1.75) +
  draw_line(x=c(0.85, 1), y=c(0.955, 0.955), color = TitleColor, size=1.75) +
  draw_text("Difference between starting position and finishing position of Top 8 racers in the Monaco Grand Prix from 1950 to 2021",
            family=font, fontface = "bold", color=subcolor, x=0.5, y=0.92, size=120) +
  draw_text("Larger circles indicate", color = subcolor, size=60, x=0.253, y=0.86, family=font, fontface="bold") +
  draw_text("greater frequency", color = subcolor, size=60, x=0.253, y=0.84, family=font, fontface="bold") + 
  draw_text("Negative values indicate", color = subcolor, size=60, x=0.178, y=0.675, family=font, fontface="bold") +
  draw_text("worse finishing placement", color = subcolor, size=60, x=0.178, y=0.655, family=font, fontface="bold") +
  draw_text("than grid placement", color = subcolor, size=60, x=0.178, y=0.635, family=font, fontface="bold") +
  draw_text("Positive values indicate", color = subcolor, size=60, x=0.59, y=0.855, family=font, fontface="bold") +
  draw_text("better finishing placement", color = subcolor, size=60, x=0.59, y=0.835, family=font, fontface="bold") +
  draw_text("than grid placement", color = subcolor, size=60, x=0.59, y=0.815, family=font, fontface="bold") +
  draw_label("Twitter: @BlakeRobMills | Source: Ergast API | GitHub: BlakeRMills", x=0.5, y=0.02, size=75, fontfamily = font, color=TitleColor, fontface = "bold") 

#Save
ggsave("~/Desktop/Bee.png", height = 14, width=20)

