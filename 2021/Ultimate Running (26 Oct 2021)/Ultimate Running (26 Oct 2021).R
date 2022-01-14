ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Libraries 
library(tidyverse)
library(sysfonts)
library(showtext)
library(ggforce)
library(cowplot)

#Aesthethics
showtext_auto()

font_add_google("Advent Pro")
font_add_google("Fira Sans Extra Condensed")

font1 <- "Advent Pro"
font2 <- "Fira Sans Extra Condensed"


# Wrangling and Cleaning
race <- race %>% filter(is.na(country)==FALSE) %>% 
  mutate(LengthRank = rank(distance*-1, ties.method = "first")) %>% 
  group_by(distance) %>% dplyr::mutate(distanceOrder = distance + seq(0, by=0.002, length.out=n())) %>%
  filter(distance > 170, LengthRank != 1) 


ultra_rankings <- ultra_rankings %>% left_join(., race, by="race_year_id") %>%
  filter(rank == 1, is.na(LengthRank)==FALSE) %>%
  mutate(hours = time_in_seconds/3600,
         diff = abs(elevation_gain) + abs(elevation_loss)) 

#Plots


p1 <- ggplot(data=ultra_rankings) +
  geom_link(aes(x=distanceOrder, xend=distanceOrder, y=0, yend=hours, color=distance), size=1.65, alpha=0.6) +
  geom_point(aes(y=hours, x=distanceOrder, color=distance), size=4.5) +
  scale_color_gradientn(colors = rev(c("#2c2d54", "#434475", "#717287", "#8692a7", "#87bcbd", "#89ab7c", "#6f9954"))) +
  coord_polar(theta = "y", clip="off", start = 4.71) +
  scale_x_continuous(limits = c(166, NA)) +
  scale_y_continuous(limits = c(NA, 48)) +
  theme_void() +
  theme(plot.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        legend.position = "none",
        plot.margin = margin(3, 1, 0.25, 0, "cm")) +
  annotate(geom="text", label="170", x=170, y=0, vjust=1.5, color= "#6f9954", family=font2, size=25) +
  annotate(geom="text", label="171", x=171, y=0, vjust=1.5, color="#89ab7c", family=font2, size=25) +
  annotate(geom="text", label="172", x=172, y=0, vjust=1.5, color="#87bcbd", family=font2, size=25) +
  annotate(geom="text", label="173", x=173, y=0, vjust=1.5, color="#717287", family=font2, size=25) +
  annotate(geom="text", label="174", x=174, y=0, vjust=1.5, color="#434475", family=font2, size=25) +
  annotate(geom="text", label="175", x=175, y=0, vjust=1.5, color="#2c2d54", family=font2, size=25) +
  annotate(geom="text", label="Trail Distance", x=172.25, y=0, vjust=2.7, color="#3d5f3c", family=font2, size=33, fontface="bold") +
  annotate(geom="text", label="(in Km)", x=172.25, y=0, vjust=6.8, color="#3d5f3c", family=font2, size=17.5, fontface="bold") 
  
ggdraw(p1) +
  draw_label(label = "Ultimate Trail Running", x=0.5, y=0.96, color="#263128", fontface="bold", size=275, fontfamily = font1) +
  draw_label(label = "Plot displays finishing times in hours (line length) of first place runners. Trails\nlonger than 170 Km are displayed, and races range between 2012 and 2021", 
             x=0.5, y=0.89, color="#3d5f3c", size=75, fontfamily = font2, lineheight = 0.3) +
  draw_label(label = "Twitter: @BlakeRobMills | Source: International Trail Running Association | GitHub: BlakeRMills", 
             x=0.5, y=0.015, color="#263128", fontface="bold", size=55, fontfamily = font2) +
  draw_label(label = "Petra Muckova is the only woman to\nfinish first in a race longer than 170 Km", x=0.75, y=0.09, color="#5b5a7e", fontface="bold", size=58, fontfamily = font2, lineheight=0.28) +
  draw_label(label = "(173.8 Km ~ 32:06:00)", x=0.75, y=0.061, color="#2a2b52", size=58, fontfamily = font2) +
  draw_label(label = "Clarke McClymont had\nthe fastest time\nfor a 175 Km race", x=0.89, y=0.706, color="#2a2b52", fontface="bold", size=58, fontfamily = font2, lineheight=0.28) + 
  draw_label(label = "(175 Km ~ 20:56:00)", x=0.89, y=0.668, color="#2a2b52", size=58, fontfamily = font2) +
  draw_label(label = "Guillaume Le Normand", x=0.165, y=0.22, color="#2c2d54", fontface="bold", size=58, fontfamily = font2) +
  draw_label(label = "(175 Km ~ 41:54:14)", x=0.165, y=0.2, color="#2c2d54", size=58, fontfamily = font2, lineheight=0.25) +
  draw_label(label = "Tiejun Li", x=0.38, y=0.196, color="#717287", fontface="bold", size=58, fontfamily = font2) +
  draw_label(label = "(172.8 Km ~ 37:53:26)", x=0.38, y=0.176, color="#717287", size=55, fontfamily = font2, lineheight=0.25) +
  draw_label(label = "15 Hrs", x=0.656, y=0.835, color="#2c2d54", size=60, fontfamily = font2, fontface="bold") +
  draw_label(label = "25 Hrs", x=0.898, y=0.414, color="#606072", size=60, fontfamily = font2, fontface="bold") +
  draw_label(label = "35 Hrs", x=0.542, y=0.065, color="#717287", size=60, fontfamily = font2, fontface="bold") +
  draw_line(x=c(0.5368, 0.5387), y=c(0.095, 0.076), size=2, color="#717287") +
  draw_line(x=c(0.8535, 0.875), y=c(0.4165, 0.414), size=2, color="#606072") +
  draw_line(x=c(0.6282, 0.6365), y=c(0.807, 0.8275), size=2, color="#2c2d54") +
  draw_line(x=c(0.822, 0.865), y=c(0.607, 0.655), size=1.5, color="#2a2b52") +
  draw_line(x=c(0.642, 0.72), y=c(0.188, 0.11), size=1.5, color="#5b5a7e") 

ggsave("~/Desktop/UltimateRunning.png", height = 15, width = 15)

