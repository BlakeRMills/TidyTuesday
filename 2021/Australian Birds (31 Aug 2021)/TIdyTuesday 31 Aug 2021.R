#Libraries
library(sysfonts)
library(cowplot)
library(plyr)
library(forcats)
library(dplyr)
library(showtext)
library(ggplot2)

#Data reading
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

#Find 10 most frequently spotted birds
PopBirds <- aggregate(bird_count ~ bird_type, data=bird_baths, FUN=sum) %>% slice_max(bird_count, n=10)

#Count by Bioregion and Overall
BioCount <- aggregate(bird_count ~ urban_rural + bioregions + bird_type, data=bird_baths, FUN=mean) %>%
  filter(bird_count != 0, bird_type %in% PopBirds$bird_type) %>%
  mutate(bird_type = factor(bird_type, level=(PopBirds$bird_type)))

MainCount <- aggregate(bird_count ~ bird_type + urban_rural, data=bird_baths, FUN=mean) %>%
  filter(bird_type %in% PopBirds$bird_type) %>%
  mutate(bird_type = factor(bird_type),
         label = paste(round(bird_count*100), "%", sep=""))

#Aes
colors <- c("#911d12", "#c6412a", "#e56d2f", "#edb62a", "#63872c", "#244a16", "#18327d", "#0f114f", "#4e006d", "#7e78c2")
background <- "#fefaf2"
font_add_google("Work Sans")
showtext_auto()
  
#Main Plot
Plot <- ggplot(MainCount, aes(x=fct_rev(bird_type), y=bird_count)) +
  geom_point(data=BioCount, aes(x=fct_rev(bird_type), y=bird_count, shape=urban_rural, color=bird_type), alpha=0.35, size=4) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels=c("0%", "25%", "50%", "75%")) +
  geom_point(aes(shape=urban_rural, color=bird_type), size=10.5) +
  scale_color_manual(values = colors) +
  geom_text(aes(label=label), size=9.5, color="white", family="Work Sans", fontface="bold") +
  coord_flip() +
  labs(x="", y="") +
  theme(panel.background = element_rect(fill=background, color=background),
        plot.background = element_rect(fill=background, color=background),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.text = element_text(family = "Work Sans", size=40, face="bold"),
        axis.text.x = element_text(color = "#7e78c2"),
        axis.text.y = element_text(color = "grey25"),
        plot.margin = margin(0, 0, 1.5, 0, unit = "cm"))

#Title
Title <- ggplot() + ggtitle("Australian Birds") + theme_void() + 
  theme(plot.title = element_text(family = "Work Sans", size=100, face="bold", hjust = 0.5, color="#5a120b"))

#Title and Main Plots
Overall <- plot_grid(Title, Plot, ncol = 1, rel_heights = c(0.1, 1)) + 
  theme(plot.background = element_rect(fill=background, color=background))

#Annotations
ggdraw(Overall) +
  draw_line(x=c(0, 0.37), y=c(0.98, 0.98), color = "#5a120b", size=2) +
  draw_line(x=c(0.63, 1), y=c(0.98, 0.98), color = "#5a120b", size=2) +
  draw_label("Average percentage of times the most popular birds are sighted in urban and rural regions", x=0.5, y=0.948, fontfamily = "Work Sans", fontface="bold", size=50, color="#5a120b") +
  draw_label("Percentage of Bird Sighted", x=0.5, y=0.04, fontfamily = "Work Sans", fontface = "bold", size=45, color = "#7e78c2") +
  draw_label("Rural Sightings Average", x=0.30, y=0.9, fontfamily = "Work Sans", size=32, fontface = "bold", color="#911d12") +
  draw_label("Urban Sightings Average", x=0.53, y=0.9, fontfamily = "Work Sans", size=32, fontface = "bold", color="#911d12") +
  draw_label("Individual Bioregion Averages", x=0.7, y=0.9, fontfamily = "Work Sans", size=32, fontface = "bold", color="#911d12", alpha = 0.6) +
  draw_line(x=c(0.312, 0.34), y=c(0.893, 0.87), size=1, color = "#911d12") +
  draw_line(x=c(0.511, 0.48), y=c(0.892, 0.87), size=1, color = "#911d12") +
  draw_line(x=c(0.645, 0.619), y=c(0.892, 0.868), size=1, color = "#911d12", alpha=0.45) +
  draw_label("Twitter: @BlakeRobMills | Source: Cleary et. al 2016 | GitHub: BlakeRMills", x=0.5, y=0.015, size=40, fontfamily ="Work Sans", color="#0f114f", fontface = "bold") 



ggsave("~/Desktop/AustralianBirds.png", height=10, width=14)



