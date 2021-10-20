# Data
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(stringr)

#Wrangling

pumpkins <- pumpkins %>% separate(id, se="-", into = c("Year", "Type")) %>%
  mutate(weight_lbs = str_remove_all(weight_lbs, ",") %>% as.numeric(),
         place = as.numeric(place)) %>%
  filter(is.na(weight_lbs)==FALSE, is.na(Type)==FALSE, place %in% c(1:500), 
         Type == "P") 

#Aesthetics
showtext_auto()
font_add_google("Josefin Sans")
font_add_google("Josefin Slab")
font1 <- "Josefin Sans"
font2 <- "Josefin Slab"
background <- "#4A532B"
violin <- "#BBC49E"
axistext <- "#eabb64"

ggplot(data=pumpkins, aes(x=Year, y=weight_lbs)) +
  geom_violin(color=violin, fill=violin, size=1.25) +
  geom_sina(aes(color=weight_lbs), alpha=0.75, size=2.85) + 
  scale_y_continuous(labels=comma, limits = c(800, 2750)) +
  scale_color_gradientn(colors = rev(c("#f5d78a", "#e09f3e", "#932a2b", "#540b0e"))) +
  coord_flip() +
  theme_void() +
  ggtitle("Giant Pumpkins", subtitle = "Plot displays weight distributions of 500 largest Giant Pumpkins from Great Pumpkin Commonwealth's Weighoff by year") +
  labs(y="Weight (in lbs)", caption = "Twitter: @BlakeRobMills | Source: BigPumpkins.com | GitHub: BlakeRMills") +
  theme(axis.text.x = element_text(color= "#f5d78a", family=font2, size=50, face="bold"),
        axis.title.x = element_text(color= "#f5d78a", family=font1, size=65, vjust=-2, face="bold"),
        plot.title = element_text(family = font1, size=200, hjust=0.5, face="bold", color="#d38d24"),
        plot.subtitle = element_text(family = font2, size=60, hjust=0.5, color="#e5ae50", face="bold", vjust=0.25),
        panel.background = element_rect(fill=background, color=background),
        plot.background = element_rect(fill=background, color=background), 
        legend.position = "none",
        plot.caption = element_text(size=40, hjust=0.5, vjust=-7, family=font1, color="#f9e7b9"),
        plot.margin = margin(0.25,0,1,-0.25, "cm")) +
  annotate(geom = "text", label="2013", x=1, y=840, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2014", x=2, y=1000, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2015", x=3, y=1050, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2016", x=4, y=1055, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2017", x=5, y=1010, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2018", x=6, y=1050, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2019", x=7, y=980, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2020", x=8, y=970, size=25, color=axistext, family=font2, hjust=1, fontface="bold") +
  annotate(geom = "text", label="2021", x=9, y=940, size=25, color=axistext, family=font2, hjust=1, fontface="bold") 

ggsave("~/Desktop/Pumpkins.png", width=15, height = 10.5)
