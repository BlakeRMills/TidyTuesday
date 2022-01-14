#Data
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

#Libraries
library(tidyverse)
library(showtext)
library(cowplot)
library(glue)
library(ggtext)
library(sysfonts)

#Aesthetics
font_add_google("Righteous")
font_add_google("Cabin")
showtext_auto()

#Filtering
Reign <- billboard %>% left_join(., audio, by="song_id") %>% mutate(week_id = as.Date(week_id, format="%m/%d/%Y")) %>%
  filter(week_id >= "1974-01-01", week_id <= "1982-12-31", tempo > 40)

SumStats <- Reign %>% group_by(week_id) %>% dplyr::summarise(valence = mean(valence, na.rm=TRUE),
                                                             liveness = mean(liveness, na.rm=TRUE),
                                                             danceability = mean(danceability, na.rm=TRUE),
                                                             energy = mean(energy, na.rm=TRUE),
                                                             tempo = mean(tempo, na.rm=TRUE),
                                                             acousticness = mean(acousticness, na.rm=TRUE))

ABBA <- Reign %>% filter(performer.x == "ABBA")

background <- "#2c303a"
BackgroundDots <- "#254e6a"
SubText <- "#c6c6c6"
AveLine <- "#fffcb7"
AbbaDot <- "#c6f6fd"
TitleColor <- "#0ac3df"

#Fonts used
font <- "Righteous"
font2 <- "Cabin"

#Creates a function to make all graphs
graphmaker <- function(vari, title){
  Plot <- ggplot() +
    geom_point(data=Reign, aes(x=week_id, y={{vari}}), color = BackgroundDots, alpha=0.2) +
    geom_line(data=SumStats, aes(x=week_id, y={{vari}}), color = AveLine, size = 1.75, alpha=0.75) +
    geom_point(data=ABBA, aes(x=week_id, y={{vari}}), color = AbbaDot, size=6) +
    theme_void() + 
    scale_y_continuous(breaks=c(as.numeric(Reign %>% summarise(max({{vari}}, na.rm=TRUE))), as.numeric(Reign %>% summarise(min({{vari}}, na.rm=TRUE)))),
                       labels = round(c(as.numeric(Reign %>% summarise(max({{vari}}, na.rm=TRUE))), as.numeric(Reign %>% summarise(min({{vari}}, na.rm=TRUE)))), 2)) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust=0.5, color="white", family = font, size=175, face="bold"),
          plot.background = element_rect(fill=background, color=background),
          axis.text.y = element_text(family=font2, color="white", size=65, vjust=0.5, face = "bold"),
          plot.margin = margin(0, 0.5, 0.75, 0.5, unit = "cm"))
  return(Plot)
}


Acousticness <- graphmaker(acousticness,"Acousticness") +
  scale_x_date(breaks=as.Date(c("1974-02-05", "1982-11-25")), labels = c("1974", "1982")) +
  theme(axis.text.x = element_text(family=font2, color="white", size=75, vjust=0.5, face = "bold"))

Dance <- graphmaker(danceability, "Danceability")

Energy <- graphmaker(energy, "Energy")

Liveness <- graphmaker(liveness,"Liveness")

Tempo <- graphmaker(tempo,"Tempo") 

Valence <- graphmaker(valence, "Valence") +
  scale_x_date(breaks=as.Date(c("1974-02-05", "1982-11-25")), labels = c("1974", "1982")) +
  theme(axis.text.x = element_text(family=font2, color="white", size=75, vjust=0.5, face = "bold"))

Main <- plot_grid(Acousticness, Dance, Energy, Liveness, Tempo, Valence, ncol = 3, nrow=2)

Title <- ggplot() + ggtitle("The Reign of ABBA") + theme_void() +
  theme(plot.title = element_text(size=300, family=font, face="bold", hjust=0.5, color = TitleColor),
        plot.background = element_rect(fill=background, color=background),
        plot.margin = margin(0.25, 0, 1.5, 0, unit = "cm"))

Label <-  ggplot() + ggtitle("Twitter: @BlakeRobMills | Source: Data.World/Billboard/Spotify | GitHub: BlakeRMills") + theme_void() +
  theme(plot.title = element_text(size=75, family=font, hjust=0.5, color= "white"),
        plot.background = element_rect(fill=background, color=background),
        plot.margin = margin(0, 0, 0.4, 0, unit = "cm"))


Overall <- plot_grid(Title, Main, Label, ncol = 1, rel_heights = c(0.15, 1, 0.03))

ggdraw(Overall) +
  draw_line(x=c(0, 0.265), y=c(0.96, 0.96), color = TitleColor, size=2) +
  draw_line(x=c(0.738, 1), y=c(0.96, 0.96), color = TitleColor, size=2) +
  draw_label("Comparison of", x=0.134, y=0.9, size=110, color = SubText, fontfamily = font2) +
  draw_label("ABBA songs in Billboard 100", x=0.321, y=0.9, color = AbbaDot, fontfamily = font2, fontface = "bold", size=110) +
  draw_label("to", x=0.455, y=0.9, color = SubText, fontfamily = font2, size=110) +
  draw_label("Average Billboard 100 songs", x=0.59, y=0.9, color = AveLine, fontfamily = font2, fontface = "bold", size=110) +
  draw_label("by week from 1974 to 1982", x=0.83, y=0.9, color = SubText, fontfamily = font2, size=110) 

ggsave("~/Desktop/ABBA.png", width=25, height=18)

