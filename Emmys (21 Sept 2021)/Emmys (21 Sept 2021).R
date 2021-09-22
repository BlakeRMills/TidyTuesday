nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

library(tidyverse)
library(cowplot)
library(sysfonts)
library(showtext)

NomRec <- nominees %>% filter(year >=2015, grepl("drama |comed", tolower(category))==TRUE,
                              grepl("comedy or drama", tolower(category))==FALSE,
                              distributor %in% c("HBO", "Netflix", "Hulu", "FX Networks", "NBC")) %>% 
  distinct(category, distributor, year, type, title, .keep_all = TRUE) %>%
  mutate(Genre = ifelse(grepl("dram", tolower(category))==TRUE, "Drama", "Comedy"),
         color = paste(type, year),
         year = factor(year)) %>%
  select(Genre, distributor, type, year, color) 


#Aesthetics 
font_add_google("Josefin Sans")
showtext_auto()
titlefont <- "Josefin Sans"

#Drama Plots
HBODrama <- ggplot(data=NomRec %>% filter(distributor=="HBO", Genre=="Drama"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "up", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffe287", "#ffc192", "#c87e96", "#8f739f", "#2369ab", "#1748b6")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2016, 2018, 2020)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, family = mainfont),
        legend.position = "none",
        plot.margin = unit(c(0,2,2,2),"cm"))

HuluDrama <- ggplot(data=NomRec %>% filter(distributor=="Hulu", Genre=="Drama"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "up", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#f58e7d", "#c87e96", "#8f739f")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2018, 2020)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, family = mainfont),
        legend.position = "none",
        plot.margin = unit(c(0,2,2,2),"cm"))

NetflixDrama <- ggplot(data=NomRec %>% filter(distributor=="Netflix", Genre=="Drama"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "up", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2016, 2018, 2020)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, family = mainfont),
        legend.position = "none",
        plot.margin = unit(c(0,2,2,2),"cm"))

NBCDrama <- ggplot(data=NomRec %>% filter(distributor=="NBC", Genre=="Drama"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "up", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#f58e7d", "#c87e96", "#2369ab")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2016, 2018, 2020)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, family = mainfont),
        legend.position = "none",
        plot.margin = unit(c(0,2,2,2),"cm"))

FXDrama <- ggplot(data=NomRec %>% filter(distributor=="FX Networks", Genre=="Drama"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "up", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffe287", "#ffc192",  "#c87e96", "#8f739f")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2016, 2018, 2020)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, family = mainfont),
        legend.position = "none",
        plot.margin = unit(c(0,2,2,2),"cm"))

#Comedy Plots
FXComedy <- ggplot(data=NomRec %>% filter(distributor=="FX Networks", Genre=="Comedy"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "down", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287",  "#f58e7d", "#c87e96", "#2369ab", 
                               "#ffc192", "#f58e7d", "#c87e96")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2015, 2017)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, vjust=110),
        legend.position = "none",
        plot.margin = unit(c(0,2,0,2),"cm"))

HuluComedy <- ggplot(data=NomRec %>% filter(distributor=="Hulu", Genre=="Comedy"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "down", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#8f739f", "#2369ab", "#1748b6")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2019, 2021)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, vjust=110),
        legend.position = "none",
        plot.margin = unit(c(0,2,0,2),"cm"))

NetflixComedy <- ggplot(data=NomRec %>% filter(distributor=="Netflix", Genre=="Comedy"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "down", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffc192", "#f58e7d", "#c87e96", "#8f739f")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2015, 2017, 2019, 2021)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, vjust=110),
        legend.position = "none",
        plot.margin = unit(c(0,2,0,2),"cm"))

HBOComedy <- ggplot(data=NomRec %>% filter(distributor=="HBO", Genre=="Comedy"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "down", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2015, 2017, 2019, 2021)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, vjust=110),
        legend.position = "none",
        plot.margin = unit(c(0,2,0,2),"cm"))

NBCComedy<- ggplot(data=NomRec %>% filter(distributor=="NBC", Genre=="Comedy"), aes(x=year)) +
  geom_dotplot(binwidth = 1, aes(fill=color, alpha = type), stackdir = "down", method="histodot",
               stackgroups = TRUE, color="grey70", stackratio = 0.75, dotsize = .6) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("#ffe287", "#ffc192", "#f58e7d", "#c87e96", "#8f739f", "#2369ab", "#1748b6",
                               "#ffc192", "#f58e7d", "#c87e96", "#2369ab", "#1748b6")) +
  theme_void() +
  scale_x_discrete(drop = FALSE, breaks= c(2015, 2017, 2019, 2021)) +
  theme(panel.background = element_rect(fill="grey15", color="grey15"),
        plot.background =  element_rect(fill="grey15", color="grey15"),
        axis.text.x = element_text(color="grey75", size=130, vjust=110),
        legend.position = "none",
        plot.margin = unit(c(0,2,0,2),"cm"))

#Title
Title <- ggplot() + ggtitle("Emmy Wins and Nominations by Genre") + 
  theme_void() + 
  theme(plot.title = element_text(hjust= 0.5, size=450, color="white",face = "bold", family = titlefont),
        plot.background = element_rect(fill="grey15", color = "grey15"),
        plot.margin = unit(c(1,0,2,0),"cm"))

#Each Plot
Drama <- plot_grid(FXDrama, HuluDrama, NetflixDrama, HBODrama, NBCDrama, ncol = 5, nrow=1) +
  theme(plot.background = element_rect(fill="grey15", color="grey15")) +
  annotate(geom="curve", x=0.15, y= 0.4, xend=0.0775, yend = 0.27, curvature= 0.35, size = 5, arrow = arrow(length = unit(6, "mm")), color="#f58e7d") +
  annotate(geom="curve", x=0.21, y= 0.63, xend=0.278, yend = 0.42, curvature= -0.35, size = 5, arrow = arrow(length = unit(6, "mm")), color="#f58e7d")

Comedy <- plot_grid(FXComedy, HuluComedy, NetflixComedy, HBOComedy, NBCComedy, ncol = 5, nrow=1) +
  theme(plot.background = element_rect(fill="grey15", color="grey15"),
        plot.margin = unit(c(2,0,-75,0),"cm")) 

#All Together
Main <- plot_grid(Title, Drama, Comedy, ncol = 1, rel_heights = c(0.1, 1, 1)) +
  theme(plot.margin = unit(c(2,0,0,0),"cm"))


ggdraw(Main) +
  draw_label("Twitter: @BlakeRobMills | Source: Emmys.com | GitHub: BlakeRMills", x=0.5, y=0.02, size=150, fontfamily = titlefont, color="white", fontface = "bold") + 
  draw_label("Comedy", x=0.012, y=0.15, size=400, fontfamily = titlefont, color="grey90", fontface = "bold", hjust = 0, alpha = 0.5) + 
  draw_label("Drama", x=0.988, y=0.825, size=400, fontfamily = titlefont, color="grey90", fontface = "bold", hjust = 1, alpha = 0.5) + 
  draw_label("FX Networks", x=0.1, y=0.45, size=275, fontfamily = titlefont, color="white", fontface = "bold") +
  draw_label("Hulu", x=0.32, y=0.45, size=275, fontfamily = titlefont, color="white", fontface = "bold") +
  draw_label("Netflix", x=0.50, y=0.45, size=275, fontfamily = titlefont, color="white", fontface = "bold") +
  draw_label("HBO", x=0.7, y=0.45, size=275, fontfamily = titlefont, color="white", fontface = "bold") +
  draw_label("NBC", x=0.9, y=0.45, size=275, fontfamily = titlefont, color="white", fontface = "bold") +
  draw_label("Transparent dots", x=0.2, y=0.66, size=130, fontfamily = titlefont, color="#f58e7d") +
  draw_label("show nominations", x=0.2, y=0.64, size=130, fontfamily = titlefont, color="#f58e7d") +
  draw_label("Opaque dots", x=0.17, y=0.79, size=130, fontfamily = titlefont, color="#f58e7d") +
  draw_label("show wins", x=0.17, y=0.77, size=130, fontfamily = titlefont, color="#f58e7d") +
  draw_label("Total Emmy Wins and Nominations for Dramas (top) and Comedies (Bottom) of Most Awarded Networks", x=0.5, y=0.9, size=200, fontfamily = titlefont, color="grey85") 

ggsave("~/Desktop/dot.png", height = 35, width = 49)
