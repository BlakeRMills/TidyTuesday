library(dplyr)
library(cowplot)
library(plyr)
library(showtext)
library(forcats)
library(ggplot2)
library(stringr)
library(sysfonts)

#Data
Lem <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
Tax <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv')

#Cleaning
LemBase <- Lem %>% select(taxon, dlc_id, age_at_death_y, birth_type, n_known_offspring) %>% 
  filter(is.na(taxon)==FALSE) %>%
  mutate(taxon = revalue(taxon, c("CMED" = "CMEAD")),
         birth_type = case_when(birth_type == "CB" ~ "Captive Born",
                                birth_type == "Unk" ~ "Birth Unknown",
                                birth_type == "WB" ~ "Wild Born")) %>%
  left_join(., Tax, by="taxon") %>% 
  mutate(common_name = str_to_title(common_name) %>% str_remove(" Lemur")) %>%
  distinct() 

#Death Data frame
LemDeathFinal <- LemBase %>% filter(is.na(age_at_death_y)==FALSE) %>%
  dplyr::group_by(common_name) %>% dplyr::summarise(mean=mean(age_at_death_y)) %>% 
  left_join(., LemBase %>% filter(is.na(age_at_death_y)==FALSE), by="common_name") %>%
  mutate(mean = round(mean, 1),
         label = sprintf('%.1f', mean))

#Birth Data frame
LemBirthFinal <- LemBase %>% filter(is.na(n_known_offspring)==FALSE) %>%
  dplyr::group_by(common_name) %>% dplyr::summarise(mean=mean(n_known_offspring)) %>% 
  left_join(., LemBase %>% filter(is.na(n_known_offspring)==FALSE), by="common_name") %>%
  mutate(mean = round(mean, 1),
         label = sprintf('%.1f', mean))

#Aesthetic Choices
font_add_google("Noto Sans JP")
showtext_auto()
Background <- "#fefaf2"
PointColors <- c("#d8ad59", "#85D4E3", "#f4b5c8")

#Death Plot
DeathPlot <- ggplot(LemDeathFinal, aes(x=fct_reorder(common_name, desc(mean)), y=mean)) +
  geom_point(aes(y=age_at_death_y, color=birth_type), size=5, alpha=0.75) +
  scale_color_manual(values= PointColors) +
  geom_point(size=12.5, color="#9c964a") +
  coord_flip() +
  geom_text(aes(label= label, family="Noto Sans JP", fontface="bold"), size=11.5, color="white") + 
  labs(x="", y="", color="Birth Location") +
  theme(axis.text = element_text(family="Noto Sans JP", size=40),
        axis.title = element_text(family="Noto Sans JP", size=50, face="bold"),
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill=Background, color=Background),
        line = element_blank()) 

#Standardize Levels for Birth Plot
Levels <- ggplot_build(DeathPlot)$layout$panel_params[[1]]$y$get_labels()

#Birth Plot
BirthPlot <- ggplot(LemBirthFinal, aes(x=factor(common_name, levels=Levels), y=mean)) +
  geom_point(aes(y=n_known_offspring, color=birth_type), size=5, alpha=0.75) +
  scale_color_manual(values= PointColors) +
  geom_point(size=12.5, color="#9C964A") +
  coord_flip() +
  geom_text(aes(label= label, family="Noto Sans JP", fontface="bold"), size=11.5, color="white") + 
  labs(x="", y="", color="Birth Location") +
  theme(axis.text = element_text(family="Noto Sans JP", size=40),
        axis.title = element_text(family="Noto Sans JP", size=50, face="bold"),
        panel.background = element_rect(fill=Background),
        plot.background = element_rect(fill=Background, color=Background),
        line = element_blank())

#Birth and Death Plot
Main <- plot_grid(DeathPlot + theme(legend.position = "none"), BirthPlot + theme(legend.position = "none"))

#Main Title
Title <- ggplot() + ggtitle("Life and Death of Lemurs") +
  theme_void() +
  theme(plot.title = element_text(colour = "grey20", family = "Noto Sans JP", face = "bold", size = 120, hjust = 0.5),
        plot.background = element_rect(fill=Background, color=Background))

#Puts Main plot with title
PlotTitle <- plot_grid(Title, Main, ncol=1, rel_heights = c(0.1, 1)) +
  theme(plot.background = element_rect(fill=Background, color=Background),
        plot.margin = unit(c(0,0,2,0),"cm")) #add a buffer at the bottom for labelms

#Custom Annotation
ggdraw(PlotTitle) +
  draw_line(x=c(0, 0.325), y=c(0.98, 0.98), color = "grey20", size=2) +
  draw_line(x=c(0.675, 1), y=c(0.98, 0.98), color = "grey20", size=2) +
  draw_label("life span (in years) and average number of offspring for", x=0.358, y=0.93, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Average", x=0.1475, y=0.93, size=60, fontfamily ="Noto Sans JP", color="#9c964a", fontface = "bold") +
  draw_label("Captive Born", x=0.5825, y=0.93, size=60, fontfamily ="Noto Sans JP", color="#85D4E3", fontface = "bold") +
  draw_label(",", x=0.628, y=0.93, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Wild Born", x=0.664, y=0.93, size=60, fontfamily ="Noto Sans JP", color="#f4b5c8", fontface = "bold") +
  draw_label(",", x=0.6976, y=0.93, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("and", x=0.714, y=0.93, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Unknown Origin", x=0.782, y=0.93, size=60, fontfamily ="Noto Sans JP", color="#d8ad59", fontface = "bold") +
  draw_label("Lemurs", x=0.863, y=0.93, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Lifespan in Years", x=0.30, y=0.055, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Number of Offspring", x=0.8, y=0.055, size=60, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") +
  draw_label("Twitter: @BlakeRobMills | Source: Nature Zehr et. al 2014 | GitHub: BlakeRMills", x=0.5, y=0.015, size=40, fontfamily ="Noto Sans JP", color="grey35", fontface = "bold") 

ggsave("~/Desktop/Lemur2.png", device = "png", width = 20, height = 14)

