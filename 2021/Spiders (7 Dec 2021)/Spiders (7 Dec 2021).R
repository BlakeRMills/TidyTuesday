spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(stringr)
library(ggstream)
library(cowplot)

# Aes
showtext_auto()
font_add_google("Six Caps")
font_add_google("Fira Sans Extra Condensed")
font1 <- "Six Caps"
font2 <- "Fira Sans Extra Condensed"

# Cleaning and Wrangling
spiders <- spiders %>%
  mutate(country = tolower(distribution) %>% str_replace_all(",", " "),
         Usa = ifelse(grepl("usa", country)==TRUE, "United States",  NA),
         Mexico = ifelse(grepl("mexico", country)==TRUE, "Mexico",  NA),
         China = ifelse(grepl("china", country)==TRUE, "China",  NA),
         Brazil = ifelse(grepl("brazil", country)==TRUE, "Brazil",  NA),
         Australia = ifelse(grepl("australia", country)==TRUE, "Australia",  NA),
         India = ifelse(grepl("india", country)==TRUE, "India",  NA),
         Japan = ifelse(grepl("japan", country)==TRUE, "Japan",  NA)) %>%
  filter(grepl("usa|mexico|china|brazil|australia|india|japan", country)==TRUE,
         family %in% c("Araneidae", "Gnaphosidae", "Theridiidae", "Thomisidae")) %>%
  pivot_longer(c(Usa, Mexico, China, Brazil, Australia, India, Japan)) %>%
  filter(is.na(value)==FALSE) %>%
  select(-name, -country) %>%
  group_by(value, year) %>%
  dplyr::count(family) 


P1 <- ggplot(spiders %>% filter(family=="Araneidae")) +
  geom_stream(aes(x=year, y=n, fill=value), type="mirror") +
  ylim(-50, 50) +
  scale_fill_manual(values= c("#6d2f20", "#b75347", "#df7e66", "#e09351", "#edc775", "#94b594", "#224b5e")) +
  theme_void() + 
  theme(legend.position = "none")

P2 <- ggplot(spiders %>% filter(family=="Gnaphosidae")) +
  geom_stream(aes(x=year, y=n, fill=value), type="mirror") +
  ylim(-50, 50) +
  scale_fill_manual(values= c("#6d2f20", "#b75347", "#df7e66", "#e09351", "#edc775", "#94b594", "#224b5e")) +
  theme_void() +
  theme(legend.position = "none")

P3 <- ggplot(spiders %>% filter(family=="Theridiidae")) +
  geom_stream(aes(x=year, y=n, fill=value), type="mirror") +
  ylim(-50, 50) +
  scale_fill_manual(values= c("#6d2f20", "#b75347", "#df7e66", "#e09351", "#edc775", "#94b594", "#224b5e")) +
  theme_void() +
  theme(legend.position = "none")

P4 <- ggplot(spiders %>% filter(family=="Thomisidae")) +
  geom_stream(aes(x=year, y=n, fill=value), type="mirror") +
  ylim(-50, 50) +
  scale_fill_manual(values= c("#6d2f20", "#b75347", "#df7e66", "#e09351", "#edc775", "#94b594", "#224b5e")) +
  theme_void() +
  theme(legend.position = c(0.5, -0.15),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(0.75, "cm"),
        legend.key.width = unit(3, "cm")) +
  guides(fill = guide_legend(nrow = 1))

ggdraw(plot_grid(P1, P2, P3, P4, ncol=1)) +
  theme(plot.background = element_rect(fill="#fbf7f0", color="#fbf7f0"),
        plot.margin = margin(7, 0, 3.5, 0, "cm")) +
  draw_text(text="Streams show number of spider species identified each year by family in various countries.",
            family=font2, size=55, x=0.5, y=1.095, color="#4f2217") +
  draw_text(text = "Twitter: @BlakeRobMills | Source: World Spider Database | GitHub: BlakeRMills", 
             x=0.5, y=-0.11, color="#4f2217", size=50, family=font2, fontface="bold") +
  draw_text(text = "Spider Streams", x=0.5, y=1.18, fontface="bold", size=300, family=font1, color="#4f2217") +
  draw_text(text="1757", x=0.07, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="1800", x=0.217, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="1850", x=0.382, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="1900", x=0.547, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="1950", x=0.712, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="2000", x=.877, y=1.023, size=65, family=font2, color="#4f2217") +
  draw_text(text="Australia", x=0.1, y=-0.0385, size=52, family=font2, fontface="bold", color="#fbf7f0") +
  draw_text(text="Brazil", x=0.233, y=-0.0385, size=52, family=font2, fontface="bold", color="#fbf7f0") +
  draw_text(text="China", x=0.368, y=-0.0385, size=52, family=font2, fontface="bold", color="grey15") +
  draw_text(text="India", x=0.501, y=-0.0385, size=52, family=font2, fontface="bold", color="grey15") +
  draw_text(text="Japan", x=0.634, y=-0.037, size=52, family=font2, fontface="bold", color="grey15") +
  draw_text(text="Mexico", x=0.768, y=-0.0385, size=52, family=font2, fontface="bold", color="grey15") +
  draw_text(text="USA", x=0.901, y=-0.0385, size=52, family=font2, fontface="bold", color="#fbf7f0") +
  draw_text(text="Araneidae", x=0.055, y=0.92, size=170, family=font1, hjust=0, color="#4f2217") +
  draw_text(text="Gnaphosidae", x=0.055, y=0.67, size=170, family=font1, hjust=0, color="#4f2217") +
  draw_text(text="Theridiidae", x=0.055, y=0.42, size=170, family=font1, hjust=0, color="#4f2217") +
  draw_text(text="Thomisidae", x=0.055, y=0.17, size=170, family=font1, hjust=0, color="#4f2217") 

ggsave("~/Desktop/Spiders.png", height=15, width=10)
  
