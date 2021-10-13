#Data
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

#Libraries
library(ggstream)
library(tidyverse)
library(stringr)
library(showtext)
library(cowplot)
library(sysfonts)

#Aes
font_add_google("Josefin Sans")
font1 <- "Josefin Sans"
background <- "grey35"
showtext_auto()

#Wrangling and Labeling
codes <- countrycode::codelist %>% select(genc3c, continent) %>% distinct() %>%
  filter(is.na(genc3c) ==FALSE, is.na(continent)==FALSE)

colnames(production) <- c("Entity", "Code", "Year", "Pelagic", "Crustaceans", "Cephalopods", "Demersal", "Freshwater", "Molluscs", "Other Marine")

prod <- production %>% left_join(., codes, by = c("Code" = "genc3c")) %>% 
  pivot_longer(-c(Entity, Year, Code, continent), names_to = "Fish", values_to = "Prod") %>% 
  filter(is.na(continent)==FALSE, is.na(Prod)==FALSE, is.na(Code)==FALSE) 

order <- (rbind(prod %>% filter(continent=="Africa") %>% select(Entity) %>% distinct(),
               prod %>% filter(continent=="Americas") %>% select(Entity) %>% distinct(),
               prod %>% filter(continent=="Asia") %>% select(Entity) %>% distinct(),
               prod %>% filter(continent=="Europe") %>% select(Entity) %>% distinct(),
               prod %>% filter(continent=="Oceania") %>% select(Entity) %>% distinct()))[[1]]

prod <- prod %>% dplyr::mutate(Entity = factor(Entity, levels = order),
                        Entity = str_replace(Entity, " and ", " & "),
                        Entity = str_replace(Entity, "Saint", "St."),
                        Entity = str_remove(Entity, " the "))


#Main Plot
Main <- ggplot(data=prod, aes(x=Year, y=Prod, fill=Fish)) +
  geom_stream(type="proportional") +  
  scale_fill_manual(values= c("#001e54", "#489cd4", "#00b4b5", "#a2f1f2", "#daa494",
                               "#7b2c21", "#4e2012")) +
  ggtitle("Gettin' Fishy", subtitle = "x\n y") + 
  theme_void() +
  scale_x_continuous(breaks = c(1970, 1990, 2010)) +
  theme(legend.position = "none",
        plot.title = element_text(size=1000, face="bold", family=font1, color="grey95", hjust = 0.5),
        plot.subtitle = element_text(size=300, face="bold", family=font1, color="grey35", hjust = 0.5),
        strip.text = element_text(size=115, face="bold", family=font1, color="grey85"),
        plot.background = element_rect(color = background, fill = background),
        strip.background = element_rect(color = background, fill = background),
        panel.background =  element_rect(color = background, fill = background),
        panel.spacing.y = unit(1, "cm"), 
        panel.spacing.x = unit(.1, "cm"),
        axis.text.x = element_text(size=100, color = "grey85", family = font1),
        plot.margin = margin(0.5, 0.75, 2, 0.75, unit = "in")) +
  facet_wrap(~Entity, ncol = 10)

ggdraw(Main) +
  draw_label("Twitter: @BlakeRobMills | Source: OurWorldInData.org | GitHub: BlakeRMills", x=0.5, y=0.015, size=250, fontfamily = font1, color="grey92", fontface = "bold") +
  draw_label("Graphs show country fish production from 1961 to 2013 of fish types by proportion of total", x=0.5, y=0.935, size=275, fontfamily = font1, color="grey85", fontface = "bold") +
  draw_label("Cephalopods", x=0.127, y=0.91, size=275, fontfamily = font1, color="#001e54", fontface = "bold") +
  draw_label("Crustaceans", x=0.276, y=0.91, size=275, fontfamily = font1, color="#489cd4", fontface = "bold") +
  draw_label("Demersal", x=0.405, y=0.91, size=275, fontfamily = font1, color="#00b4b5", fontface = "bold") +
  draw_label("Freshwater", x=0.53, y=0.91, size=275, fontfamily = font1, color="#a2f1f2", fontface = "bold") +
  draw_label("Molluscs", x=0.647, y=0.91, size=275, fontfamily = font1, color="#daa494", fontface = "bold") +
  draw_label("Other Marine", x=0.778, y=0.91, size=275, fontfamily = font1, color="#7b2c21", fontface = "bold") +
  draw_label("Pelagic", x=0.905, y=0.91, size=275, fontfamily = font1, color="#4e2012", fontface = "bold") 
ggsave("~/Desktop/Fish.png", height = 90, width = 55, limitsize = FALSE)
