# Libraries
library(ggimage)
library(tidyverse)
library(geomtextpath)
library(stringr)
library(showtext)
library(cowplot)
library(MetBrewer)

# Data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>%
  mutate(Breed = str_squish(Breed)) %>% .[, c(1:5, 7, 10:16)]
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>%
  mutate(Breed = str_squish(Breed)) %>% select(Breed, `2020 Rank`)

# Aes
showtext_auto()
font_add_google("Quicksand")
font_add_google("Fira Sans Condensed")

# Wrangling
breed_rank <- breed_traits %>% 
  left_join(breed_rank_all, by="Breed") %>%
  filter(`2020 Rank` <= 25) %>% 
  select(-`2020 Rank`) %>%
  pivot_longer(-Breed) %>% 
  group_by(Breed) %>%
  mutate(Breed = str_squish(Breed),
         xrow = row_number(),
         name = case_when(name== "Affectionate With Family" ~ "Family\nAffection",
                          name== "Good With Young Children" ~ "Good w/\nChildren",
                          name== "Good With Other Dogs" ~ "Good w/\nDogs",
                          name== "Openness To Strangers" ~ "Openness to\nStrangers",
                          name== "Watchdog/Protective Nature" ~ "Protective",
                          TRUE ~ name),
         name = str_remove(name, "Level"), 
         drawing = paste("https://raw.githubusercontent.com/BlakeRMills/TidyTuesday/main/2022/Dog%20Breeds/Drawings/", Breed %>% str_replace_all(" ", "%20"), ".png", sep="") %>% str_replace_all(" ", "%20"),
         Breed = case_when(Breed == "Cavalier King Charles Spaniels" ~ "Cavalier King\nCharles Spaniels",
                           Breed == "Pointers (German Shorthaired)" ~ "Pointers\n(German Shorthaired)",
                           Breed == "German Shepherd Dogs" ~ "German Shepherd",
                           Breed == "Bernese Mountain Dogs" ~ "Bernese\nMountain Dogs",
                           Breed == "Yorkshire Terriers" ~ "Yorkshire\nTerriers",
                           Breed == "Miniature Schnauzers" ~ "Miniature\nSchnauzers",
                           Breed == "Retrievers (Labrador)" ~ "Retrievers\n(Labrador)",
                           Breed == "Doberman Pinschers" ~ "Doberman\nPinschers",
                           Breed == "Pembroke Welsh Corgis" ~ "Pembroke Welsh\nCorgis",
                           Breed == "Australian Shepherds" ~ "Australian\nShepherds",
                           TRUE ~ Breed))


# Plots 
MainPlot <- ggplot(data=breed_rank) +
  geom_segment(data = data.frame(y=c(1, 3, 5)), aes(x=0.5, xend=12.5, y=y, yend=y), linetype="8f", color=met.brewer("Cross", 12)[10]) +
  geom_bar(aes(x=xrow, y=value, fill=as.factor(xrow)), stat="identity") +
  geom_image(aes(x=0, y=-2.5, image=drawing), size=0.265, color= met.brewer("Cross", 12)[12])  +
  geom_text(data = data.frame(x=0, y=c(1, 3, 5)), aes(x=x, y=y, label=y), size=20, fontface='bold') +
  scale_fill_manual(values=met.brewer("Cross", 12)) +
  coord_polar() +
  theme_void() +
  ylim(-2.5, 5.5) +
  xlim(0, 13) +
  facet_wrap(~Breed, strip.position = "bottom") +
  theme(plot.margin = margin(13, 0, 3, 0, "cm"),
        strip.text = element_text(family="Quicksand", size=60, face="bold", lineheight = 0.3, vjust =1),
        legend.position = "none")


Key <- ggplot(data=breed_rank %>% filter(Breed=="Beagles")) +
  geom_segment(data = data.frame(y=c(1, 3, 5)), aes(x=0.5, xend=12.5, y=y, yend=y), linetype="8f", size=0.7, color=met.brewer("Cross", 12)[10]) +
  geom_bar(aes(x=xrow, y=value, fill=as.factor(xrow)), stat="identity") +
  geom_text(data = data.frame(x=0, y=c(1, 3, 5)), aes(x=x, y=y, label=y), size=25, fontface="bold") +
  scale_fill_manual(values=met.brewer("Cross", 12)) +
  coord_polar() +
  theme_void() +
  ylim(-2.5, 5.5) +
  xlim(0, 13) +
  theme(legend.position = "none")

Annotated <- ggdraw(MainPlot) +
  theme(plot.background = element_rect(fill="#fbf7f0", color="#fbf7f0"),
        plot.margin = margin(1, 0, -1, 0, "cm")) +
  draw_plot(Key, .52, .76, .305, .305) +
  draw_text(text = "Twitter: @BlakeRobMills | Source: American Kennel Club | GitHub: BlakeRMills", x=0.5, y=0.028, color=met.brewer("Cross", 12)[11], fontface="bold", size=80, family = "Quicksand") +
  draw_text(x=0.035, y=0.98, text="Dog Breeds", family="Fira Sans Condensed", size=325, fontface="bold", color=met.brewer("Cross", 12)[11], hjust=0) +
  draw_text(x=0.035, y=0.895, text="Plots show characteristic ratings\nof the 25 most popular dogs of 2020,\naccording to the American Kennel Club.\nRatings are based on a scale of\n1 (low) to 5 (high).", 
            family="Fira Sans", size=92, lineheight=0.37, hjust=0, color=met.brewer("Cross", 12)[12]) +
  draw_text(x=0.872, y=0.98, text="Affectionate w/ Family", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[1]) +
  draw_text(x=0.863, y=0.955, text="Good w/ Children", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[2]) +
  draw_text(x=0.89, y=0.93, text="Good w/ Other Dogs", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[3]) +
  draw_text(x=0.845, y=0.905, text="Shedding", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[4]) +
  draw_text(x=0.833, y=0.88, text="Drooling", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[5]) +
  draw_text(x=0.88, y=0.855, text="Openness to Strangers", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[6]) +
  draw_text(x=0.528, y=0.98, text="Barking", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[12]) +
  draw_text(x=0.519, y=0.955, text="Energy", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[11]) +
  draw_text(x=0.493, y=0.93, text="Trainability", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[10]) +
  draw_text(x=0.49, y=0.905, text="Adaptability", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[9]) +
  draw_text(x=0.489, y=0.88, text="Protectiveness", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[8]) +
  draw_text(x=0.517, y=0.855, text="Playfulness", family="Quicksand", size=88, fontface="bold", color=met.brewer("Cross", 12)[7]) 

ggsave("~/Desktop/DogPlots.png", width = 21, height = 29)
  

