# Data 
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

# Libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(sysfonts)

# Aes
showtext_auto()
font_add_google("Advent Pro")
font <- "Advent Pro"

IvyCols <- c("#381C00", "#a9dbf5", "#B31B1B", "#00693e",
             "#A51C30", "#f58025", "#011F5B", "#00356B")

# Wrangling
Ivy <- pell %>%
  filter(NAME %in% c("Harvard University", "Columbia University", "Columbia University in the City of New y", "Columbia University in the City of New York",
                     "Brown University", "University of Pennsylvania", "Yale University", "Princeton University", "Dartmouth College", "Cornell University")) %>%
  mutate(NAME = case_when(NAME == "Harvard University" ~ "Harvard",
                          NAME == "Yale University" ~ "Yale",
                          NAME == "Princeton University" ~ "Princeton",
                          NAME %in% c("Columbia University", "Columbia University in the City of New y", "Columbia University in the City of New York") ~ "Columbia",
                          NAME == "Brown University" ~ "Brown",
                          NAME == "University of Pennsylvania" ~ "UPenn",
                          NAME == "Cornell University" ~ "Cornell",
                          NAME == "Dartmouth College" ~ "Dartmouth"))

ggplot() + 
  geom_point(data=Ivy, aes(x=YEAR, y=AWARD, color=NAME, size=RECIPIENT)) +
  scale_color_manual(values=IvyCols) +
  scale_size(range = c(2, 12), labels = scales::comma) +
  expand_limits(y = 13000000) +
  scale_y_continuous(labels = scales::dollar, breaks=c(0, 3000000, 6000000, 9000000, 12000000)) +
  facet_wrap(~NAME, ncol = 2,
             labeller = labeller(
               .rows = as_labeller(~ glue::glue("<span style='color: {IvyCols};'> {.x} </span>")))) +
  ggtitle("Ivy League Pell Grants",
          subtitle = "Plot shows the total amount of Pell Grants awarded to students at each Ivy League University from 1999 to 2017.\nPoints are sized to reflect the total number of recipients receiving grants at each university.") +
  labs(y="", x="", size = "Number of Recipients", caption = "Twitter: @BlakeRobMills | Source: U.S. Department of Education | GitHub: BlakeRMills") +
  theme(plot.title = element_text(size=100, face="bold", hjust=0.5, family=font, color = "#202A44"),
        plot.subtitle = element_text(size=30, hjust=0.5, family=font, lineheight = 0.9, color = "#202A44"),
        plot.margin = margin(1, 1, 2, 0.5, "cm"),
        plot.background = element_rect(fill="#f5eee3"),
        plot.caption = element_text(hjust=0.5, family=font, face="bold", size=25, vjust=-6, color = "#202A44"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color="grey65", size=1, linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(4, "lines"),
        strip.text = ggtext::element_textbox(size=50, face="bold", family=font),
        strip.background = element_blank(),
        axis.text.x = element_text(size=21, family=font, face="bold", color = "#202A44"),
        axis.text.y = element_text(size=18, family=font, face="bold", color = "#202A44"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_rect(fill="#f5eee3"),
        legend.title = element_text(face = "bold", family=font, size=35, color = "#202A44"),
        legend.text = element_text(size=25, family=font, color = "#202A44"),
        legend.margin = margin(0.5, 0, 0.25, 0, "cm"),
        legend.key.width = unit(2.5, "cm")) +
  guides(color = "none",
         size = guide_legend(override.aes = list(color = "#202A44"),
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             label.hjust = 0.5))

ggsave("~/Desktop/PellGrants.png", width = 20, height = 20)
  
