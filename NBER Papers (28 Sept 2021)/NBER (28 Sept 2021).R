#Libraries
library(tidyverse)
library(showtext)
library(plyr)
library(cowplot)

#Aesthethics 
font_add_google("Hind Siliguri")
showtext_auto()
PlotFont <- "Hind Siliguri"
AxisFont <- "Roboto"
BColor <- "#ebe6ea"

#Data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#Filtering and Cleaning
All <- left_join(papers, paper_programs, by = "paper") %>% left_join(., programs, by = "program") %>%
  filter(is.na(program_desc)==FALSE, program_desc != "Technical Working Papers", year >= 2000) %>%
  mutate(program_desc = revalue(program_desc, c("International Finance and Macroeconomics" = "International Economics",
                                                "Economic Fluctuations and Growth" = "Economic Fluctuations",
                                                "Environment and Energy Economics" = "Environment Economics",
                                                "International Trade and Investment" = "International Trade",
                                                "Productivity, Innovation, and Entrepreneurship" = "Innovation",
                                                "Development of the American Economy" = "American Economics",
                                                "Development Economics" = "Development",
                                                "Economics of Education" = "Education")))

#Plot DF
TotalPapers <- All %>% dplyr::count(year, month)
Total <- All %>% group_by(year, month) %>% dplyr::count(program_desc) %>% left_join(., TotalPapers, by=c("year", "month")) %>%
  mutate(per = (n.x/n.y)*100)

#Main Plot
p1 <- ggplot(Total, aes(x=year, y=month, fill=per)) +
  geom_tile() +
  scale_y_reverse(breaks =c(1, 12), labels=c("Jan", "Dec")) + 
  scale_x_continuous(breaks=c(2000,2021)) + 
  scale_fill_gradientn(colors = c("#f5bc87", "#f4a881", "#df7e5e", "#ae6153", "#a4707f", "#5f4859", "#5a3b52")) +
  theme_void() +
  labs(fill=" ") + 
  theme(strip.text = element_text(size=90, family=PlotFont, face="bold", color= "grey25"),
        plot.background = element_rect(color=BColor, fill=BColor),
        axis.text = element_text(size=60, family=AxisFont, color="grey35"),
        panel.background = element_rect(color=BColor, fill=BColor),
        plot.margin = margin(0.5, 1, 2, 0.5, unit = "cm"),
        panel.spacing.y = unit(0.15, "cm"),
        panel.spacing.x = unit(.35, "cm"), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=60, family=AxisFont)) +
  facet_wrap(~program_desc)

#Title
Title <- ggplot() + ggtitle("NBER Paper Topic Popularity") + 
  theme_void() + 
  theme(plot.title = element_text(hjust= 0.5, size=250, color="grey25", face = "bold", family = PlotFont),
        plot.background = element_rect(fill=BColor, color = BColor),
        plot.margin = unit(c(1,0,0,0),"cm"))

#Combine Plots
Main <- plot_grid(Title, p1, ncol = 1, rel_heights = c(0.1, 1)) +
  theme(plot.margin = unit(c(2,0,0,0),"cm"))

#Annotate
ggdraw(Main) +
  draw_label("Twitter: @BlakeRobMills | Source: National Bureau of Economic Research | GitHub: BlakeRMills", x=0.5, y=0.015, size=85, fontfamily = AxisFont, color="grey25", fontface = "bold") +
  draw_label("Color displays percentage topic occupies", x=0.712, y=0.0875, size=60, fontfamily = AxisFont, fontface = "bold", color="grey25") +
  draw_label(" of total papers published each month", x=0.712, y=0.0725, size=60, fontfamily = AxisFont, fontface = "bold", color="grey25") +
  draw_label("Fluctuations in Popularity of Working Paper Topics in NBER Since January 2000 to June 2021", x=0.5, y=0.888, size=110, fontfamily = AxisFont, color="grey35") 
  
#Save
ggsave("~/Desktop/Articles.png", width=25, height=25)

