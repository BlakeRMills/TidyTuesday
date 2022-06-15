# Data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')

# Libraries
library(cowplot)
library(lubridate)
library(MetBrewer)
library(showtext)
library(tidyverse)

# Aes
cols <- c(met.brewer("Hiroshige")[1:5],
          met.brewer("Hiroshige")[6:10] %>% rev())
showtext_auto()
font_add_google("Advent Pro")

# Wrangling

state <- data.frame(state.name = state.name, state.abb)

d <- drought %>% 
  mutate(date = str_remove(DATE, "d_"),
         date = ymd(date), 
         D0 = D0 - D1,
         D1 = D1 - D2,
         D2 = D2 - D3,
         D3 = D3 - D4,
         W0 = W0 - W1, 
         W1 = W1 - W2,
         W2 = W2 - W3,
         W3 = W3 - W4, 
         state = str_replace(state, "-", " ") %>% str_to_title) %>%
  filter(year(date) >= 2012,
         !state %in% c("Hawaii", "Alaska")) %>%
  left_join(., state, by=c("state" = "state.name")) %>%
  select(-DATE, -`-9`, -`0`) %>%
  pivot_longer(-c(state.abb, date, state)) %>%
  mutate(value = ifelse(grepl("D", name)==T, value, value*-1),
         name = factor(name, 
                       levels = c("D4", "D3", "D2", "D1", "D0",
                                  "W4", "W3", "W2", "W1", "W0"),
                       labels = c("Exceptional Drought", "Extreme Drought", 
                                  "Severe Drought", "Moderate Drought", 
                                  "Abnormally Dry", "Exceptional Wet", 
                                  "Extreme Wet", "Severe Wet", 
                                  "Moderate Wet", "Abnormally Wet"))) 

# Grid
mgrid <- us_state_grid1 %>% filter(!code %in% c("HI", "AK", "DC"))

# Plot
plot <- ggplot(data=d) +
  geom_segment(aes(x=as.Date("2012-01-01"), xend=as.Date("2022-04-01"), y=25, yend=25), linetype="dashed", color=cols[4], alpha=0.75, size=0.30) +
  geom_segment(aes(x=as.Date("2012-01-01"), xend=as.Date("2022-04-01"), y=-25, yend=-25), linetype="dashed", color=cols[9], alpha=0.75, size=0.30) +
  geom_segment(aes(x=as.Date("2012-01-01"), xend=as.Date("2022-04-01"), y=50, yend=50), linetype="dashed", color=cols[3], alpha=0.75, size=0.30) +
  geom_segment(aes(x=as.Date("2012-01-01"), xend=as.Date("2022-04-01"), y=-50, yend=-50), linetype="dashed", color=cols[8], alpha=0.75, size=0.30) +
  geom_text(aes(x=as.Date("2019-02-01"), y=85, label=state.abb), size= 7, color="grey70", family="Advent Pro", fontface="bold") +
  geom_area(aes(x=date, y=value, fill=name)) +
  scale_fill_manual(values= cols) +
  facet_geo(~state, grid = mgrid) +
  theme_void() + 
  theme(plot.background = element_rect(fill="white", color="white"),
        strip.text = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.title = element_text(family="Advent Pro", face="bold", size=45, hjust=0.5, color=cols[7]),
        legend.text = element_text(family="Advent Pro", face="bold", color="gray45", size=17, vjust=5),
        plot.margin = margin(6.5, 1, 1.5, 1, unit="cm")) +
  guides(fill = guide_legend(nrow=2, byrow = TRUE, reverse=TRUE,
                             title = "Conditions", title.position = "top",
                             label.position = "bottom", keywidth=8, vjust=-5))

# Annotates
Annotated <- ggdraw(plot) +
  draw_text(x=0.5, y=0.95, text="Drought & Wet Conditions", family= "Advent Pro", fontface="bold", size=88, color= cols[6]) +
  draw_text(x=0.5, y=0.86, text="Plot displays abnormal drought and wet conditions in the United States from 2012 to 2022. Area plot\nrepresents the total land area of each state experiencing the specified condition.", 
            family= "Advent Pro", fontface="bold", size=25, color=cols[7], lineheight=1) +
  draw_text(x=0.566, y=0.579, color=cols[3], text="50% Drought", family="Advent Pro", size=13, fontface="bold") +
  draw_text(x=0.566, y=0.564, color=cols[4], text="25% Drought", family="Advent Pro", size=13, fontface="bold") +
  draw_text(x=0.443, y=0.553, color=cols[9], text="25% Wet", family="Advent Pro", size=13, fontface="bold") +
  draw_text(x=0.443, y=0.539, color=cols[8], text="50% Wet", family="Advent Pro", size=13, fontface="bold") +
  draw_text("Twitter: @BlakeRobMills | Source: National Integrated Drought Information System | GitHub: BlakeRMills", x=0.5, y=0.02, fontface="bold", size=16, family = "Advent Pro", color=cols[6])

ggsave("~/Desktop/DroughtPlot.png", width=18, height = 13)

