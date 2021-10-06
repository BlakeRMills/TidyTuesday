nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

#Libraries
library(tidyverse)
library(geofacet)
library(showtext)
library(sysfonts)
library(maps)
library(mapdata)
library(gganimate)


#Aes
font_add_google("Lato")
font_add_google("Crete Round")
showtext_auto(enable = TRUE)

titlefont <- "Crete Round"
textfont <- "Lato"
background <- "#f4f1e6"

#Cleaning
salary <- nurses %>% select(State, Year, `Annual Salary Median`) %>% filter(Year >=2000, State %in% state.name) %>% mutate(State = tolower(State)) %>%
  right_join(., map_data("state"), by=c("State"="region")) 



#plot
Animate <- ggplot(data=salary, aes(x=long, y=lat, fill=`Annual Salary Median`, group=group)) + 
  geom_polygon(color = background) +
  scale_fill_gradientn(colors = c("#b63661", "#cc517a", "#f484a9", "#f9c4c6", "#b1ecf1", "#77d7df", "#1db8c6", "#00a2b1"),
                       breaks = c(50000, 75000, 100000), labels= c("$50,000,", "$75,000", "$100,000")) +
  theme_void() +
  transition_states(Year,  transition_length = 2, state_length = 1) +
  theme(legend.position = "bottom") +  
  coord_fixed(1.3) +
  ggtitle("Median Annual Salary of Nurses by State", subtitle = 'Year: {closest_state}') + 
  labs(caption="Twitter: @BlakeRobMills | Source: Data.World | GitHub: BlakeRMills") +
  theme(plot.title = element_text(family= titlefont, face="bold", hjust=0.5, size=21, color="#005861"),
        plot.subtitle = element_text(family= titlefont, hjust=0.5, size=17, color="#00a2b1"),
        plot.background = element_rect(fill=background, color=background),
        plot.caption = element_text(hjust = 0.5, size=9, family=textfont, color="#881f43"),
        legend.position = c(0.2, 0.125),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 8, color="grey35"),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.25, 0.5, "cm")) +
  annotate(geom="text", y=29.5, x=-114.5, label = "Median Salary", size=4, family=titlefont, color="grey25")

#Save
final.animated <- animate(Animate, renderer = gifski_renderer("NurseSalary.gif"), width = 10, height = 6.5, units = "in", res = 300, dev= "png", end_pause = 10)
