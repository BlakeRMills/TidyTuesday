# Libraries/Functions
library(tidyverse)
library(sf)
library(showtext)
library(MetBrewer)
`%notin%` <- Negate(`%in%`)

# Data
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

## Roads come from Natural Earth
roads <- geojsonio::geojson_sf("~/Desktop/ne_10m_roads") %>% 
  filter(sov_a3=="USA", type=="Major Highway")

# Aes
showtext_auto()
font_add_google("Advent Pro")
darkRed <- met.brewer("Greek", 10)[1]
medRed <- met.brewer("Greek", 10)[2]

# Wrangling

grid <- roads %>% st_make_grid(n=100) %>%
  st_as_sf(crs=4326) %>% mutate(geometry = x) %>% 
  dplyr::mutate(row = row_number())

stations <- stations %>%
  filter(STATE %notin% c("AK", "HI")) %>%
  select(LATITUDE, LONGITUDE) %>% 
  mutate(n=1) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs=4326)

stationCounts <- grid %>% st_join(., stations, join=st_contains) %>% 
  as.data.frame() %>%
  select(row, n) %>%
  group_by(row) %>% 
  summarize(sum = sum(n, na.rm = T))

Union <- st_union(roads)

RoundCount <- grid %>% left_join(stationCounts) %>% filter(sum > 0) %>% 
  filter(row %notin% c(100)) %>%
  mutate(sumCap = ifelse(sum > 1000, 1000, sum))

RoadDensity <- c()
for(j in unique(RoundCount$sumCap)){
  ColorRoad <- st_union(subset(RoundCount, RoundCount$sumCap==j))
  ColorRoad <- st_intersection(Union, ColorRoad) %>% 
    as.data.frame() %>% 
    mutate(RoadCount=j)
  RoadDensity <-  rbind(RoadDensity, ColorRoad)
}

ggplot() + 
  geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=group), color="#fadbbd", fill="#fdefe2") +
  geom_sf(data=roads, aes(geometry=geometry), size=0.75, color="grey65") +
  geom_sf(data=RoadDensity, aes(geometry=geometry, color=RoadCount), size=0.75) +
  scale_color_stepsn(n.breaks=10, colors=rev(met.brewer("Greek", 10)),
                     labels=c(seq(100,800, 100), "900+")) +
  theme_void() +
  scale_y_continuous(limits=c(25, 50)) +
  scale_x_continuous(limits=c(-125, -67)) +
  labs(caption="Twitter: @BlakeRobMills | Source: US DOT | GitHub: BlakeRMills", color="Number of Alternative Fuel Stations") + 
  ggtitle("Alternative Fuel Stations Around Major Highways",
          subtitle = "Plot shows major highways colored by number of alternative fuel stations nearby in the United States. Grey lines\nrepresent no alternative fuel stations in the area. (Plot does not imply stations are on the highway, rather near to the highway.)") +
  theme(legend.position = c(0.5, 1.03),
        legend.direction = "horizontal",
        plot.background = element_rect(color="#fcf9f6", fill="#fcf9f6"),
        legend.title = element_text(face="bold", family="Advent Pro", size=55, vjust=-5.25, hjust=0.5, color=met.brewer("Greek", 10)[5]),
        legend.key.width = unit(3.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.text = element_text(family="Advent Pro", size=30, face="bold", vjust=11, color=darkRed),
        plot.title = element_text(hjust=0.5, size=120, face="bold", family="Advent Pro", vjust=8.5, color=met.brewer("Greek", 10)[3]),
        plot.subtitle = element_text(hjust=0.5, size=45, family="Advent Pro", vjust=19, lineheight = 0.35, color=darkRed),
        plot.margin = margin(2.5, 0, 0, 0, "cm"),
        plot.caption = element_text(family="Advent Pro", face="bold", hjust=0.5, size=35, vjust=4, color=medRed)) +
  guides(color=guide_colorsteps(title.position="top")) +
  annotate(geom="curve", y=30, yend=37.1, x=-120.8, xend=-122.45, curvature=-0.3,  arrow = arrow(length = unit(3, "mm")), size=0.8, color=medRed) +
  annotate(geom="curve", y=30, yend=33.5, x=-120.8, xend=-118.2, curvature=-0.3,  arrow = arrow(length = unit(3, "mm")), size=0.8, color=medRed) +
  annotate(geom="curve", y=38, yend=42.55, x=-69.5, xend=-70.5, curvature=0.7,  arrow = arrow(length = unit(2.5, "mm")), size=0.8, color=medRed) +
  annotate(geom="text", x=-120.8, y=28.8, label="On the West Coast, the Los Angeles and\nSan Francisco/San Diego metro areas have\nthe highest concentration of stations.", family="Advent Pro", size=11, lineheight=0.3, color=medRed) +
  annotate(geom="text", x=-69.5, y=36.7, label="On the East Coast, the Boston\nmetro area has the highest\nconcentration of stations.", family="Advent Pro", size=11, lineheight=0.3, color=medRed)

ggsave("~/Desktop/AlternativeFuel.png", width = 12, height = 8)
