# Data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Libraries
library(tidyverse)
library(ggdist)
library(stringr)
library(ggimage)
library(showtext)
library(cowplot)

# Aes

showtext_auto()
font_add_google("Bungee Shade")
font_add_google("Advent Pro")
Star <- "~/Desktop/Chocolate (18 Jan 2022)/Star.png"
Border <- "~/Desktop/Chocolate (18 Jan 2022)/Border.png"


# Wrangling
Count <- chocolate %>% 
  dplyr::count(country_of_bean_origin) %>% 
  filter(n > 50, country_of_bean_origin != "Blend") %>% 
  dplyr::rename(c("Total" = "n"))

chocolate <- chocolate %>%
  mutate(percentage = cocoa_percent %>% str_remove("%") %>% as.numeric(),
         cuts = cut(percentage, c(0, 60, 65, 70, 75, 100),
                    labels = c("< 60%", "60%-65%", "65%-70%", "70%-75%", "> 75%"))) %>%
  filter(country_of_bean_origin %in% Count$country_of_bean_origin) %>% 
  group_by(country_of_bean_origin) %>%
  dplyr::count(cuts, rating) %>%
  left_join(Count)

# Plots
plot <- ggplot(data=chocolate, aes(x=rating, y=n, fill=cuts)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(values=c("#f4bc7c","#bb6634","#8f401c","#5f2e1a","#3b160c")) +
  theme_void() +
  geom_text(aes(label=as.character(rating), x=rating, y=-4), size=20, family="Advent Pro", fontface="bold") +
  geom_image(aes(image=Star, x=1.1, y=45), asp=2.2, size=0.2) +
  geom_text(aes(label="Beans", x=1.15, y=41), angle = 25, size=28, family="Advent Pro", fontface="bold", color="black") +
  geom_text(aes(label=Total, x=1.055, y=48), angle = 25, size=34, family="Bungee Shade", color="black") +

  xlim(c(0.8, 4.25)) +
  theme(panel.background = element_rect(fill="#ffebcc", color="transparent"),
        legend.position = c(0.835, 0.015), 
        legend.direction = "horizontal",
        legend.key.width = unit(3.5, "cm"),
        legend.key.height = unit(1.5, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(6, 0.5, 3, 0.5, "cm"),
        strip.text = element_blank()
        ) +
  facet_wrap(~country_of_bean_origin, ncol=3) 

Annotated <- ggdraw(plot) +
  theme(plot.background = element_rect(fill="#fdf7ee", color="transparent")) +
  draw_label(label = "Twitter: @BlakeRobMills | Source: Flavors of Cacao | GitHub: BlakeRMills", x=0.5, y=0.022, color="#3b160c", fontface="bold", size=100, fontfamily = "Advent Pro") +
  draw_label(label = "Chocolate Ratings", x=0.5, y=0.95, color="#3b160c", fontface="bold", size=400, fontfamily = "Bungee Shade") +
  draw_label(label = "Plots show number of cocoa beans and\ntheir rating distribution (scored 1-5) by origin\ncountry and colored by percentage of cocoa.", 
             x=0.832, y=0.16, color="#5f2e1a", fontface="bold", size=100, fontfamily = "Advent Pro", lineheight=0.3) +
 
  draw_text(text="Cocoa Percentage", x=0.83, y=0.093, size=125, family="Advent Pro", color="#3b160c", fontface="bold") +
  draw_line(x=c(0.68, 0.75), y=c(0.091, 0.091), size=1.5, color="#3b160c") +
  draw_line(x=c(0.91, 0.98), y=c(0.091, 0.091) , size=1.5, color="#3b160c") +
  
  draw_text(text="< 60%", x=0.706, y=0.064, size=65, family="Advent Pro", color="#3b160c", fontface="bold") +
  draw_text(text="60-65%", x=0.77, y=0.064, size=65, family="Advent Pro", color="#fdf7ee", fontface="bold") +
  draw_text(text="65-70%", x=0.83, y=0.064, size=65, family="Advent Pro", color="#fdf7ee", fontface="bold") +
  draw_text(text="70-75%", x=0.89, y=0.064, size=65, family="Advent Pro", color="#fdf7ee", fontface="bold") +
  draw_text(text="> 75%", x=0.952, y=0.064, size=65, family="Advent Pro", color="#fdf7ee", fontface="bold") +
  
  draw_text(text="Belize", x=0.11, y=0.81, size=200, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.008, y=0.316, width=0.326) +

  draw_text(text="Bolivia", x=0.44, y=0.81, size=200, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.337, y=0.316, width=0.326) +
  
  draw_text(text="Brazil", x=0.765, y=0.81, size=210, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.666, y=0.316, width=0.326) +
  
  draw_text(text="Colombia", x=0.12, y=0.64, size=170, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.008, y=0.146, width=0.326) +
  
  draw_text(text="Dominican\nRepublic", x=0.43, y=0.64, size=130, angle=25, family="Bungee Shade", color="#3b160c", lineheight=0.3) +
  draw_image(image=Border, x=0.337, y=0.146, width=0.326) +
  
  draw_text(text="Ecuador", x=0.76, y=0.65, size=170, angle=25, family="Bungee Shade", color="#3b160c") + 
  draw_image(image=Border, x=0.666, y=0.146, width=0.326) +
  
  draw_text(text="Guatemala", x=0.12, y=0.47, size=170, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.008, y=-0.024, width=0.326) +
  
  draw_text(text="Madagascar", x=0.44, y=0.485, size=140, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.337, y=-0.024, width=0.326) +
  
  draw_text(text="Mexico", x=0.765, y=0.47, size=210, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.666, y=-0.024, width=0.326) +
  
  draw_text(text="Nicaragua", x=0.12, y=0.305, size=160, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.008, y=-0.194, width=0.326) +
  
  draw_text(text="Peru", x=0.42, y=0.302, size=200, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.337, y=-0.194, width=0.326) +
  
  draw_text(text="Tanzania", x=0.78, y=0.305, size=180, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.666, y=-0.194, width=0.326) +
  
  draw_text(text="Venezuela", x=0.105, y=0.13, size=150, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.008, y=-0.364, width=0.326) +
  
  draw_text(text="Vietnam", x=0.445, y=0.13, size=200, angle=25, family="Bungee Shade", color="#3b160c") +
  draw_image(image=Border, x=0.337, y=-0.364, width=0.326) 
  
# Save 
ggsave("~/Desktop/Chocolate.png", height = 23, width = 25)
