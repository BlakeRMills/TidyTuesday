# Data
pride_sponsors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv')
static_list <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')


# Libraries
library(tidyverse)
library(ggiraph)
library(cowplot)
library(packcircles)
library(MetBrewer)
library(showtext)

# Aes
font_add_google("Advent Pro")
showtext_auto()
cols <- met.brewer("Signac", 26)[-1]

# Cleaning and Wrangling
pride_sponsors <- pride_sponsors %>%
  mutate(Company = ifelse(is.na(`True donor value`)==T, Company, `True donor value`)) %>%
  select(Company, `Pride Event Sponsored`)

pride <- static_list %>%
  left_join(pride_sponsors) %>%
  filter(Company != "Grand Total")


# Function for Circle Packing DF
PackingFun <- function(df, CityPride, CityName){
  city <- df %>% 
    filter(`Pride Event Sponsored` == CityPride) %>%
    distinct(Company, .keep_all = T)
  
  packing <- circleProgressiveLayout(city$`Amount Contributed Across States`, sizetype="area")
  citypack <- cbind(city, packing)
  
  brandSize <- city %>% 
    arrange(desc(`Amount Contributed Across States`)) %>%
    select(Company) %>%
    mutate(id=row_number())
  
  dat.gg <- circleLayoutVertices(packing, npoints=50) %>%
    mutate(City = CityName) %>%
    left_join(brandSize, by="id")
}

# All Circle Pack Plots
AllCircles <- rbind(PackingFun(pride, "NYC Pride", "New York City"),
                    PackingFun(pride, "Atlanta Pride", "Atlanta"),
                    PackingFun(pride, "Houston Pride", "Houston"),
                    PackingFun(pride, "LA Pride", "Los Angeles"),
                    PackingFun(pride, "Chicago Pridefest", "Chicago"),
                    PackingFun(pride, "Houston Pride", "Houston"),
                    PackingFun(pride, "Miami Pride", "Miami"),
                    PackingFun(pride, "SF Pride", "San Francisco")) %>%
  mutate(id = paste(id, City))

# Main Plot
plot <- ggplot() +
  geom_polygon(data = AllCircles, aes(x, y, group = id, fill=Company)) +
  ylim(-445, 445) +
  xlim(-900, 800) +
  scale_fill_manual(values=met.brewer("Signac", 26)[-1]) +
  coord_fixed() +
  theme_void() +
  facet_wrap(~City) +
  theme(strip.text = element_text(family ="Advent Pro", face="bold", size=38, margin=margin(b=3), color="#004f63"),
        legend.position = "none", 
        plot.margin = margin(3, 2, -1, 2, unit="cm")) 

# Annotations and Title

final <- ggdraw(plot) +
  theme(plot.background = element_rect(fill="white", color="white")) +
  draw_text("Corporate Rainbow Washing", x=0.5, y=0.96, family="Advent Pro", size=72, fontface="bold", color="#122451") +
  draw_text("Plot displays corporations that sponsored pride in various cities while donating to anti-LGBTQ+\npoliticians. Circle size and listed total corresponds to amount donated to anti-LGBTQ+ politicians across analyzed states.\nCompanies marked with an asterisk (*) have signed on to the Human Right Campaign's Business Pledge.",
            x=0.5, y=0.87, size=23, family="Advent Pro", lineheight=1, color="#004f63") +
  # Atlanta
  draw_text("State Farm", x=0.108, y=0.678, family="Advent Pro", fontface="bold", color=cols[18], size=22) +
  draw_text("($79,550)", x=0.108, y=0.655, family="Advent Pro", color=cols[18], size=18, fontface="bold") +
  draw_text("Enterprise", x=0.282, y=0.678, family="Advent Pro", fontface="bold", color=cols[10], size=22) +
  draw_text("($41,300)", x=0.282, y=0.655, family="Advent Pro", color=cols[10], size=18, fontface="bold") +
  draw_text("Cox Enterprises", x=0.208, y=0.619, family="Advent Pro", fontface="bold", color=cols[9], size=18) +
  draw_text("($6,500)", x=0.208, y=.599, family="Advent Pro", color=cols[9], size=14, fontface="bold") +

  # Chicago 
  draw_text("Budweiser", x=0.548, y=0.677, family="Advent Pro", fontface="bold", color=cols[5], size=22) +
  draw_text("($45,250)", x=0.548, y=0.654, family="Advent Pro", color=cols[5], size=18, fontface="bold") +
  
  # Houston
  draw_text("Budweiser", x=0.729, y=0.669, family="Advent Pro", fontface="bold", color=cols[5], size=22)  +
  draw_text("Chevron", x=0.772, y=0.73, family="Advent Pro", fontface="bold", color=cols[7], size=18)  +
  draw_text("($21,000)", x=0.772, y=0.710, family="Advent Pro", color=cols[7], size=14, fontface="bold") +
  draw_text("Walmart", x=0.77, y=0.626, family="Advent Pro", fontface="bold", color=cols[25], size=18)  +
  draw_text("($25,500)", x=0.77, y=0.606, family="Advent Pro", color=cols[25], size=14, fontface="bold") +
  draw_text("PNC ($10,000)", x=0.879, y=0.71, family="Advent Pro", fontface="bold", color=cols[17], size=14, lineheight=0.8)  +
  draw_text("Bank of America ($10,000)", x=0.925, y=0.69, family="Advent Pro", fontface="bold", color=cols[4], size=14, lineheight=0.8)  +
  draw_text("Walgreens ($4,500)", x=0.905, y=0.671, family="Advent Pro", fontface="bold", color=cols[24], size=14, lineheight=0.8)  +
  draw_text("Gilead ($1,000)", x=0.887, y=0.654, family="Advent Pro", fontface="bold", color=cols[11], size=13, lineheight=0.8)  +
  draw_text("Target* ($500)", x=0.878, y=0.638, family="Advent Pro", fontface="bold", color=cols[20], size=13, lineheight=0.8)  +
  draw_text("Jack Daniel's", x=0.86, y=0.6, family="Advent Pro", fontface="bold", color=cols[12], size=18)  +
  draw_text("($34,250)", x=0.86, y=0.578, family="Advent Pro", color=cols[12], size=14, fontface="bold") +
  draw_line(x=c(0.831, 0.831), y=c(0.61, 0.645), size=1, color=cols[12]) +
  
  # LA
  draw_text("Toyota", x=0.135, y=0.435, family="Advent Pro", color="white", fontface="bold", size=34) +
  draw_text("($601,500)", x=0.135, y=0.403, family="Advent Pro", fontface="bold", color="white", size=18) +
  draw_text("Microsoft*", x=0.242, y=0.43, family="Advent Pro", fontface="bold", color=cols[14], size=16) +
  draw_text("($2,000)", x=0.242, y=0.412, family="Advent Pro", color=cols[14], size=14, fontface="bold") +
 
  # Miami
  draw_text("AT&T*", x=0.458, y=0.435, family="Advent Pro", color="white", fontface="bold", size=30) +
  draw_text("($307,137)", x=0.458, y=0.403, family="Advent Pro", fontface="bold", color="white", size=18) +
  draw_text("Enterprise", x=0.435, y=0.34, family="Advent Pro", color=cols[10], fontface="bold", size=22) +
  draw_text("Jack Daniel's", x=0.443, y=0.315, family="Advent Pro", color=cols[12], fontface="bold", size=22) +
  draw_line(x= c(0.476, 0.52), y=c(0.343, 0.418), color=cols[10], size=1) +
  draw_line(x= c(0.492, 0.514), y=c(0.32, 0.37), color=cols[12], size=1) +
  draw_text("NBC\n($11,000)", x=0.508, y=0.488, family="Advent Pro", fontface="bold", color=cols[15], size=14, lineheight=0.8)  +
  draw_text("Aetna \n($8,250)", x=0.552, y=0.478, family="Advent Pro", fontface="bold", color=cols[1], size=14, lineheight=0.8)  +
  draw_text("Truist ($6,000)", x=0.585, y=0.453, family="Advent Pro", fontface="bold", color=cols[22], size=14, lineheight=0.8)  +
  draw_text("Sysco ($4,000)", x=0.593, y=0.435, family="Advent Pro", fontface="bold", color=cols[19], size=14, lineheight=0.8)  +
  draw_text("Capital One* ($2,500)", x=0.605, y=0.418, family="Advent Pro", fontface="bold", color=cols[6], size=14, lineheight=0.8)  +
  draw_text("Johnson & Johnson* ($1,750)", x=0.617, y=0.402, family="Advent Pro", fontface="bold", color=cols[13], size=14, lineheight=0.8)  +
  draw_text("Trulieve ($1,000)", x=0.585, y=0.387, family="Advent Pro", fontface="bold", color=cols[23], size=13, lineheight=0.8)  +
  draw_text("Outfront ($500)", x=0.577, y=0.372, family="Advent Pro", fontface="bold", color=cols[16], size=13, lineheight=0.8)  +
  
  # NYC
  draw_text("Amazon*", x=0.72, y=0.433, family="Advent Pro", fontface="bold", color=cols[2], size=24)  +
  draw_text("($45,250)", x=0.72, y=0.412, family="Advent Pro", color=cols[2], size=18, fontface="bold") +
  draw_text("Budweiser", x=0.892, y=0.42, family="Advent Pro", fontface="bold", color=cols[5], size=22)  +
  draw_text("Target*", x=0.8115, y=0.372, family="Advent Pro", fontface="bold", color=cols[20], size=14)  +
  draw_line(x=c(0.8115, 0.8115), y=c(0.377, 0.402), color=cols[20], size=1) +
  
  #SF
  draw_text("Comcast", x=0.1, y=0.184, family="Advent Pro", fontface="bold", color=cols[8], size=24) +
  draw_text("($121,350)", x=0.1, y=0.159, family="Advent Pro", color=cols[8], size=18, fontface="bold") +
  draw_text("Budweiser", x=0.288, y=0.172, family="Advent Pro", fontface="bold", color=cols[5], size=22)  +
  
  draw_text("Twitter: @BlakeRobMills | Source: Data For Progress | GitHub: BlakeRMills", 
            x=0.5, y=0.025, fontface="bold", size=16, family = "Advent Pro", color="#122451") +
  draw_text("Toyota has the greatest total contributions to anti-LGBTQ+ politicians ($601,500)\nthat are corporate sponsors for Prides. Budweiser is the corportate sponsor invested in the\nmost Prides (4), while donating $45,250 to anti-LGBTQ+ politicians across analyzed states.",
            x=0.67, y=0.17, size=20, family="Advent Pro", fontface="bold", color="#004f63") +
  draw_line(x=c(0.35, 1), y=c(0.24, 0.24), size=1, color="#004f63") +
  draw_line(x=c(0.35, 1), y=c(0.1, 0.1), size=1, color="#004f63") +
  draw_line(x=c(0.35, 0.35), y=c(0.1, 0.24), size=1, color="#004f63") +
  draw_line(x=c(0, 0.15), y=c(0.96, 0.96), size=2.5, color="#122451") +
  draw_line(x=c(0.85, 1), y=c(0.96, 0.96), size=2.5, color="#122451") 

# Save
ggsave("~/Desktop/PridePlot.png", width=16.5, height=12.5)

