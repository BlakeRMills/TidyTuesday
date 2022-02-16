plate6 <- readr::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

# Libraries
library(tidyverse)
library(showtext)

# Aes
showtext_auto()
font_add_google("Bai Jamjuree")
font_add_google("Chakra Petch")
font1 <- "Bai Jamjuree"
font2 <- "Chakra Petch"

# Wrangling
plate6 <- plate6 %>%
  mutate(RateNeg = `Iliteracy Rate` * -1)


ggplot(data=plate6) +
  geom_bar(aes(x=RateNeg, y=`Iliteracy Rate`),stat="identity", width=2.7,  fill="grey15") +
  geom_segment(aes(x=-101, xend=RateNeg + 0.04, y=`Iliteracy Rate`-0.5, yend=`Iliteracy Rate`-0.5), size=6.5, lineend = "round", color="black") +
  geom_segment(aes(x=-101, xend=RateNeg + 0.04, y=`Iliteracy Rate`-0.5, yend=`Iliteracy Rate`-0.5), size=6, lineend = "round", color="#e3d2c2") +
  geom_text(aes(label=Date, x=-107, y=`Iliteracy Rate`-0.5), size=15, family=font1) +
  scale_y_continuous(limits = c(0, 105)) +
  scale_x_continuous(breaks = plate6$RateNeg, labels=paste(plate6$`Iliteracy Rate`, "%", sep=""), limits = c(-120, -45)) +
  theme_void() +
  ggtitle("ILLITERACY.") +
  labs(caption="Recreation of plot by W.E.B. Du Bois | Data : Anthony Starks | Plot : Blake Robert Mills") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=60, family=font2),
        plot.background = element_rect(fill="#e3d2c2", color="#e3d2c2"),
        plot.margin = margin(0.5, 0.5, 0, 0, "cm"),
        axis.text.x = element_text(size=32, family=font1, vjust=12),
        plot.caption = element_text(family=font2, size=28, hjust=0.5, vjust=4, face="bold")) +
  annotate(geom="text", label="Percent of\nIlliteracy", x=-108, y=0, lineheight=0.3, size=12, family=font1)

ggsave("~/Desktop/DuBois.png", width=6, height = 10)
  
