# remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)

library(tidyverse)
q <- ggplot(cars, aes(speed, dist)) + geom_point()
q


t <- sticker("noter/db.png",
             package="staRmanlyrics", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             white_around_sticker = T,
             filename="inst/figures/sticker.png")
