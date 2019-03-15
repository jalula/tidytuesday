library(tidyverse)
library(extrafont)
loadfonts(device = "win")

board_games <- readr::read_csv('data/2019/2019-03-12/board_games.csv')

head(board_games)

hl_games <- board_games %>% 
  filter(board_games$name %in% c('Catan','Twilight Struggle','Acquire','Mouse Trap','Connect Four'))

board_games %>% 
  ggplot(.) +
  geom_point(aes(x = year_published, y = average_rating),
             alpha = .1,
             shape = 16) +
  geom_point(data = hl_games,
             aes(x = year_published, y = average_rating),
             size = 1.1) +
  geom_text(data = hl_games,
             aes(x = year_published, y = average_rating, label = name),
             vjust=c(-1,-1,1,1,-1),
             size = 5) +
  geom_smooth(aes(x = year_published, y = average_rating),
              se = FALSE,
              col = 'red',
              size = 1.2) +
  theme(plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_line(colour = "grey85"),
        text=element_text(family="Calibri", size=15)) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2010, by = 10),
                     labels = c('1950',"'60","'70","'80","'90","2000","'10"),
                     minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2.5),
                     labels = c('0.0','2.5','5.0','7.5','10.0'),
                     minor_breaks = NULL,
                     limits = c(0,10.1)) +
  labs(x = '',
       y = 'Average User Rating',
       title = 'A Golder Age of Board Games?',
       subtitle = 'Average user rating for games by original year of production',
       caption = "(Source: Board Game Geeks)")

dist.name<-adist('mouse',board_games$name, partial = TRUE, ignore.case = TRUE)
board_games$name[dist.name == 0]
