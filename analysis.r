library(plyr)
library(dplyr)

library(scales)
library(ggplot2)
library(cetcolor)

heart = read.csv('s1_heart_data.csv')

ggplot(heart %>% filter(distance_start > 0.8)) + geom_point(aes(x=heart_rate_stop, y=heart_rate_start, color=rest_time), alpha=0.8) +
  scale_color_gradientn('Rest Time', trans='log', colors=cet_pal(7, 'inferno'), breaks=c(1,10,100,1000)) +
  geom_abline(intercept=0, slope=1, color='green') + 
  ggtitle('Heart Rate Change During Stops While Running') + 
  xlab('Heart Rate at Stop (bpm)') + 
  ylab('Heart Rate when Starting Again (bpm)')

ggplot(heart %>% filter(distance_start > 0.8)) + geom_point(aes(color=heart_rate_stop, y=heart_rate_start, x=rest_time), alpha=0.8) +
  scale_color_gradientn('Initial Heart Rate (bpm)',  colors=cet_pal(7, 'inferno')) +
  # geom_abline(intercept=0, slope=1, color='green') + 
  ggtitle('Heart Rate Change During Stops While Running') + 
  xlab('Rest Duration (sec)') + 
  ylab('Heart Rate when Starting Again (bpm)') + 
  scale_x_log10()
