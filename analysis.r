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

ggplot(heart %>% filter(distance_stop > 0.5, rest_time < 361, heart_rate_stop < 190, !is.na(avg_temp))) + 
  geom_point(aes(x=rest_time, y=heart_rate_start, color=heart_rate_stop)) +
  facet_grid(cut(avg_temp, breaks=seq(-10,40, 5))~.) + 
  scale_color_gradientn('Initial Heart Rate (bpm)',  colors=cet_pal(7, 'inferno')) +
  xlab('Rest Time') + ylab('Heart Rate when Starting Again') + 
  ggtitle('Heart Rate Relaxation after Paused Running', 
          subtitle='Over various temperatures ranges (indicated by facet) and initial heart rates (indicated by color)\nhttps://maxcandocia.com/article/2019/Jan/09/modeling-heart-rate-nonlinear')
