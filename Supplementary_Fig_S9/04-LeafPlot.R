
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

#############################################################################

library(tidyverse)
library(ggplot2)
library(ggridges)

#############################################################################

famsFilt = readLines('01-LeafFilt.txt')
famsSort = readLines('02-LeafSort.txt')

famsSortFilt = famsSort[famsSort %in% famsFilt]

hemedf = read.table('03-LeafHeatmapClass.txt', header = T, sep = '\t')

#############################################################################

colnames(hemedf)

hemedf = hemedf[, c('NCBI_RefSeq', 'Heme', 'Order')]
hemedf = hemedf[hemedf$Order %in% famsSortFilt, ]


hemedf$Order = factor(hemedf$Order, levels = rev(famsSortFilt))

hemeplot = ggplot(hemedf, aes(x = Heme, y = Order)) + 
  geom_density_ridges_gradient(aes(fill = ..x.., scale = 0.8), size = 0.1) + 
  scale_x_continuous(limits = c(0, 1)) +  
  scale_fill_viridis_c(option = "D") + 
  scale_y_discrete(position = "right") + 
  theme_bw()+
  theme(plot.margin=unit(c(0, 0, 0, 0),'mm'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none', 
        axis.text.x = element_text(size = 8, colour = 'black',hjust = 0.5, vjust = 0),
        axis.text.y = element_text(size = 10, colour = 'black',hjust = 0, vjust = 0, face = 'italic'))

# ggsave('LeafHeme.pdf', hemeplot, width = 4, height = 7, units = 'in') 

ggsave('05-LeafHeme.pdf', hemeplot, width = 2.5, height = 7, units = 'in') 

rev(table(hemedf$Order))
