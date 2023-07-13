
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

##########################################################################

library(ggplot2)
library(reshape2)
library(RColorBrewer)

##########################################################################

otumodu = read.table('01-reformat.txt', header = T, sep = '\t')
colnames(otumodu)[1] = 'Strains'

omdf = melt(otumodu, measure.vars = colnames(otumodu)[2:ncol(otumodu)])
omdf$Strains = factor(omdf$Strains, levels = otumodu$Strains)

##########################################################################

table(omdf$value)

omdf$Flcolor = brewer.pal(n = 8, name = 'Blues')[2]
omdf$Flcolor[omdf$value == 0.33] = brewer.pal(n = 8, name = 'Blues')[4]
omdf$Flcolor[omdf$value == 0.66] = brewer.pal(n = 8, name = 'Blues')[6]
omdf$Flcolor[omdf$value == 1] = brewer.pal(n = 8, name = 'Blues')[8]

pauxo = ggplot(omdf, aes(x = Strains, y = variable)) +
  geom_tile(fill = omdf$Flcolor) + 
  # scale_fill_discrete(values = brewer.pal(n = 8, name = 'Reds')[c(2,4,6,8)]) + 
  theme_bw()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8, colour = 'black', face = 'italic', 
                                   angle = 30, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 8, colour = 'black', hjust = 1, vjust = 0.5), 
        legend.key.height = unit(.05, 'in'),
        legend.key.width = unit(.1, 'in'),
        legend.title = element_blank(),
        legend.text =  element_text(size = 6, colour = 'black'), 
        legend.position = 'none')

ggsave('03_Auxo.pdf', pauxo, width = 8, height = 2.9, units = 'in')




