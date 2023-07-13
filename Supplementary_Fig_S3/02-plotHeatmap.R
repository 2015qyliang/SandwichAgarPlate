
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

#########################################################################

library(ggplot2)
library(reshape2)

#########################################################################

mdf = read.table('01-metabolics.txt', header = T, sep = '\t')

colnames(mdf)
table(mdf$Substance.classification)

mdf.melt = melt(mdf, measure.vars = c("S08" , "S26" ,"S20" ), variable.name = 'strains')

p = ggplot(mdf.melt, aes(x = strains, y = Metabolite, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = 'white', mid = 'orange', high = 'FireBrick', 
                       midpoint = 15) + 
  facet_grid(Substance.classification~., scales="free_y", space= "free") +
  theme(plot.margin = margin(t = 0, r = 4.5, b = 0, l = 0, "in"),
        panel.background = element_rect(color = 'black', fill = NA),
        panel.grid = element_blank(), 
        axis.title.y.left =  element_text(size = 10),
        axis.text.x = element_text(size = 8, color = 'black', hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(size = 6, color = 'black'), 
        strip.text.y = element_text(size = 4 ,angle = 360, color = 'black'), 
        legend.key = element_blank(), 
        legend.position = 'bottom',
        legend.key.width = unit(0.15, 'in'), 
        legend.key.height = unit(0.05, 'in'), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave('03-Metabolic.pdf', p, height = 10, width = 9, units = 'in')





