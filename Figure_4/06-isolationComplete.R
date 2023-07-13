
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

############################################################################

library(ggplot2)
library(ggridges)

############################################################################

cmpheme = read.table('04-filtSummary.txt', header = T, sep = '\t')
isotype = readLines('05-isoType.txt')
envs = table(cmpheme$ENV)
isotype = intersect(isotype, names(envs))
cmpheme$ENV = factor(cmpheme$ENV, 
                       levels = isotype,
                       labels = paste0(names(envs[isotype]), ' (',envs[isotype],')'))

hemeplot = ggplot(cmpheme, aes(x = Heme, y = ENV)) + 
  geom_density_ridges_gradient(aes(fill = ..x.., scale = 1.5), size = 0.1) + 
  scale_x_continuous(limits = c(0, 1)) +  
  scale_fill_viridis_c(option = "D") + 
  scale_y_discrete(position = "left") + 
  theme_bw()+
  theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),'mm'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none', 
        axis.text.x = element_text(size = 8, colour = 'black',hjust = 0.5, vjust = 0),
        axis.text.y = element_text(size = 8, colour = 'black',hjust = 1, vjust = 0.5))


ggsave('07-IsolationRidges.pdf', hemeplot, width = 3.5, height = 4.3, units = 'in') 
