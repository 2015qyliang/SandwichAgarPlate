
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

#####################################################################

library(ggplot2)

#####################################################################

heme = read.table('01-miniheme.txt', header = T, sep = '\t')
heme$Source = factor(heme$Source, 
                     levels = c("host", "marine sediments", "freshwater"))

set.seed(123)

p = ggplot(heme, aes(x = Source, y = log10(Minimal))) +
  geom_boxplot(size = 0.5, outlier.color = 'grey50', 
               outlier.size = 1, outlier.shape = 16) + 
  geom_jitter(width = 0.3, alpha = 0.2, shape = 16) + 
  scale_y_continuous(limits = c(-5, 3)) + 
  labs(x = 'Source type', 
       y = 'log10(Minimal hemin\ngrowth concentration)') + 
  theme_bw() + theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, color = 'black'), 
    axis.text.x = element_text(size = 8, color = 'black', hjust = 0.5, vjust = 1), 
    axis.text.y = element_text(size = 8, color = 'black')
  )

ggsave('03-MiniHeme.pdf', p, width = 3.3, height = 2.5, units = 'in')

#####################################################################

wr = wilcox.test(log10(heme[heme$Source == 'host', 'Minimal']), 
                 log10(heme[heme$Source == 'freshwater', 'Minimal']))
wr 
# W = 64, p-value = 0.02096

wr = wilcox.test(log10(heme[heme$Source == 'host', 'Minimal']), 
                 log10(heme[heme$Source == 'marine sediments', 'Minimal']))
wr
# W = 89, p-value = 0.01686

wr = wilcox.test(log10(heme[heme$Source == 'marine sediments', 'Minimal']), 
                 log10(heme[heme$Source == 'freshwater', 'Minimal']))
wr
# W = 6, p-value = 0.1386

