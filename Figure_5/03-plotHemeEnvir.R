
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

###############################################################################

library(ggplot2)
library(ggsci)
library(reshape2)

###############################################################################

hemedf = read.table('01-hemeFilt.txt', header = T, sep = '\t')

isolateType = read.table('02-taxonIsolatetype.txt', header = T, 
                         sep = '\t', row.names = 1)
isolateType = isolateType[order(isolateType$GTDB_Phylum), ]
isoType = isolateType$Isolation.Source
names(isoType) = rownames(isolateType)

hemedf.melt = melt(hemedf, variable.name = 'Genomes', 
                   measure.vars = colnames(hemedf)[2:ncol(hemedf)])

hemedf.melt$envir = isoType[hemedf.melt$Genomes] 

hemedf.melt = hemedf.melt[hemedf.melt$envir != 'host', ]

hemedf.melt$envir = factor(hemedf.melt$envir, 
                           levels = c("freshwater enviroment", 
                                      "marine environment"))

newdf = dcast(data = hemedf.melt, genes ~Genomes, value.var = 'value' )
write.table(newdf, '04-hemeFiltSummary.txt', 
            append = F, quote = F, sep = '\t', 
            row.names = F, col.names = T, na = '0')

###############################################################################

hemedf = read.table('01-hemeFilt.txt', header = T, sep = '\t')

isolateType = read.table('02-taxonIsolatetype.txt', header = T, 
                         sep = '\t', row.names = 1)
isolateType = isolateType[order(isolateType$GTDB_Phylum), ]
isoType = isolateType$Isolation.Source
names(isoType) = rownames(isolateType)

hemedf = hemedf[, 1:(ncol(hemedf)-5)]

hemedf.melt = melt(hemedf, variable.name = 'Genomes', 
                   measure.vars = colnames(hemedf)[2:ncol(hemedf)])

hemedf.melt$envir = isoType[hemedf.melt$Genomes] 

hemedf.melt = hemedf.melt[hemedf.melt$envir != 'host', ]

hemedf.melt$envir = factor(hemedf.melt$envir, 
                           levels = c("freshwater enviroment", 
                                      "marine environment"))

table(hemedf.melt$envir)/nrow(hemedf)
# freshwater enviroment 
# 113 
# marine environment 
# 74 

###############################################################################

head(hemedf.melt)

hemedf.melt$value[hemedf.melt$value < 20] = NA

p = ggplot(hemedf.melt, aes(x = Genomes, y = genes, fill = value)) + 
  geom_tile() + labs(y = NULL) +
  scale_fill_gradient2(low = 'white', mid = 'steelblue', high = 'red',  # FireBrick 
                       midpoint = (60.2 + 20)/2) +
  facet_grid(.~envir, scales = 'free', space = 'free') +
  theme(plot.margin = margin(t = 0.2, r = 1, b = 0.2, l = 0.2, "in"),
        panel.background = element_rect(color = 'black', fill = NA),
        panel.grid = element_blank(), 
        axis.title.y.left =  element_text(size = 10),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        # axis.text.x = element_text(size = 8, color = 'black', hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(size = 6, color = 'black'), 
        strip.text.x = element_text(size = 6 ,  color = 'black'), 
        # strip.text.y = element_text(size = 4 ,angle = 360, color = 'black'), 
        legend.key = element_blank(), 
        legend.position = 'bottom',
        legend.key.width = unit(0.15, 'in'), 
        legend.key.height = unit(0.05, 'in'), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8))
  
ggsave('05-uncultureHeme1.pdf', p, height = 6, width = 9, units = 'in')

#############################################################################

hemedf = read.table('01-hemeFilt.txt', header = T, sep = '\t')
hemedf = hemedf[ , c(1, (ncol(hemedf)-4):ncol(hemedf))]
colnames(hemedf)

hemedf.melt = melt(hemedf, variable.name = 'Genomes', 
                   measure.vars = colnames(hemedf)[2:ncol(hemedf)])

hemedf.melt$value[hemedf.melt$value < 20] = NA

hemedf.melt$envir = 'Self'

p = ggplot(hemedf.melt, aes(x = Genomes, y = genes, fill = value)) + 
  geom_tile() + labs(y = NULL) +
  scale_fill_gradient2(low = 'white', mid = 'steelblue', high = 'red', 
                       midpoint = (60.2 + 20)/2) + # 60.2  49.7
  facet_grid(.~envir, scales = 'free', space = 'free') +
  theme(plot.margin = margin(t = 0.2, r = 0.1, b = 0.2, l = 0, "in"),
        panel.background = element_rect(color = 'black', fill = NA),
        panel.grid = element_blank(), 
        axis.title.y.left =  element_text(size = 10),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        # axis.text.x = element_text(size = 8, color = 'black', hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(size = 6, color = 'black'), 
        strip.text.x = element_text(size = 6 ,  color = 'black'), 
        # strip.text.y = element_text(size = 4 ,angle = 360, color = 'black'), 
        legend.key = element_blank(), 
        legend.position = 'bottom',
        legend.key.width = unit(0.15, 'in'), 
        legend.key.height = unit(0.05, 'in'), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave('05-uncultureHeme2.pdf', p, height = 6, width = 1.8, units = 'in')

############################################################################


