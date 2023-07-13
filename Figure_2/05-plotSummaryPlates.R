
# coding: utf-8
# email: qfsfdxlqy@163.com
# Github: https://github.com/2015qyliang

##########################################################################

library(reshape2)
library(ggplot2)
library(ggsci)

##########################################################################

newdf = read.table('02-HelperStrains.txt', header = T, sep = '\t')
helpers = types = sameSp = c()

for (hl in sort(unique(newdf$Hepler))) {
  for (tp in c('KS', 'NS', 'NG')) {
    tmpdf = newdf[newdf$Hepler == hl & newdf$Type == tp , ]
    helpers = append(helpers, hl)
    sameSp = append(sameSp, length(unique(tmpdf$sameSp)))
    types = append(types, tp)
  }
}

filtDF = data.frame(Helper = helpers, sameSp, Type = types )
filtDF$Type = factor(filtDF$Type, levels = c('KS', 'NS', 'NG'))
filtDF$Helper = factor(filtDF$Helper, 
                       levels = c('CK','S08','S11','S20','S26','S47','S60','S63','S64'))

#################################################

vip = ggplot(filtDF, aes(x= Type, y = sameSp, group = Helper, color = Helper)) + 
  geom_line(linewidth = 0.25) + 
  # geom_text(aes(label = sameSp, x = ), size = 3) +
  scale_color_manual(values = c('black','#BC3C29E5','#0072B5E5','#E18727E5','#20854EE5', 
                                '#7876B1E5','#6F99ADE5','#FFDC91E5','#EE4C97E5'), 
                     breaks = c('CK','S08','S11','S20','S26','S47','S60','S63','S64')) + 
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.4),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.key.height = unit(0.1, 'in'), 
        legend.key.width = unit(0.1, 'in'), 
        axis.text.x = element_text(size = 8, colour = 'black', 
                                   angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 8, colour = 'black', angle = 90,
                                   hjust = 0.5, vjust = 0.5))
# ggsave('07-Summary.pdf', vip, width = 4, height = 3, units = 'in')

ggsave('07-Summary.pdf', vip, width = 3.6, height = 2.7, units = 'in')


#################################################

vip = ggplot(filtDF, aes(x= Type, y = sameSp, group = Helper, color = Helper)) + 
  geom_line() + 
  # geom_text(aes(label = sameSp, x = ), size = 3) +
  scale_color_manual(values = c('black','#BC3C29E5','#0072B5E5','#E18727E5','#20854EE5', 
                               '#7876B1E5','#6F99ADE5','#FFDC91E5','#EE4C97E5'), 
                    breaks = c('CK','S08','S11','S20','S26','S47','S60','S63','S64')) + 
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.4),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.key.height = unit(0.1, 'in'), 
        legend.key.width = unit(0.1, 'in'), 
        axis.text.x = element_text(size = 8, colour = 'black', 
                                   angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 8, colour = 'black', angle = 90,
                                   hjust = 0.5, vjust = 0.5))
# ggsave('07-Summary.pdf', vip, width = 4, height = 3, units = 'in')

ggsave('07-Summary.pdf', vip, width = 3.6, height = 2.7, units = 'in')

#################################################

vip = ggplot(filtDF, aes(x= Type, y = sameSp, group = Helper, fill = Helper)) + 
  geom_col(position = 'dodge') + 
  # geom_text(aes(label = sameSp, x = ), size = 3) +
  scale_fill_manual(values = c('grey50','#BC3C29E5','#0072B5E5','#E18727E5','#20854EE5', 
                               '#7876B1E5','#6F99ADE5','#FFDC91E5','#EE4C97E5'), 
                    breaks = c('CK','S08','S11','S20','S26','S47','S60','S63','S64')) + 
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.4),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.key.height = unit(0.1, 'in'), 
        legend.key.width = unit(0.1, 'in'), 
        axis.text.x = element_text(size = 8, colour = 'black', angle = 90, hjust = 0.5, vjust = 0.5),
        # axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = 'black', hjust = 1, vjust = 0.5))
# ggsave('07-Summary.pdf', vip, width = 4, height = 3, units = 'in')

ggsave('07-Summary.pdf', vip, width = 3.6, height = 2.7, units = 'in')








##########################################################################
##########################################################################
##########################################################################

platesDF = read.table('01-plates.txt', header = T, sep = '\t')
platesDF = melt(platesDF, variable.name = 'GP',
                measure.vars = colnames(platesDF)[2:4])

platesDF$GP = factor(platesDF$GP, levels = c('S', 'SG', 'G'))

vip = ggplot(platesDF, aes(x= Phylum , y = value, fill = GP)) + 
  geom_col(position = 'stack') + 
  scale_fill_nejm() + 
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8, colour = 'black', 
                                   angle = 30, face = 'italic', 
                                   hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8, colour = 'black', hjust = 1, vjust = 0.5),
        legend.text = element_text(size = 8, colour = 'black', hjust = 0, vjust = 0.5),
        legend.key.width = unit(0.1, 'in'), 
        legend.key.height = unit(0.01, 'in'))
ggsave('06-plate.pdf', vip, width = 3, height = 3, units = 'in')

##########################################################################

newdf = read.table('02-Summary.txt', header = T, sep = '\t')
newdf$Taxon = factor(newdf$Taxon, levels = c('KS', 'NS', 'NG'))
vip = ggplot(newdf, aes(x= Taxon, y = Count, group = Gps)) + 
  geom_line() + geom_text(aes(label = Count), size = 3) + 
  # geom_col(fill = NA, color = 'grey20', size = 0.2) +
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.4),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8, colour = 'black', angle = 90, hjust = 0.5, vjust = 0.5),
        # axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = 'black', hjust = 1, vjust = 0.5))
# ggsave('07-Summary.pdf', vip, width = 4, height = 3, units = 'in')

ggsave('07-Summary.pdf', vip, width = 3.6, height = 2.7, units = 'in')

##########################################################################


newdf = read.table('04-GrowthPromotion.txt', header = T, sep = '\t')
newdf$Taxon = factor(newdf$Taxon, 
                     levels = c('Total', 'Actinobacteria', 'Bacteroidetes',
                                'Firmicutes', 'Proteobacteria', 'Rhodothermaeota'))
vip = ggplot(newdf, aes(x= Taxon, y = Count, group = Promotation, fill = Promotation)) + 
  geom_col(position = 'stack') +
  geom_text(aes(label = Count), size = 3) +
  scale_fill_manual(values = c(pal_d3(palette = 'category20c')(7)[7], 
                               pal_d3(palette = 'category20c')(7)[1])) + 
  theme_bw()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5),'in'),
        panel.background = element_rect(fill="white", color="black", size = 0),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(size = 8, colour = 'black', 
                                   angle = 30, hjust = 1, vjust = 1),
        # axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = 'black', hjust = 1, vjust = 0.5),
        legend.position = 'none')
ggsave('08-GrowthPromotio.pdf', vip, width = 2.6, height = 3, units = 'in')






