library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
source('./R/withinsubjerror.R')

library(RColorBrewer)
col.label <- brewer.pal(3, 'Reds')[3]
col.incongr <- brewer.pal(5, 'Blues')[2]
col.congr <- brewer.pal(5, 'Blues')[5]
cols.labels <- rev(brewer.pal(5, 'Reds')[2:5])
cols.sounds <- rev(brewer.pal(5, 'Blues')[2:5])

# Experiment 1
##############################################################################
exp1 <- read.csv('./data/exp1-final.csv', header=T, stringsAsFactors=F)
exp1$trial_type <- factor(exp1$trial_type, levels=c('label','congruent', 'incongruent'), ordered=T)
exp1$exp <- factor(exp1$exp, levels=c('tyi','tyo'), ordered=T)

exp1.bar <- summarySEwithin(exp1, measurevar='rt', betweenvars='exp', withinvars='trial_type', idvar='subjCode', conf.interval=0.95)

dodge = position_dodge(width=0.9)
plot.exp1 <- ggplot(exp1.bar, aes(x=exp, y=rt, fill=trial_type)) +
  geom_bar(position=dodge, stat='identity') +
  geom_bar(position=dodge, stat='identity', color='black', size=0.5, show_guide=FALSE) +
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci), position=dodge, width=0.4, size=0.5) +    
  coord_cartesian(ylim=c(400,700)) +
  scale_x_discrete('', labels=c('Experiment 1A', 'Experiment 1B')) +
  scale_y_continuous('Verification Speed (msec)', breaks=seq(400,700,by=50)) +
  scale_fill_manual('Cue Type', labels=c('Label','Sound-Image Congruent', 'Sound-Image Incongruent'), 
                    values=c(col.label, col.congr, col.incongr)) +
  theme_classic(base_size=12) +
  theme(legend.position=c(0.75, 0.9), 
        legend.direction='vertical',
        legend.key=element_rect(color='black', size=1.0),
        legend.key.size=unit(8, units='points'),
        axis.ticks.length=unit(5, units='points'),
        axis.ticks.x=element_blank())

jpeg('./figure/plot-exp1.jpg', width=3.5, height=3.5, units='in', res=200)
print(plot.exp1)
dev.off()

pdf('./figure/plot-exp1.pdf', width=3.5, height=3.5)
print(plot.exp1)
dev.off()

# Experiment 2
##############################################################################
require(lme4)
require(AICcmodavg)
typ <- read.csv('./data/typ-final.csv', header=T, stringsAsFactors=F)
inter <- lmer(rt ~ cue_typeC * delayC * zSound + zLabel + (1|subjCode) + (0+cue_typeC|subjCode) + (0+delayC|subjCode) + (1|trialID), data=typ, REML=F)

ratings <- read.csv('./data/TYP/imageratings.csv', header=T, stringsAsFactors=F)
predratings <- seq(round(min(ratings$zSound), 1), round(max(ratings$zSound), 1), by=0.1)

# generate model predictions
predictors <- expand.grid(cue_typeC = c(-0.5, 0.5),
                          delayC = c(-0.5, 0.5),
                          zSound = predratings,
                          zLabel = 0)
modelPredictions <- predictSE.mer(inter, predictors, type='response', print.matrix=F)
plotvals <- cbind(predictors, modelPredictions)

names(plotvals)[names(plotvals) %in% c('fit','se.fit')] <- c('rt','se')
plotvals$upr <- plotvals$rt + plotvals$se
plotvals$lwr <- plotvals$rt - plotvals$se

# reverse key to reflect order in chart
plotvals$cue_typeC <- plotvals$cue_typeC * -1

typ.simultaneous <- ggplot(plotvals[plotvals$delayC == -0.5, ], aes(x=zSound)) +
  geom_smooth(aes(y=rt, ymin=lwr, ymax=upr, color=factor(cue_typeC), linetype=factor(cue_typeC)), stat='identity', lwd=0.7) +
  geom_rug(data=ratings, sides='b', stat='identity') +
  coord_cartesian(ylim=c(550, 750)) +
  scale_x_continuous('', breaks=seq(-1.5, 1.5, by=0.5)) +
  scale_y_continuous('Verification Speed (msec)') + 
  scale_color_manual('Cue Type', labels=c('Sound','Label'), values=c(col.congr, col.label)) +
  scale_linetype_manual('Cue Type', labels=c('Sound','Label'), values=c(2,1)) +
  ggtitle('Simultaneous') +
  theme_classic(base_size=12) +
  theme(legend.background=element_blank(), 
        legend.position='none',
        axis.ticks.length=unit(10, units='points'),
        plot.margin=unit(c(1, 0.5, 1, 1), "lines"))

typ.delayed <- ggplot(plotvals[plotvals$delayC == 0.5, ], aes(x=zSound)) +
  geom_smooth(aes(y=rt, ymin=lwr, ymax=upr, color=factor(cue_typeC), linetype=factor(cue_typeC)), stat='identity', lwd=0.7, show_guide=FALSE) +
  geom_line(aes(y=rt, color=factor(cue_typeC), linetype=factor(cue_typeC)), stat='identity', lwd=0.7) +
  geom_rug(data=ratings, sides='b', stat='identity') +
  coord_cartesian(ylim=c(350, 550)) +
  scale_x_continuous('', breaks=seq(-1.5, 1.5, by=0.5)) +
  scale_y_continuous('') + 
  scale_color_manual('Cue Type', labels=c('Sound','Label'), values=c(col.congr, col.label)) +
  scale_linetype_manual('Cue Type', labels=c('Sound','Label'), values=c(2,1)) +
  ggtitle('Delayed (400 msec)') +
  theme_classic(base_size=12) +
  theme(legend.position=c(0.8, 0.2),
        legend.key=element_rect(color='white', fill='white'),
        axis.ticks.length=unit(10, units='points'),
        plot.margin=unit(c(1, 1, 1, -0.8), "lines"))

gp = gpar(fontsize = 12, fontfamily = 'Helvetica')
plot.typ <- arrangeGrob(typ.simultaneous, typ.delayed, ncol=2,
                  sub=textGrob('Sound-Image Congruence (z-scores)', vjust=-1.0, gp=gp))

jpeg('./figure/plot-exp2.jpg', width=7.5, height=4.5, units='in', res=200)
print(plot.typ)
dev.off()

pdf('./figure/plot-exp2.pdf', width=7.5, height=4.5)
print(plot.typ)
dev.off()

# Experiment 3: Average Image Rating
##############################################################################
source('./R/withinsubjerror.R')
tmc.ave <- read.csv('./data/tys-average-final.csv', header=T, stringsAsFactors=F)
tmc.ave$cueType <- factor(tmc.ave$cueType, levels=c('label','sound'), ordered=T)
tmc.ave.plot <- summarySEwithin(tmc.ave, measurevar='zSound', withinvars='cueType', idvar='name', conf.interval=0.95)

dodge = position_dodge(width=0.8)
ave <- ggplot(tmc.ave.plot, aes(x=cueType, y=zSound, fill=cueType)) +
  geom_bar(stat='identity', position=dodge, width=0.9, color='black') +
  geom_errorbar(aes(ymin=zSound-ci, ymax=zSound+ci), position=dodge, width=0.3) +
  coord_cartesian(ylim=c(-0.10, 0.15)) +
  scale_y_continuous('Average Visual Congruence', breaks=seq(-0.10, 0.20, by=0.05)) +
  scale_x_discrete('Cue Type', labels=c('Label', 'Sound')) +
  scale_fill_manual(values=c(col.label, col.congr)) +
  geom_hline(yintercept=0, lty=1, lwd=0.8) +
  theme_classic(base_size=12) +
  theme(panel.grid.major.y=element_line(color='black', size=0.5),
        axis.ticks.length=unit(5, units='points'),
        axis.ticks.x=element_blank(),
        legend.position='none')

jpeg('./figure/plot-exp3-ave.jpg', width=3.5, height=3.5, units='in', res=200)
print(ave)
dev.off()

pdf('./figure/plot-exp3-ave.pdf', width=3.5, height=3.5)
print(ave)
dev.off()

# Experiment 3: Time Course
##############################################################################
plot_timecourse <- function(data, title, ytitle, legend.position, plot.margin, axis.text.y, cols) {
  ggplot(data, aes(x=time_bin, y=proportion, color=image)) +
    geom_smooth(method='loess', se=F, lwd=1.5) +
    coord_cartesian(ylim=c(0.00, 0.25), xlim=c(0,20)) +
    scale_y_continuous(ytitle) +
    scale_x_continuous('', breaks=seq(0,20,by=5)) +
    scale_color_manual('Sound\nCongruence', labels=c('1','2','3','4'), values=cols) +
    geom_vline(xintercept=6, lty=2, lwd=0.5) +
    #annotate('text', x=label.x, y=label.y, label=label, size=9) +
    ggtitle(title) +
    theme_classic(base_size=12) +
    theme(panel.grid.major.y=element_line(color='black', size=0.5),
          axis.ticks.length=unit(5, units='points'),
          axis.text.y=axis.text.y,
          legend.position=legend.position,
          legend.background=element_blank(),
          plot.margin=plot.margin)
}

tmc.prop <- read.csv('./data/tys-timecourse-final.csv', header=T, stringsAsFactors=F)
tmc.prop$image <- factor(tmc.prop$image, levels=paste0('prop_', c('a','b','c','d')), ordered=T)

tmc.label <- plot_timecourse(tmc.prop[tmc.prop$cueType == 'label', ], 
                            'Category Label', 
                            'Proportion Fixations',
                            'none',
                            unit(c(1,0.5,1,1), 'lines'),
                            element_text(),
                            cols.labels)
tmc.sound <- plot_timecourse(tmc.prop[tmc.prop$cueType == 'sound', ], 
                            'Environmental Sound',
                            '',
                            'none',
                            unit(c(1,1,1,-0.8), 'lines'),
                            element_blank(),
                            cols.sounds)

plot_durations <- function(data, cols) {
  ggplot(data, aes(x=image, y=duration, fill=image)) +
    geom_bar(stat='identity', width=1.0, color='black') +
    geom_errorbar(aes(ymin=duration-ci, ymax=duration+ci), width=0.3) +
    coord_cartesian(ylim=c(0, 250)) +
    scale_y_continuous('Duration Fixations (ms)', breaks=seq(0,300,by=50)) +
    scale_x_discrete('Images ordered by\nvisual congruence', 
                     labels=c('least',expression(symbol('\256')),expression(symbol('\256')),'most')) +
    scale_fill_manual(values=rev(cols)) +
    theme_classic(base_size=10) +
    theme(axis.ticks.length=unit(5, units='points'),
          axis.ticks.x=element_blank(),
          legend.position='none',
          plot.margin = unit(c(0.0,0.0,0.0,0.0), 'lines'))
}

dur <- read.csv('./data/tys-dur-final.csv', header=T, stringsAsFactors=F)
dur$cueType <- factor(dur$cueType, levels=c('label','sound'), ordered=TRUE)
dur$image <- factor(dur$image, levels=c('d','c','b','a'), ordered=TRUE)
dur.plot <- summarySEwithin(dur, measurevar='duration', withinvars=c('cueType','image'), idvar='name')

dur.label <- plot_durations(dur.plot[dur.plot$cueType == 'label', ], cols.labels)
dur.sound <- plot_durations(dur.plot[dur.plot$cueType == 'sound', ], cols.sounds)

gp = gpar(fontsize = 12, fontfamily = 'Helvetica')
tmc <- arrangeGrob(tmc.label, tmc.sound, ncol=2,
                   sub=textGrob('Time Bin (100 msec)', vjust=-1.0, gp=gp))
vp.right <- viewport(x = 0.85, y = 0.33, width=0.24, height=0.34)
vp.left <- viewport(x = 0.36, y = 0.33, width=0.24, height=0.34)

jpeg('./figure/plot-exp3-tmc.jpg', width=7.5, height=5.5, units='in', res=200)
print(tmc)
print(dur.sound, vp=vp.right)
print(dur.label, vp=vp.left)
dev.off()

pdf('./figure/plot-exp3-tmc.pdf', width=7.5, height=5.5)
print(tmc)
print(dur.sound, vp=vp.right)
print(dur.label, vp=vp.left)
dev.off()
