library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

library(motivatedcues)

# Experiment 1
##############################################################################
data("exp1_final_rep")
exp1 <- exp1_final_rep
exp1$trial_type <- factor(exp1$trial_type, levels=c('label','congruent', 'incongruent'), ordered=T)
exp1$exp <- factor(exp1$exp, levels=c('tyi','tyo-rep'), ordered=T)

exp1.bar <- summarySEwithin(exp1, measurevar='rt', betweenvars='exp', withinvars='trial_type', idvar='subjCode', conf.interval=0.95)

dodge = position_dodge(width=0.9)
plot.exp1 <- ggplot(exp1.bar, aes(x=exp, y=rt, fill=trial_type)) +
  geom_bar(position=dodge, stat='identity') +
  geom_bar(position=dodge, stat='identity', color='black', size=0.5, show_guide=FALSE) +
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci), position=dodge, width=0.4, size=0.5) +
  coord_cartesian(ylim=c(400,700)) +
  scale_x_discrete('', labels=c('Experiment 1A', 'Experiment 1B')) +
  scale_y_continuous('Verification Speed (ms)', breaks=seq(400,700,by=50)) +
  scale_fill_manual('Cue Type', labels=c('Label','Congruent Sound', 'Incongruent Sound'),
                    values=c(col.label, col.congr, col.incongr)) +
  theme_classic(base_size=12) +
  theme(legend.position=c(0.75, 0.9),
        legend.direction='vertical',
        legend.key=element_rect(color='black', size=1.0),
        legend.key.size=unit(8, units='points'),
        axis.ticks.length=unit(5, units='points'),
        axis.ticks.x=element_blank())

plot.exp1

# Experiment 2
##############################################################################
require(lme4)
require(AICcmodavg)

data("typ_final")
data("imageratings")
typ <- typ_final
ratings <- imageratings

inter <- lmer(rt ~ cue_typeC * delayC * zSound + zLabel + (1+cue_typeC|subjCode) + (1|trialID), data=typ, REML=F)

predratings <- seq(round(min(ratings$zSound), 1), 1.5, by=0.1)

# generate model predictions
predictors <- expand.grid(cue_typeC = c(-0.5, 0.5),
                          delayC = c(-0.5, 0.5),
                          zSound = predratings,
                          zLabel = 0)
modelPredictions <- predictSE(inter, predictors, type='response', print.matrix=F)
plotvals <- cbind(predictors, modelPredictions)

names(plotvals)[names(plotvals) %in% c('fit','se.fit')] <- c('rt','se')
plotvals$upr <- plotvals$rt + plotvals$se
plotvals$lwr <- plotvals$rt - plotvals$se

# reverse key to reflect order in chart
plotvals$cue_typeC <- plotvals$cue_typeC * -1

library(png)

img.y <- 0.11
img.x <- c(0.15, 0.4, 0.65, 0.90)
img.width <- 0.18
img.height <- img.width*(0.78)
img1 <- readPNG("figure/bird_pic_neither.png")
img1 <- rasterGrob(img1, interpolate=TRUE, width=img.width, height=img.height, x=img.x[1], y=img.y)
img2 <- readPNG("figure/bird_pic_label.png")
img2 <- rasterGrob(img2, interpolate=TRUE, width=img.width, height=img.height, x=img.x[2], y=img.y)
img3 <- readPNG("figure/bird_pic_sound.png")
img3 <- rasterGrob(img3, interpolate=TRUE, width=img.width, height=img.height, x=img.x[3], y=img.y)
img4 <- readPNG("figure/bird_pic_both.png")
img4 <- rasterGrob(img4, interpolate=TRUE, width=img.width, height=img.height, x=img.x[4], y=img.y)

typ.all <- ggplot(plotvals, aes(x=zSound)) +
  geom_smooth(aes(y=rt, ymin=lwr, ymax=upr, color=factor(cue_typeC), linetype=factor(cue_typeC)),
              data=plotvals[plotvals$delayC == -0.5, ],
              stat='identity', lwd=0.7) +
  geom_smooth(aes(y=rt, ymin=lwr, ymax=upr, color=factor(cue_typeC), linetype=factor(cue_typeC)),
              data=plotvals[plotvals$delayC == 0.5, ],
              stat='identity', lwd=0.7) +
  geom_rug(data=ratings, sides='b', stat='identity') +
  coord_cartesian(ylim=c(400, 750)) +
  scale_x_continuous('Sound-Image Congruence (z-score)', breaks=seq(-1.5, 1.5, by=0.5)) +
  scale_y_continuous('Verification Speed (ms)', breaks=seq(400,750,by=50)) +
  scale_color_manual('Cue Type', labels=c('Sound','Label'), values=c(col.congr, col.label)) +
  scale_linetype_manual('Cue Type', labels=c('Sound','Label'), values=c(2,1)) +
  annotate('text', label='Simultaneous', x=0, y=740) +
  annotate('text', label='Delayed (400 msec)', x=0, y=540) +
  annotation_custom(img1) +
  annotation_custom(img2) +
  annotation_custom(img3) +
  annotation_custom(img4) +
  theme_classic(base_size=12) +
  theme(legend.position=c(0.85, 0.59),
        legend.key=element_rect(color='white', fill='white'),
        axis.ticks.length=unit(10, units='points'))

print(typ.all)
