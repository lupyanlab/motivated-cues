library(ggplot2)
library(grid)
library(AICcmodavg)
source('R/withinsubjerror.R')

library('RColorBrewer')

blue <- brewer.pal(6, 'Blues')[1]
green <- brewer.pal(6, 'Greens')[3]
red <- brewer.pal(6, 'Reds')[6]

tyv <- read.csv('output/tyv_valid.csv', header=TRUE)
plot_tyv <- function(tyv) {
  tyv.bar <- summarySEwithin(tyv, measurevar='rt', betweenvars=NULL, withinvars=c('picType','cueType'), idvar='subjCode')
  g <- ggplot(tyv.bar, aes(x=cueType, y=rt, fill=picType)) +
    geom_bar(position=position_dodge(width=.9), stat='identity', color='black') +
    geom_errorbar(aes(ymin=rt-se, ymax=rt+se), position=position_dodge(width=0.9), width=0.4, size=1.0) +
    coord_cartesian(ylim=c(400,700)) +
    scale_x_discrete('Cue Type', labels=c('Label', 'Sound')) +
    scale_fill_manual('Image Type', labels=c('Silent', 'Action'), values=c(red,blue)) +
    scale_y_continuous('Verification Speed (msec)', breaks=seq(400,700,by=50)) +
    theme_classic(base_size=20) +
    theme(legend.justification='right', 
          legend.direction='vertical', 
          legend.background=element_blank(),
          legend.title.align=0.0,
          axis.ticks.length=unit(10, units='points'))
  return(g)
}

g <- plot_tyv(tyv)


plot_tyi1 <- function() {
  tyi <- read.csv('output/tyi_valid.csv', header=TRUE)
  tyi$cue_factor <- factor(tyi$cue_factor, levels=c('male','female','congruent','incongruent'), ordered=T)
  
  tyi.bar <- summarySEwithin(tyi, measurevar='rt', betweenvars=NULL, withinvars=c('cue_factor'), idvar='subjCode')
  tyi.bar$fill <- c('a','a','b','c')
  g <- ggplot(tyi.bar, aes(x=cue_factor, y=rt, fill=fill)) +
    geom_bar(position=position_dodge(width=.9), stat='identity', color='black') +
    geom_errorbar(aes(ymin=rt-se, ymax=rt+se), position=position_dodge(width=0.9), width=0.4, size=1.0) +    
    coord_cartesian(ylim=c(400,700)) +
    scale_x_discrete('Cue Type', labels=c('Label\n(male)', 'Label\n(female)', 'Sound\n(congruent)', 'Sound\n(incongruent)')) +
    scale_y_continuous('Verification Speed (msec)', breaks=seq(400,700,by=50)) +
    scale_fill_manual('Cue Type', values=c(red, green, blue)) +
    theme_classic(base_size=20) +
    theme(legend.position='none', axis.ticks.length=unit(10, units='points'))
  return(g)
}

plot_tyi2 <- function() {
  tyi <- read.csv('output/tyi_valid.csv', header=TRUE)
  tyi$cue_factor <- factor(tyi$cue_factor, levels=c('male','female','congruent','incongruent'), ordered=T)
  tyi.block <- summarySEwithin(tyi, measurevar='rt', betweenvars=NULL, withinvars=c('cue_factor','block'), idvar='subjCode')
  g <- ggplot(tyi.block, aes(x=block, y=rt, color=cue_factor)) +
    geom_smooth(aes(group=cue_factor), method=lm)
}

quickplot <- function(g) {
  return(g + facet_grid(cueType~.) + 
           scale_x_continuous(breaks=seq(0,20,by=2)) +
           stat_summary(fun.data=mean_se, geom='pointrange') + 
           geom_smooth(method='loess', se=FALSE) +
           geom_hline(yintercept=0, lty=2))}

generate_preds <- function(mod, data) {
  min_ <- round(min(c(min(data$zSound), min(data$zLabel))), 1)
  max_ <- round(max(c(max(data$zSound), max(data$zLabel))), 1)
  typs = seq(min_, max_, by=0.1)
  
  # generate model predictions
  preds_sound <- expand.grid(zSound=typs, zLabel=0.0, cueTypeC=c(-0.5,0.5), delayC=c(-0.5,0.5))
  preds_sound <- cbind(preds_sound, predictSE.mer(mod, preds_sound, type='response', print.matrix=T))
  preds_sound$gradient <- 'soundMatch'
  
  preds_label <- expand.grid(zSound=0.0, zLabel=typs, cueTypeC=c(-0.5,0.5), delayC=c(-0.5,0.5))
  preds_label <- cbind(preds_label, predictSE.mer(mod, preds_label, type='response', print.matrix=T))
  preds_label$gradient <- 'categoryTypicality'
  
  preds <- rbind.data.frame(preds_sound, preds_label)
  
  preds$typicality <- ifelse(preds$gradient == 'soundMatch', preds$zSound, preds$zLabel)
  names(preds)[names(preds) %in% c('fit','se.fit')] <- c('rt','se')
  preds$upr <- preds$rt + preds$se
  preds$lwr <- preds$rt - preds$se
  return(preds)
}

plot_typ <- function(preds) {
  labeller <- function(var, value){
    value <- as.character(value)
    if (var=="gradient") { 
      value[value=="categoryTypicality"] <- "Category Typicality"
      value[value=="soundMatch"]   <- "Source Match"
    } else if (var=="delayC") {
      value[value==-0.5] <- "Simultaneous"
      value[value==0.5] <- "Delayed (400 msec)"
    }
    return(value)
  }
  
  myTheme <- function(base_size = 12, base_family = "", ...){
    modifyList(theme_bw(base_size = base_size, base_family = base_family),
               list(legend.justification=c(1,0), legend.position=c(1,0), legend.title.align=0.5, legend.key=element_blank(),
                    legend.background=element_blank()))
  }
  
  yaxis <- seq(425,775,by=25)
  yaxis <- yaxis[which(yaxis!=575)]
  
  g <- ggplot(preds, aes(x=typicality, y=rt, color=factor(cueTypeC), fill=factor(cueTypeC), linetype=factor(cueTypeC))) +
    geom_smooth(aes(ymin=lwr, ymax=upr), lwd=0.7, stat='identity') +
    facet_grid(delayC ~ gradient, labeller=labeller, scales='free_y') +
    scale_x_continuous('Image Rating (z-scores)') +
    scale_y_continuous('Verification Speed (msec)', breaks=yaxis) + 
    scale_color_manual('Cue Type', labels=c('Label','Sound'), values=c(red,'blue')) +
    scale_linetype_manual('Cue Type', labels=c('Label','Sound'), values=c(1,2)) +
    scale_fill_grey('Cue Type', labels=c('Label','Sound'), start=0.6, end=0.8) +
    myTheme(base_size=30) + theme(legend.background=element_blank(), axis.ticks.length=unit(10, units='points'))
  return(g)
}