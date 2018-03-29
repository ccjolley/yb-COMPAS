library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)

d <- read.csv('data/compas-scores-two-years.csv')

# First, look at race and decile score

X1 <- d %>% select(race,decile_score)
X1$race <- factor(X1$race,levels(X1$race)[c(1,3,4,2,5,6)])
delta <- 0.35
X1_m <- X1 %>% group_by(race) %>% 
  summarize(m=mean(decile_score)) 
X1_m$xmin <- (1:6)-delta
X1_m$xmax <- (1:6)+delta
ggplot(X1,aes(x=race,y=decile_score)) +
  geom_jitter(size=2,shape=1,color='cornflowerblue',alpha=0.25) +
  geom_segment(data=X1_m,aes(x=xmin,xend=xmax,y=m,yend=m),color='red',size=2) +
  scale_y_continuous(breaks=2*(1:5)) +
  xlab('Race') + ylab('Decile Score')

# Next, compare decile score and actual recidivism

X2 <- d %>% select(decile_score,two_year_recid,race)
ggplot(X2,aes(x=decile_score,y=two_year_recid)) +
  geom_jitter(size=2,shape=1,color='cornflowerblue',alpha=0.25) +
  xlab('Decile Score') + ylab('Two-year recidivism')

# might be clearer as a line plot

X3 <- X2 %>% select(decile_score,two_year_recid) %>%
  table %>% 
  as.data.frame 
ggplot(X3,aes(x=decile_score,y=Freq,group=two_year_recid,color=two_year_recid)) +
  geom_line()
# Overall, if a person had a score of 6 or higher they were more likely to re-offend than not to.

# Error rates
hi95 <- function(x) { binom.test(sum(x),length(x),0.5)$conf.int[2] }  
lo95 <- function(x) { binom.test(sum(x),length(x),0.5)$conf.int[1] }  
X4 <- X2 %>% 
  mutate(race=as.character(race),
         race=ifelse(race=='Asian','Other',race),
         race=ifelse(race=='Native American','Other',race)) %>%
  mutate(tn=(decile_score < 6) & (two_year_recid==0),
         fn=(decile_score < 6) & (two_year_recid==1),
         tp=(decile_score > 5) & (two_year_recid==1),
         fp=(decile_score > 5) & (two_year_recid==0)) %>% 
  group_by(race) %>%
  summarize(fn_m=mean(fn),fp_m=mean(fp),fn_lo=lo95(fn),fn_hi=hi95(fn),fp_lo=lo95(fp),fp_hi=hi95(fp),
            tn_m=mean(tn),tp_m=mean(tp),tn_lo=lo95(tn),tn_hi=hi95(tn),tp_lo=lo95(tp),tp_hi=hi95(tp))

plotme <- rbind(X4 %>% select(race,fn_m,fn_lo,fn_hi) %>%
                  rename(m=fn_m,lo=fn_lo,hi=fn_hi) %>%
                  mutate(group='fn'),
                X4 %>% select(race,fp_m,fp_lo,fp_hi) %>%
                  rename(m=fp_m,lo=fp_lo,hi=fp_hi) %>%
                  mutate(group='fp'))
ggplot(plotme,aes(x=race,y=m,group=group,fill=group)) +
  geom_bar(stat='identity',position='stack') +
  xlab('Race') + ylab('Error rates')
# Overall error rates comparabale for Blacks, Whites, and Hispanics

ggplot(plotme %>% filter(group=='fn'),
       aes(x=fct_reorder(race,m),y=m)) +
  geom_bar(stat='identity',position='dodge',fill='goldenrod') +
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.5,color='gray40') +
  xlab('Race') + ylab('Error rates') +
  ggtitle('False negatives')

ggplot(plotme %>% filter(group=='fp'),
       aes(x=fct_reorder(race,m),y=m)) +
  geom_bar(stat='identity',position='dodge',fill='goldenrod') +
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.5,color='gray40') +
  xlab('Race') + ylab('Error rates') +
  ggtitle('False positives')

# Now this is getting interesting... need error bars on the overall rates

plotme2 <- rbind(X4 %>% select(race,tn_m,tn_lo,tn_hi) %>%
                  rename(m=tn_m,lo=tn_lo,hi=tn_hi) %>%
                  mutate(group='tn'),
                X4 %>% select(race,tp_m,tp_lo,tp_hi) %>%
                  rename(m=tp_m,lo=tp_lo,hi=tp_hi) %>%
                  mutate(group='tp'))

ggplot(plotme2 %>% filter(group=='tn'),
       aes(x=fct_reorder(race,m),y=m)) +
  geom_bar(stat='identity',position='dodge',fill='seagreen2') +
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.5,color='gray40') +
  xlab('Race') + ylab('Match rates') +
  ggtitle('True negatives')

ggplot(plotme2 %>% filter(group=='tp'),
       aes(x=fct_reorder(race,m),y=m)) +
  geom_bar(stat='identity',position='dodge',fill='seagreen2') +
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.5,color='gray40') +
  xlab('Race') + ylab('Match rates') +
  ggtitle('True positives')

# What about real recidivism rates by race?

X2 %>% group_by(race) %>%
  summarize(m=mean(two_year_recid),lo=lo95(two_year_recid),hi=hi95(two_year_recid)) %>%
  ggplot(aes(x=fct_reorder(race,m),y=m)) +
    geom_bar(stat='identity',fill='seagreen2') +
    geom_errorbar(aes(ymin=lo,ymax=hi),width=0.5,color='gray40') +
    xlab('Race') + ylab('Two-year recidivism rate')
