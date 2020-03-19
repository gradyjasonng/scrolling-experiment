library(dplyr)
library(lme4)
library(car)
library(MASS)
library(ggplot2)
library(qqplotr)
library(emmeans)
library(sjPlot)
library(gridExtra)
library(fitdistrplus)
library(dfoptim)

df <- readRDS("./cleaned.rds")

#set ylim within 1.6x of outlier cutoff
ylim1 = boxplot.stats(df$RT)$stats[c(1, 5)]*1.6

#theme setting - for export purposes
old <- theme_set(theme_minimal()) 
theme_set(theme_bw())

theme_update(panel.background = element_rect(fill = "transparent"), # bg of the panel
             plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
             legend.background = element_rect(fill = "transparent"), # get rid of legend bg
             legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
)

#DESCRIPTIVE

#raw RT by metaphor 
ggplot(df, aes(metaphor, RT, fill= metaphor)) + geom_boxplot(width = 0.4) + 
  coord_cartesian(ylim = ylim1) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ metaphor, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  geom_text(data = aggregate(RT~ metaphor, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 140), nudge_x = 0.25) +
  geom_text(data = aggregate(RT~ metaphor, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0)) 

ggsave("plots/rawRT-metaphor.png", width = 6, height = 7, units ="in", bg = 'transparent')

#raw RT by friction 
ggplot(df, aes(friction, RT, fill= friction)) + geom_boxplot(width = 0.4) + 
  coord_cartesian(ylim = ylim1) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ friction, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  geom_text(data = aggregate(RT~ friction, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 140), nudge_x = 0.25) +
  geom_text(data = aggregate(RT~ friction, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0)) 

ggsave("plots/rawRT-friction.png", width = 7, height = 5, units ="in", bg = 'transparent')


#raw RT by groups - friction
ggplot(df, aes(friction, RT, fill= friction)) + geom_boxplot(width = 0.4) + 
  coord_cartesian(ylim = ylim1) +
  facet_grid(cols = vars(metaphor)) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ metaphor*friction, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  geom_text(data = aggregate(RT~ metaphor*friction, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 145), nudge_x = 0, size = 3) +
  geom_text(data = aggregate(RT~ friction, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0)) 

ggsave("plots/rawRT-groups-friction.png", width = 8, height = 6, units ="in", bg = 'transparent')

#raw RT by groups - metaphor
ggplot(df, aes(metaphor, RT, fill = metaphor)) +  geom_boxplot(width = 0.4) + 
  facet_grid(cols = vars(friction)) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ metaphor*friction, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  coord_cartesian(ylim = ylim1) +
  geom_text(data = aggregate(RT~ metaphor*friction, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 145), nudge_x = 0, size=3) +
  geom_text(data = aggregate(RT~ metaphor*friction, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0))
ggsave("plots/rawRT-groups-metaphor.png", width = 8, height = 6, units ="in", bg = 'transparent')

#logged RT by groups - metaphor
ggplot(df, aes(metaphor, log(RT), fill = metaphor)) +  geom_boxplot(width= 0.3) + 
  facet_grid(cols = vars(friction))
ggsave("plots/logRT-groups-metaphor.png", width = 8, height = 6, units ="in", bg = 'transparent')

#logged RT by trial
ggplot(df, aes(as.character(trial), log(RT), fill =trial)) + geom_boxplot( alpha = 0.5, width = 0.3) + 
  facet_grid(cols = vars(metaphor)) +
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="line") + 
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="point") 
ggsave("plots/loggedRT-trial.png", width = 8, height = 6, units ="in", bg = 'transparent')


#logged RT by order 
ggplot(df, aes(order, log(RT), fill = order)) + 
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)
ggsave("plots/loggedRT-order.png", width = 8, height = 6, units ="in", bg = 'transparent')

#logged RT by question - lots of variance here
ggplot(df, aes(question, log(RT), fill = friction)) + 
  facet_wrap(vars(friction), scales = "free_x") +
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)
ggsave("plots/loggedRT-question.png", width = 8, height = 6, units ="in", bg = 'transparent')

#logged RT by question - ordered by target char distance
ggplot(df, aes(reorder(question, charsBeforeTarget), log(RT), fill = friction)) + 
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F) +
  labs(x = "Questions (Ordered by increasing target distance)") 
ggsave("plots/loggedRT-question-ordered.png", width = 8, height = 6, units ="in", bg = 'transparent')

meanCBT <- df %>% group_by(friction) %>% summarize(mean = mean(charsBeforeTarget))

#mean characters before target by friction
ggplot(meanCBT, aes(friction, mean)) + geom_line(group= 1) + geom_point(group =1) +
  coord_cartesian(ylim= c(800,1400))
ggsave("plots/CBT-friction.png", width = 4, height = 5, units ="in", bg = 'transparent') 

#log RT by friction 
ggplot(df, aes(friction, log(RT), fill= friction)) + geom_boxplot(width = 0.4) + 
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black")
ggsave("plots/logRT-friction.png", width = 4, height = 5, units ="in", bg = 'transparent')

#correlation - confound found
summary(lm(RT ~ charsBeforeTarget, data = df))

#more informative summary of confound
ggplot(df, aes(charsBeforeTarget, log(RT), color = friction)) + geom_jitter(width=5, alpha = 0.4) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black", alpha = 0.7) +
  geom_vline(xintercept = meanCBT$mean, color = c("red", "green", "blue")) +
  geom_smooth(method='lm', color = 1, size = 0.2)
ggsave("plots/loggedRT-charBeforeTarget.png", width = 8, height = 6, units ="in", bg = 'transparent')


#line graph versions
ggplot(df,aes(metaphor, RT, color = friction)) +
  stat_summary(aes(y = RT,group=friction),fun.y=median, geom="line", size=1) +
  stat_summary(aes(y = RT,group=friction),fun.y=median, geom="point", size=2) +
  coord_cartesian(ylim = c(0,40))
ggsave("plots/rawRT-line.png", width = 8, height = 6, units ="in", bg = 'transparent')

ggplot(df,aes(friction, RT, color = metaphor)) +
  stat_summary(aes(y = RT,group=metaphor),fun.y=median, geom="line", size=1) +
  stat_summary(aes(y = RT,group=metaphor),fun.y=median, geom="point", size=2) +
  coord_cartesian(ylim = c(0,40))
ggsave("plots/rawRT-line2", width = 8, height = 6, units ="in", bg = 'transparent')

#INFERENTIAL


#RESPONSE TIMES

#RT density distrbutions across groups
p1 <- ggplot(df, aes(RT)) + geom_density() + coord_cartesian(xlim = c(0,300))
p2 <- ggplot(df, aes(RT)) + geom_density() + facet_wrap(friction ~ metaphor, ncol = 2) + coord_cartesian(xlim = c(0,300))
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("plots/group-density.png", plot = p3, width = 8, height = 5, units ="in", bg = 'transparent')

leveneTest(RT ~ metaphor*friction, data = df) # p = 0.5046 

#eh, looks pretty gamma
ggplot(df, aes(RT)) + geom_density(alpha = 0.2) 

normFit <- fitdist(df$RT, dnorm)
gammaFit <- fitdist(df$RT, dgamma)
lnormFit <- fitdist(df$RT, dlnorm)

plot(normFit)
plot(gammaFit)
plot(lnormFit)

#lnorm fits slightly better, but gamma is good enough, and easier to implement in glmer()
summary(gammaFit)
summary(lnormFit)

#preliminary curve fits
ggplot(df, aes(RT)) +  geom_density() + 
  geom_line(aes(x=df$RT, y=dnorm(df$RT, normFit$estimate["mean"], normFit$estimate["sd"])), color="red", size = 0.9, alpha = 0.6) +
  geom_line(aes(x=df$RT, y=dlnorm(df$RT, lnormFit$estimate["meanlog"], lnormFit$estimate["sdlog"])), color="green", size = 0.8, alpha = 0.6) +
  geom_line(aes(x=df$RT, y=dgamma(df$RT,gammaFit$estimate["shape"], gammaFit$estimate["rate"])), color="blue", size = 0.9, alpha = 0.6) +
  coord_cartesian(ylim = c(0,0.03), xlim = c(0,200)) 
ggsave("plots/distr-comparison.png", width = 5, height = 5, units ="in", bg = 'transparent')


#model fitting
m1 <- glmer(RT ~  friction*metaphor*trialScaled + charsBeforeTargetscaled + (friction|mTurkCode) + (trialScaled|mTurkCode), data = df, 
            family = Gamma(link = "identity"), 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#fitted against residuals 
plot(fitted(m), residuals(m), xlab = "Fitted Values", ylab = "Residuals", col = "grey")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)))

summary(m1)
Anova(m1)
# friction                        1.1781  2    0.55484    
# metaphor                        4.3240  1    0.03758 *  
# trialScaled                     0.5404  1    0.46227    
# charsBeforeTargetscaled       201.9830  1    < 2e-16 ***
# friction:metaphor               0.0026  2    0.99871    
# friction:trialScaled            0.4317  2    0.80585    
# metaphor:trialScaled            0.6444  1    0.42213    
# friction:metaphor:trialScaled   1.3634  2    0.50575    

cor(df$RT, fitted(m))^2

#normal looking residuals
ggplot(m) + geom_histogram(aes(.resid), bins = 30)


#plots pred values of RT
plot_model(m, type = "eff", terms = c("friction","metaphor"), ci.lvl = 0.95, order.terms = c(3,1,2)
           ) + coord_cartesian(ylim = c(0,60)) + geom_line()
ggsave("plots/pred-RT.png", width = 5, height = 5, units ="in", bg = 'transparent')

#posthocs
emmeans(m,c("metaphor"))
emmeans(m,c("friction"))
pairs(emmeans(m,c("metaphor",'friction')), by = "friction")
plot(emmeans(m, c("metaphor", "friction")), comparisons = TRUE, by = "metaphor")

#SCROLLS

m2 <- glmer(scrolls ~  friction*metaphor*trial + charsBeforeTargetscaled + (friction|mTurkCode), data = df, 
              family = poisson(link = "log"), 
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#trial as categorical
m2.1 <- glmer(scrolls  ~  friction*metaphor*trial + charsBeforeTargetscaled + (friction|mTurkCode), 
              data = df%>% mutate(trial = as.character(trial)), 
            family = poisson(link = "log"), 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


summary(m2)
Anova(m2)
# friction                 113.9807  2  < 2.2e-16 ***
# metaphor                   1.7273  1   0.188751    
# trial                      0.5583  1   0.454941    
# charsBeforeTargetscaled 2289.7054  1  < 2.2e-16 ***
# friction:metaphor          0.6293  2   0.730045    
# friction:trial             1.4932  2   0.473967    
# metaphor:trial            68.9746  1  < 2.2e-16 ***
# friction:metaphor:trial   12.6539  2   0.001787 ** 

Anova(m2.1) 
# friction                               113.0183  2     <2e-16 ***
# metaphor                                 1.6833  1     0.1945    
# as.character(trial)                    102.7573  5     <2e-16 ***
# charsBeforeTargetscaled               2194.3529  1     <2e-16 ***
# friction:metaphor                        0.5106  2     0.7747    
# friction:as.character(trial)           188.1338 10     <2e-16 ***
# metaphor:as.character(trial)           101.0297  5     <2e-16 ***
# friction:metaphor:as.character(trial)  324.9468 10     <2e-16 ***

cor(df$scrolls, fitted(m2.1))^2
ggplot(m2) + geom_histogram(aes(.resid), bins = 30)

#significant effects when looking at scrolls and distance likely show that the participants adapted their behaviour quickly to match, hence similar RTs

#predicted scroll counts
plot_model(m2, type = "eff", terms = c("friction"), ci.lvl = 0.95, order.terms = c(3,1,2)
)  + geom_line()
ggsave("plots/pred-scrolls.png", width = 5, height = 5, units ="in", bg = 'transparent')

#predicted scrolls
plot_model(m2, type =  "eff", terms = c("trial", "metaphor"))
plot_model(m2, type =  "eff", terms = c("trial", "metaphor", "friction"))

#predicted scrolls - trial as categorical - seems a little noisy, but it does seem to suggest the trends shown above?
plot_model(m2.1, type =  "eff", terms = c("trial", "metaphor", "friction"), ci.lvl = 0) 
+ geom_smooth(method ='lm', se = F) # manually fitting a line to predicted values

#emmeans back-transformed 
regrid(emmeans(m2,c("metaphor")))
regrid(emmeans(m2,c("friction")))

#all pairwise slope comparisons are significant 
pairs(emtrends(m2, c("metaphor","friction"), "trial", by = "friction"))

#SPEED

ggplot(df, aes(distance/RT)) + geom_density()

m3 <- glmer(1+ distance/RT ~  friction*metaphor*trial + charsBeforeTargetscaled + (friction|mTurkCode), data = df, 
              family = Gamma(link = "log"), 
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(m3)
Anova(m3) 
# friction                16.0195  2  0.0003322 ***
# metaphor                 2.2561  1  0.1330907    
# trial                    5.0885  1  0.0240856 *  
# charsBeforeTargetscaled 49.6229  1  1.863e-12 ***
# friction:metaphor        0.1017  2  0.9504272    
# friction:trial          21.2955  2  2.375e-05 ***
# metaphor:trial           1.7655  1  0.1839428    
# friction:metaphor:trial  0.1443  2  0.9303818 

plot_model(m3, type = "eff", terms = c("friction","metaphor"), ci.lvl = 0.95, order.terms = c(3,1,2)
)  + geom_line()
ggsave("plots/pred-speed.png", width = 5, height = 5, units ="in", bg = 'transparent')

pairs(emtrends(m3, c("friction"), "trial")) #marginally significant/ significant differences in slopes
plot_model(m3, type = "eff", terms = c("trial", "friction")) 
#metaphor did not have a significant effect on scan speed over time, but friction does. 
#seems to converge over time, perhaps participants adapt to find a universally optimal speed? After all, theres only so much content that can be scanned at once.

regrid(emmeans(m3,c("metaphor")))
regrid(emmeans(m3,c("friction")))

#DISTANCE - don't know how to intepret this conceptually but I'll leave it here for now

m3.1 <- glmer(1+distance ~  friction*metaphor*trial + charsBeforeTargetscaled + (friction|mTurkCode), data = df, 
            family = Gamma(link = "log"), 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(m3.1)
Anova(m3.1) 

plot_model(m3.1, type = "eff", terms = c("friction","metaphor"), ci.lvl = 0.95, order.terms = c(3,1,2)
)  + geom_line()
ggsave("plots/pred-dist.png", width = 5, height = 5, units ="in", bg = 'transparent')

plot_model(m3.1, type = "eff", terms = c("trial", "friction"))

emtreads

emmeans(m3.1,c("metaphor"))
emmeans(m3.1,c("friction"))
pairs(emmeans(m3.1,c("metaphor",'friction')), by = "friction")



#MISC


#first v last trial RTs
ggplot(df %>% filter(trial == 1 | trial == 6), aes(as.character(trial), RT, fill = metaphor)) +
  stat_summary(aes(group=metaphor, colour = metaphor), fun.y=median, geom="line") + 
  stat_summary(aes(group=metaphor, colour = metaphor), fun.y=median, geom="point") +
  coord_cartesian(ylim = c(20,30))


#RT by trial
ggplot(df, aes(trial, log(RT), fill = metaphor)) +
  geom_jitter(aes(color = metaphor),alpha = 0.3,width=0.2)+
  geom_smooth(method=lm)
stat_summary(aes(group=metaphor, colour = metaphor), fun.y=median, geom="line")

summary(lm(RT ~ trial, data = df))

#compare native RTs by order of presentation
ggplot(df %>% filter(friction == "Native"), aes(block, log(RT), fill = metaphor)) + geom_boxplot()

#compare RTs across blocks
ggplot(df , aes(block, RT, group = metaphor)) +
  stat_summary(aes(colour = metaphor), fun.y=median, geom="line")

