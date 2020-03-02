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

old <- theme_set(theme_minimal()) 
theme_set(theme_bw())

theme_update(panel.background = element_rect(fill = "transparent"), # bg of the panel
             plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
             legend.background = element_rect(fill = "transparent"), # get rid of legend bg
             legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
)

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

ggsave("plots/rawRT-friction.png", width = 8, height = 6, units ="in", bg = 'transparent')

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

#logged RT summary
ggplot(df, aes(metaphor, log(RT), fill = metaphor)) +  geom_boxplot(width= 0.3) + 
  facet_grid(cols = vars(friction))
ggsave("plots/rawRT-metaphor.png", width = 8, height = 6, units ="in", bg = 'transparent')

ggplot(df, aes(metaphor, log(RT), fill= friction)) + geom_boxplot()

#logged RT by trial
ggplot(df, aes(as.character(trial), log(RT), fill =trial)) + geom_boxplot( alpha = 0.5, width = 0.3) + 
  facet_grid(cols = vars(metaphor)) +
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="line") + 
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="point") 
ggsave("plots/loggedRT-trial.png", width = 8, height = 6, units ="in", bg = 'transparent')


#logged RT by trial, by participant
ggplot(df %>% filter(friction == "Normal"), aes(trial, log(RT)))  + 
  geom_smooth(aes(group = mTurkCode),method="lm", se=F, alpha = 0.2, size = 0.2)

ggplot(df %>% filter(friction == "Rough"), aes(trial, log(RT)))  + 
  geom_smooth(aes(group = mTurkCode),method="lm", se=F, alpha = 0.2, size = 0.2)

ggplot(df %>% filter(friction == "Smooth"), aes(trial, log(RT)))  + 
  geom_smooth(aes(group = mTurkCode),method="lm", se=F, alpha = 0.2, size = 0.2)


#logged RT by order 
ggplot(df, aes(order, log(RT), fill = order)) + 
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)
ggsave("plots/loggedRT-order.png", width = 8, height = 6, units ="in", bg = 'transparent')

ggplot(df, aes(RT, group = order, fill = friction)) + 
  facet_wrap(vars(friction), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


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


ggplot(df, aes(RT, group = question, fill = friction)) + 
  facet_wrap(vars(friction), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


#each friction seems to have quite different means of target word distance
meanCBT <- df %>% group_by(friction) %>% summarize(mean = mean(charsBeforeTarget))

summary(lm(RT ~ charsBeforeTarget, data = df))

ggplot(df, aes(charsBeforeTarget, log(RT), color = friction)) + geom_jitter(width=5, alpha = 0.4) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black", alpha = 0.7) +
  geom_vline(xintercept = meanCBT$mean, color = c("red", "green", "blue")) +
  geom_smooth(method='lm', color = 1, size = 0.2)
ggsave("plots/loggedRT-charBeforeTarget.png", width = 8, height = 6, units ="in", bg = 'transparent')

#Plot scrolls by group
ggplot(df, aes(friction, log(1+scrolls), fill = metaphor)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(metaphor)) 

#Plot distance by group
ggplot(df, aes(friction, distance, fill = metaphor)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(metaphor)) +
  coord_cartesian(ylim = c(0,300000))

#INFERENTIAL

#RT density distrbutions across groups
p1 <- ggplot(df, aes(RT)) + geom_density() + coord_cartesian(xlim = c(0,300))
p2 <- ggplot(df, aes(RT)) + geom_density() + facet_wrap(friction ~ metaphor, ncol = 2) + coord_cartesian(xlim = c(0,300))
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("plots/group-density.png", plot = p3, width = 8, height = 5, units ="in", bg = 'transparent')

leveneTest(RT ~ metaphor*friction, data = df) # p = 0.5046 


normFit <- fitdist(df$RT, dnorm)
gammaFit <- fitdist(df$RT, dgamma)
lnormFit <- fitdist(df$RT, dlnorm)

plot(normFit)
plot(gammaFit)
plot(lnormFit)

summary(gammaFit)
summary(lnormFit)


ggplot(df, aes(RT)) +  geom_density() + 
  geom_line(aes(x=df$RT, y=dnorm(df$RT, normFit$estimate["mean"], norm$estimate["sd"])), color="green", size = 0.8, alpha = 0.6) +
  #geom_line(aes(x=df$RT, y=dlnorm(df$RT, lnormFit$estimate["meanlog"], lnormFit$estimate["sdlog"])), color="red", size = 0.8, alpha = 0.6) +
  geom_line(aes(x=df$RT, y=dgamma(df$RT,gammaFit$estimate["shape"], gammaFit$estimate["rate"])), color="blue", size = 0.8, alpha = 0.6) +
  coord_cartesian(ylim = c(0,0.03), xlim = c(0,200)) 
ggsave("plots/distr-comparison.png", width = 5, height = 5, units ="in", bg = 'transparent')

df <- df %>% mutate(charsBeforeTargetscaled = scale(charsBeforeTarget), trialScaled = scale(trial)) 

#model fitting
m0 <- glmer(RT ~  friction*metaphor + charsBeforeTargetscaled + (friction|mTurkCode) + (trialScaled|mTurkCode), data = df, 
            family = Gamma(link = "identity"), 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#tests
m <- m0


#fitted against residuals
plot(fitted(m), residuals(m), xlab = "Fitted Values", ylab = "Residuals", col = "grey")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)))

summary(m)
Anova(m)

cor(df$RT, fitted(m))^2

ggplot(m) + geom_histogram(aes(.resid), bins = 30)

plot_model(m, type = "eff")
pairs(emmeans(m,c("metaphor",'friction')), by = "friction")
plot(emmeans(m, c("metaphor", "friction")), comparisons = TRUE, by = "friction")

