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

df <- readRDS("./cleaned.rds")

#set ylim within 1.6x of outlier cutoff
ylim1 = boxplot.stats(df$RT)$stats[c(1, 5)]*1.6

#raw RT by metaphor 
ggplot(df, aes(metaphor, RT, fill= metaphor)) + geom_boxplot(width = 0.4) + 
  coord_cartesian(ylim = ylim1) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ metaphor, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  geom_text(data = aggregate(RT~ metaphor, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 140), nudge_x = 0.25) +
  geom_text(data = aggregate(RT~ metaphor, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0))


#raw RT by groups
ggplot(df, aes(metaphor, RT, fill = metaphor)) +  geom_boxplot(width = 0.4) + 
  facet_grid(cols = vars(texture)) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ metaphor*texture, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  coord_cartesian(ylim = ylim1) +
  geom_text(data = aggregate(RT~ metaphor*texture, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers not shown")), 
            aes(label = RT, y = 145), nudge_x = 0, size=3) +
  geom_text(data = aggregate(RT~ metaphor*texture, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0))

#logged RT summary
ggplot(df, aes(metaphor, log(RT), fill = metaphor)) +  geom_boxplot(width= 0.3) + 
  facet_grid(cols = vars(texture))

ggplot(df, aes(metaphor, log(RT), fill= texture)) + geom_boxplot()

#logged RT by trial
ggplot(df, aes(as.character(trial), log(RT), fill =trial)) + geom_boxplot( alpha = 0.5, width = 0.3) + 
  facet_grid(cols = vars(metaphor)) +
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="line") + 
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="point") 

#logged RT by order 
ggplot(df, aes(order, log(RT), fill = order)) + 
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)

ggplot(df, aes(RT, group = order, fill = texture)) + 
  facet_wrap(vars(texture), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


#logged RT by question - lots of variance here
ggplot(df, aes(question, log(RT), fill = texture)) + 
  facet_wrap(vars(texture), scales = "free_x") +
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)

ggplot(df, aes(RT, group = question, fill = texture)) + 
  facet_wrap(vars(texture), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


#Plot scrolls by group
ggplot(df, aes(texture, log(1+scrolls), fill = metaphor)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(metaphor)) 

#Plot distance by group
ggplot(df, aes(texture, distance, fill = metaphor)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(metaphor)) +
  coord_cartesian(ylim = c(0,300000))

#INFERENTIAL

#RT density distrbutions across groups
p1 <- ggplot(df, aes(RT)) + geom_density() + coord_cartesian(xlim = c(0,300))
p2 <- ggplot(df, aes(RT)) + geom_density() + facet_wrap(texture ~ metaphor, ncol = 2) + coord_cartesian(xlim = c(0,300))
grid.arrange(p1,p2, nrow=1)

leveneTest(RT ~ metaphor*texture, data = df) # p = 0.5046 

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
  geom_line(aes(x=df$RT, y=dlnorm(df$RT, lnormFit$estimate["meanlog"], lnormFit$estimate["sdlog"])), color="red", size = 0.8, alpha = 0.6) +
  geom_line(aes(x=df$RT, y=dgamma(df$RT,gammaFit$estimate["shape"], gammaFit$estimate["rate"])), color="blue", size = 0.8, alpha = 0.6) +
  coord_cartesian(ylim = c(0,0.03), xlim = c(0,200)) 


#model fitting
m1 <- glmer(RT ~ texture*metaphor + (texture|mTurkCode) + 
              (1|question), data = df, 
            family = Gamma(link = "identity"), verbose = 0,
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


#tests
m <- m1

#plot estimated effects
plot_model(m, show.values = TRUE, value.offset = .3)


#fitted against residuals
plot(fitted(m), residuals(m), xlab = "Fitted Values", ylab = "Residuals", col = "grey")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)))

summary(m)
Anova(m)
contrast(emmeans(m, "texture"))
contrast(emmeans(m, "metaphor"))

plot(emmeans(m, c("metaphor")), comparisons = TRUE)
