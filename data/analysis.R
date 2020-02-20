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

#RT SUMMARY GRAPHS - ZOOMED
ylim1 = boxplot.stats(df$RT)$stats[c(1, 5)]*1.6

ggplot(df, aes(condition, RT, fill= condition)) + geom_boxplot(width = 0.4) + 
  coord_cartesian(ylim = ylim1) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ condition, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  geom_text(data = aggregate(RT~ condition, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers\nnot shown")), 
            aes(label = RT, y = 140), nudge_x = 0.25) +
  geom_text(data = aggregate(RT~ condition, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0))

ggplot(df, aes(condition, RT, fill = condition)) +  geom_boxplot(width = 0.4) + 
  facet_grid(cols = vars(texture)) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=1, color="black", fill="black") +
  geom_text(data = aggregate(RT ~ condition*texture, df, median) %>% mutate(RT = round(RT,1)), aes(label = RT, y = RT), nudge_y = 3) +
  coord_cartesian(ylim = ylim1) +
  geom_text(data = aggregate(RT~ condition*texture, df, function(x) paste(sum(!between(x, ylim1[1], ylim1[2])), "outliers not shown")), 
            aes(label = RT, y = 145), nudge_x = 0, size=3) +
  geom_text(data = aggregate(RT~ condition*texture, df, function(x) paste("n =", length(x))), aes(label = RT, y = 0))

#RT SUMMARY GRAPHS - LOGGED
ggplot(df, aes(condition, log(RT), fill = condition)) +  geom_boxplot(width= 0.3) + 
  facet_grid(cols = vars(texture))

ggplot(df, aes(condition, log(RT), fill= texture)) + geom_boxplot()

#RT BY TRIAL - LOGGED
ggplot(df, aes(as.character(trial), log(RT), fill =trial)) + geom_boxplot( alpha = 0.5, width = 0.3) + 
  facet_grid(cols = vars(condition)) +
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="line") + 
  stat_summary(aes(y = log(RT),group=1), fun.y=median, colour="red", geom="point") 

#RT BY ORDER - LOGGED 
ggplot(df, aes(order, log(RT), fill = order)) + 
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)

ggplot(df, aes(RT, group = order, fill = texture)) + 
  facet_wrap(vars(texture), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


#RT BY QUESTION - LOGGED - lots of variance here
ggplot(df, aes(question, log(RT), fill = texture)) + 
  facet_wrap(vars(texture), scales = "free_x") +
  geom_boxplot(size = 0.5, width = 0.3, show.legend = F)

ggplot(df, aes(RT, group = question, fill = texture)) + 
  facet_wrap(vars(texture), nrow = 3) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0,200))


#SCROLLS
ggplot(df, aes(texture, log(1+scrolls), fill = condition)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(condition)) 

#DISTANCE
ggplot(df, aes(texture, distance, fill = condition)) + 
  geom_boxplot(show.legend = F) +
  facet_grid(cols = vars(condition)) +
  coord_cartesian(ylim = c(0,300000))

#INFERENTIAL

#RT DENSITY DISTRIBUTIONS
p1 <- ggplot(df, aes(RT)) + geom_density() + coord_cartesian(xlim = c(0,300))
p2 <- ggplot(df, aes(RT)) + geom_density() + facet_wrap(texture ~ condition, ncol = 2) + coord_cartesian(xlim = c(0,300))
grid.arrange(p1,p2, nrow=1)

leveneTest(RT ~ condition*texture, data = df)

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


#model for individual changes over texture - random slope - best fit atm
m1 <- glmer(RT ~ texture*condition + (texture|mTurkCode) + 
              (1|question), data = df, 
            family = Gamma(link = "identity"), verbose = 0,
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


#just other stuff - random intercept, log norm 
m2 <- glmer(RT ~ texture*condition + (1|mTurkCode), data = df, family = Gamma(link = "identity"), verbose = T)

mlnorm1 <- glmer(RT ~ texture*condition + (1|mTurkCode) + 
              (1|question), data = df, 
            family = gaussian(link = "log"), verbose = 0,
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


mlnorm <- glmer(RT ~ texture*condition + (1|mTurkCode) , data = df, 
                family = gaussian(link = "log"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))



m <- m1


plot_model(m, show.values = TRUE, value.offset = .3)

plot(fitted(m), residuals(m), xlab = "Fitted Values", ylab = "Residuals", col = "grey")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(m), residuals(m)))
ggplot(df, aes(log(RT), fill=condition)) + geom_density(size = 0.5, alpha=0.5) 
ggplot(df, aes(log(RT), fill=texture)) + geom_density(size = 0.5, alpha=0.5) 


summary(m)
Anova(m)
pairs(emmeans(m, c("texture","condition")))
contrast(regrid(emmeans(m, "texture")))
contrast(regrid(emmeans(m, "condition")))

plot(emmeans(m, c("condition")), comparisons = TRUE)
