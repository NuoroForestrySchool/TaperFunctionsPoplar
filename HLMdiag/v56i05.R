################################################################################
###  preliminaries
################################################################################
library("HLMdiag")  # note that this will load lme4
library("nullabor") # used to construct lineups
library("plyr")     # used to construct school-level data set
library("ggplot2")  # used for plots

## The Exam example is bundled with mlmRev
data("Exam", package = "mlmRev")
head(Exam)

################################################################################
###  Model fm1 on page 6
################################################################################
(fm1 <- lmer(normexam ~ standLRT + (1 | school), Exam, REML = FALSE))

## Extract level-1 residuals
resid1_fm1 <- HLMresid(fm1, level = 1, type = "LS", standardize = TRUE)
head(resid1_fm1)

## LS level-1 residuals vs fitted values (Not in the paper)
qplot(x = fitted, y = LS.resid, data = resid1_fm1, geom = c("point", "smooth")) +
  ylab("LS level-1 residuals") + xlab("fitted values")

################################################################################
## Figure 1, page 7
################################################################################
qplot(x = standLRT, y = LS.resid, data = resid1_fm1, geom = c("point", "smooth")) + 
  ylab("LS level-1 residuals")

## Comparing alternatives to fm1
fm1a <- lmer(normexam ~ standLRT + I(standLRT^2) + (1 | school), 
  Exam, REML = FALSE)
fm1b <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + (1 | school), 
  Exam, REML = FALSE)

anova(fm1, fm1a, fm1b) 

################################################################################
## Model fm2 on page 7
################################################################################

## The model with the quadratic and cubic terms is preferred, 
## and we rename it fm2 for clarity in the paper.
fm2 <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + 
  (1 | school), Exam, REML = FALSE)


## Extract level-1 residuals
resid1_fm2 <- HLMresid(fm2, level = 1, type = "LS", standardize = "semi")
head(resid1_fm2)


################################################################################
## Figure 2, page 8
################################################################################
qplot(x = `I(standLRT^2)`, y = semi.std.resid, data = resid1_fm2) + 
  geom_smooth(method = "lm") + 
  ylab("semi-standardized residuals") + 
  xlab("standLRT2")

################################################################################
## Figure 3, page 9 -- this code is shown in appendix A.1
################################################################################
## Simulating null data
set.seed(1234)
sim_fm2 <- simulate(fm2, nsim = 19)

## Refit
refit_fm2 <- apply(sim_fm2, 2, refit, object = fm2)

## Extract level-1 residuals
sim_fm2_lev1_resid <- ldply(refit_fm2, function(x){
  HLMresid(object = x, level = 1, type = "LS", sim = x@resp$y, standardize = "semi")
})

## Labeling for nullabor
sim_fm2_lev1_resid$.n <- rep(1:19, each = 4059)
names(sim_fm2_lev1_resid)[4:5] <- c("standLRT2", "standLRT3")

## Formatting for nullabor
lev1_resid_fm2 <- HLMresid(object = fm2, level = 1, type = "LS", standardize = "semi")
names(lev1_resid_fm2)[3:4] <- c("standLRT2", "standLRT3")
class(lev1_resid_fm2[,3])  <- "numeric"

## Creating lineup
qplot(standLRT2, semi.std.resid, data = lev1_resid_fm2,
  geom = "point", alpha = I(0.3)) %+%
  lineup(true = lev1_resid_fm2, samples = sim_fm2_lev1_resid) +
  facet_wrap(~ .sample, ncol = 4) +
  geom_hline(aes(yintercept = 0), colour = I("red")) + 
  ylab("semi-standardized residuals")

################################################################################
## Figure 4, page 10
################################################################################
ssresid <- na.omit(resid1_fm2$semi.std.resid)
ggplot_qqnorm(x = ssresid, line = "rlm")

################################################################################
## Model fm3, page 11
################################################################################
fm3 <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + sex +
  (standLRT | school), Exam, REML = FALSE)

## Extract level-2 EB residuals
resid2_fm3 <- HLMresid(object = fm3, level = "school")
head(resid2_fm3)

################################################################################
## Figure 5, page 13
################################################################################
## Construct school-level data set
SchoolExam <- ddply(Exam, .(school), summarise, size = length(school),
  schgend = unique(schgend), schavg = unique(schavg),
  type = unique(type), schLRT = mean(standLRT))

## Left panel -- figure 5
qplot(x = reorder(SchoolExam$schgend, resid2_fm3[,1], median), 
  y = resid2_fm3[,1], geom = 'boxplot', 
  xlab = 'school gender', ylab = 'level-2 residual (Intercept)')

## Right panel -- figure 5
qplot(x = schavg, y = resid2_fm3[,1], data = SchoolExam, 
  geom = c("point", "smooth"), 
  xlab = "average intake score", ylab = "level-2 residual (Intercept)")

## Testing utility of omitted variables
fm3a <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + sex +
  schgend + (standLRT | school), Exam, REML = FALSE)
fm3b <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + sex +
  schgend + schavg + (standLRT | school), Exam, REML = FALSE)

anova(fm3, fm3a, fm3b)


################################################################################
## Model fm4, page 12
################################################################################

## From above we found both schgend and schavg to improve the model,
## and we rename the improved model fm4 for clarity
fm4 <- lmer(normexam ~ standLRT + I(standLRT^2) + I(standLRT^3) + sex + 
  schgend + schavg + (standLRT | school), data = Exam, REML = FALSE)

## Extract level-2 EB residuals
resid2_fm4 <- HLMresid(fm4, level = "school")

################################################################################
## Figure 6, page 13
################################################################################
## Left panel
ggplot_qqnorm(resid2_fm4$`(Intercept)`, line = "rlm")

## Right panel
ggplot_qqnorm(resid2_fm4$standLRT, line = "rlm")


################################################################################
## Diagnostics for changes in the fixed effects parameter values
################################################################################

## Calculate Cook's distance for model fm4
cooksd_fm4  <- cooks.distance(fm4, group = "school")

## Calculate MDFFITS for model fm4
mdffits_fm4 <- mdffits(fm4, group = "school")

## Figure 7 - left panel
dotplot_diag(x = cooksd_fm4, cutoff = "internal", 
             name = "cooks.distance") + 
  ylab("Cook's distance") + xlab("school")

## Figure 7 - right panel
dotplot_diag(x = cooksd_fm4, cutoff = "internal", name = "cooks.distance", 
  modify = "dotplot") + ylab("Cook's distance") + xlab("school")

## Change in the fixed effects parameter vector when school 25 is deleted
beta_cdd25 <- as.numeric(attr(cooksd_fm4, "beta_cdd")[[25]])
names(beta_cdd25) <- names(fixef(fm4))
beta_cdd25

################################################################################
## Diagnostics for changes in precision of the fixed effects parameter values
################################################################################

## Calculate covariance ratio for model fm4
covratio_fm4 <- covratio(fm4, group = "school")

## Calculate covariance trace for model fm4
covtrace_fm4 <- covtrace(fm4, group = "school")

################################################################################
## Diagnostics for variance components
################################################################################

## Calculate relative variance change for model fm4
rvc_fm4 <- rvc(fm4, group = "school")
head(rvc_fm4)


## Figure 8
dotplot_diag(rvc_fm4[, "D22"], cutoff = "internal", name = "rvc", modify = "boxplot") + 
  xlab("school") + ylab("RVC")

################################################################################
## Leverage
################################################################################

## Calculate leverage for model fm4
leverage_fm4 <- leverage(fm4, level = "school")
head(leverage_fm4)
