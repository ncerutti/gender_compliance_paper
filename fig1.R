library(readxl)
library(ggplot2)
library(reshape2)

exp2 <- read_excel("C:/Users/Nico/OneDrive/papers/Covid/covid-3/exp2.xls")

colnames(exp2)[colnames(exp2) == "draws"] ="drawsT"
colnames(exp2)[colnames(exp2) == "femaledraws"] ="drawsF"
colnames(exp2)[colnames(exp2) == "maledraws"] ="drawsM"

exdata <- exp2[c(8, 63, 75:77)]

exdata <- melt(exdata, id.vars = c("responseid", "treatment"))

levels(exdata$variable) <- c("T", "Female", "Male")

colnames(exdata)[colnames(exdata) == "variable"] = "Gender"

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(subset(exdata, !is.na(exdata$value) & (exdata$treatment != 1) & (exdata$Gender!="T")), aes(x = value, y=factor(treatment), fill=Gender)) +
# geom_boxplot(alpha=0.3, outlier.alpha=0.1, notch=TRUE, notchwidth=0.5) +
  geom_boxplot(alpha=0.3, outlier.alpha=0.1) +
# geom_violin(alpha=0.3) +
# geom_point(position=position_jitterdodge()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Draws") +
  ylab("Treatment") +
  geom_vline(xintercept = 6, linetype = "dashed", color="red") +
  xlim(c(0,30)) +
  theme(legend.title = element_blank())
# stat_summary(fun.data=data_summary, geom="pointrange", color="red") +
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

exp1 <- read_excel("C:/Users/Nico/OneDrive/papers/Covid/covid-3/exp1.xls")

exp1$condition[exp1$condition == "Rec.+peer info"] <-  "Recommendation + information"

ggplot(subset(exp1, !is.na(exp1$draws)), aes(x=factor(round), y=draws, fill=factor(female))) +
  geom_boxplot(alpha=0.3) +
# geom_point(position=position_jitterdodge(), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Round") +
  ylab("Draws") +
  facet_wrap(~condition, ncol=1) +
  theme(legend.title = element_blank())