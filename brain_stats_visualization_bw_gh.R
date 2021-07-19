library(ggplot2)

#graphing estimates from model
#shapes/color added in post
df <- read.csv("C:/Users/imura/Documents/grad_5/brain_paper/scalogram_numeric_HW.csv")

df$HW = as.numeric(df$HW)

ggplot(df, aes(x = HW, y = Higher.order, color = HW)) + 
  geom_point(size = 3) +
  xlab("Head Width (mm)") + 
  ylab("Estimated Higher Order Processing Score")+ 
  theme(text = element_text(size=20)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

#scatter of MB vs. score plus abline, also regression stats
dff <- read.csv("C:/Users/imura/Documents/grad_5/brain_paper/Eva_Atta_Dw.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
dff$Bin = as.factor(dff$Bin)
dff$Higher.order = as.numeric(dff$Higher.order)

ggplot(dff, aes(x = Higher.order, y = MBS)) + geom_point(data = dff, aes(x = Higher.order, y = MBS, shape = Bin), size = 6) +
  scale_x_continuous() + xlab("Estimated Sensory Integration Score") + ylab("Normalized MB-S Volume") + theme(text = element_text(size=35), axis.text.x = element_text(size=35), axis.text.y = element_text(size=35)) + scale_y_continuous() +  geom_smooth(method = "lm", se = FALSE, color = "black")

summary(lm(MBS ~ Higher.order, data = dff))


#absolute volumes
df <- read.csv("C:/Users/imura/Documents/grad_4/vol_geneexp/Eva_total_vol_corrected.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df$Bin = as.factor(df$Bin)

#stats

#Shapiro-Wilk: if significant then non-normal
#repeat for each compartment
shapiro.test(df$OL)

#for normally distributed variables: ANOVA
#repeat for each compartment
atta_mod1 <- lm(OL ~ Bin, data = df)
anova(atta_mod1)

#Tukey normal pairwise posthoc
TukeyHSD(aov(OL ~ as.factor(Bin), df), conf.level=0.95, digits=10)

#Kruskal-Wallis nonparametric test
#repeat for each compartment
kruskal.test(OL ~ Bin, data = df)

#nonparametric pairwise Wilcox posthoc
pairwise.wilcox.test(df$OL, df$Bin, p.adjust.method = "bonferroni")

#graphing
ggplot(df, aes(x = Bin, y = OL)) + geom_boxplot(fill = "blue", colour = "black") + scale_x_discrete() + xlab("Subcaste Head Width (mm)") + ylab("Absolute OL Volume") + theme(text = element_text(size=35), axis.text.x = element_text(size=35, angle = 90), axis.text.y = element_text(size=35)) + scale_y_continuous(labels = scales::scientific)

#proportional volumes
df <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Eva_Atta.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df$Bin = as.factor(df$Bin)

#stats

#Shapiro-Wilk: if significant then non-normal
#repeat for each compartment
shapiro.test(df$OL)

#for normally distributed variables: ANOVA
#repeat for each compartment
atta_mod1 <- lm(OL ~ Bin, data = df)
anova(atta_mod1)

#Tukey normal pairwise posthoc
TukeyHSD(aov(OL ~ as.factor(Bin), df), conf.level=0.95, digits=10)

#Kruskal-Wallis nonparametric test
#repeat for each compartment
kruskal.test(OL ~ Bin, data = df)

#nonparametric pairwise Wilcox posthoc
pairwise.wilcox.test(df$OL, df$Bin, p.adjust.method = "bonferroni")

#graphing
ggplot(df, aes(x = Bin, y = OL)) + geom_boxplot(fill = "blue", colour = "black") + scale_x_discrete() + xlab("Subcaste Head Width (mm)") + ylab("Normalized OL Volume") + theme(text = element_text(size=35), axis.text.x = element_text(size=35, angle = 90), axis.text.y = element_text(size=35)) + scale_y_continuous(labels = scales::scientific)

#LDA - linear discriminant analysis
library(MASS)
library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)

train <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Eva_Atta.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Ac09.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

test$Category = as.factor(test$Category)
train$Category = as.factor(train$Category)

trainf <- names(train) %in% c("ID", "EtE", "MBS", "Bin")
trainff <- train[!trainf]

testf <- names(test) %in% c("Colony", "ID", "EtE", "Bin")
testff <- test[!testf]

lda.trainff <- lda(Category ~ OL + AL + MB.MC+ MB.LC + MB.P + CB + SEG, CV = TRUE, data = trainff)
#cannot incorporate ROCB bc "variables are collinear"

trainff$lda <- lda.trainff$class
table(trainff$lda,trainff$Category)

model <- lda(Category ~ OL + AL + MB.MC+ MB.LC + MB.P + CB + SEG, data = trainff)

predictions <- model %>% predict(testff)

#percent accuracy
mean(predictions$class==testff$Category)

#graph points
predictions <- model %>% predict(testff)
lda.data <- cbind(trainff, predict(model)$x)

#circle and manual color
#adapted from https://luisdva.github.io/rstats/Grouping-points/
library(ggforce)
ggplot(lda.data, aes(LD1, LD2)) +
  theme(text = element_text(size=35)) +
  geom_mark_ellipse(color = "transparent", expand = 0, aes(fill=Category))+
  geom_point(aes(shape = Category, color = Category), size = 5) +
  scale_fill_manual(values = alpha(c("#e4b4e9", "lightgreen", "#f2a9a4"), 1)) +
  scale_color_manual(values=c("#e76bf3", "#00bf7d", "#f8766d")) +
  xlab("LD1 (84.33%") + ylab("LD2 (15.78%)")

#PCA
df <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Eva_Atta.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df$Bin = as.factor(df$Bin)

library(ggfortify)
dff <- names(df) %in% c("ID", "EtE", "Bin", "MBS", "Category")
dfff <- df[!dff]
autoplot(prcomp(log(dfff)), data = df, shape = "Bin", label.size=1, size = 5) + theme(text = element_text(size=35), axis.text.x = element_text(size=35), axis.text.y = element_text(size=35))
#colour = "Bin"
