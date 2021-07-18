library(ggplot2)

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
library(dplyr)
library(MASS)
library(tidyverse)
library(caret)
train <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Eva_Atta.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("C:/Users/imura/Documents/old_csv_downloads/Ac09.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

test$Bin = as.factor(test$Bin)
train$Bin = as.factor(train$Bin)

trainf <- names(train) %in% c("ID", "EtE", "MBS")
trainff <- train[!trainf]

testf <- names(test) %in% c("Colony", "ID", "EtE", "Category")
testff <- test[!testf]

lda.trainff <- lda(Bin ~ OL + AL + MB.MC+ MB.LC + MB.P + CX + SEZ, CV = TRUE, data = trainff)
#cannot incorporate ROCX because variables are collinear

trainff$lda <- lda.trainff$class
table(trainff$lda,trainff$Bin)

model <- lda(Bin ~ OL + AL + MB.MC+ MB.LC + MB.P + CX + SEZ, data = trainff)
model

predictions <- model %>% predict(testff)

#percent accuracy
mean(predictions$class==testff$Bin)

#plot
library(ggforce)
ggplot(lda.data, aes(LD1, LD2)) +
  #for polygon
  #geom_mark_hull(concavity = 5,expand=0,radius=0,aes(fill=Category))+
  theme(text = element_text(size=35)) +
  geom_mark_ellipse(color = "transparent", expand = 0, aes(fill=Category))+
  geom_point(aes(color = Category), size = 5) +
  scale_fill_manual(values = alpha(c("#e4b4e9", "lightgreen", "#f2a9a4"), 1)) +
  scale_color_manual(values=c("#e76bf3", "#00bf7d", "#f8766d")) +
  xlab("LD1 (84.33%") + ylab("LD2 (15.78%)")

#PCA
library(ggfortify)
dff <- names(df) %in% c("ID", "EtE", "Bin", "MBS", "Category")
dfff <- df[!dff]
autoplot(prcomp(log(dfff)), data = df, colour = "Bin", label.size=1, size = 5) + theme(text = element_text(size=35), axis.text.x = element_text(size=35), axis.text.y = element_text(size=35))

