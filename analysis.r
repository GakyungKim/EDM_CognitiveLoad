## import data
install.packages("readxl")
dear.data <- readxl:: read_excel(file.choose())
str(dear.data)
summary(dear.data)
## eliminate unusable data
dear.data.1 <- dear.data[,-c(1:4)]
dear.data.1$Cue <- as.factor(dear.data.1$Cue)
dear.data.1$Sex <- as.factor(dear.data.1$Sex)
dear.data.1$Major <- as.factor(dear.data.1$Major)
dear.data.1 <- dplyr::mutate(dear.data.1, Freq.omit = Freq - Freq.Play)

dear.data.1 <- dplyr::mutate(dear.data.1, Freq.omit = Freq - (Freq.Play + Freq.Paused + Freq.SeekFR2))
attach(dear.data.1) ## data.1 is reorganized data frame (no unusable data)

## correlation test between cognitive load(pupil size) and self-regulation #1
cor.test(Dur, Freq, method = "spearman")

IQR.MPD <- fivenum(MPD.MOV)[4] - fivenum(MPD.MOV)[2]
high.mpd <- which(MPD.MOV > fivenum(MPD.MOV)[4] + IQR.MPD*1.5)
low.mpd <- which(MPD.MOV < fivenum(MPD.MOV)[2] - IQR.MPD*1.5)
dear.data.1$MPD.MOV[high.mpd] <- NA
dear.data.1$MPD.MOV[low.mpd] <- NA
# remove outliers
high.mpd; low.mpd
dear.data.1 <- dear.data.1[-c(high.mpd, low.mpd),]
detach(dear.data.1) ## data.2 is reorganized with removed subjects
attach(dear.data.1)

IQR.freq <- fivenum(Freq.omit)[4] - fivenum(Freq.omit)[2]
high.freq <- which(Freq > fivenum(Freq.omit)[4] + IQR.freq*1.5)
low.freq <- which(Freq < fivenum(Freq.omit)[2] - IQR.freq*1.5)
dear.data.1$Freq[high.freq] <- NA
dear.data.1$Freq[low.freq] <- NA
# remove outliers
high.freq; low.freq
dear.data.1 <- dear.data.1[-c(6, 12, 28, 38, 39, 44, 46, 76, 77),]
detach(dear.data.1)
attach(dear.data.1)

#IQR.dur <- fivenum(Dur)[4] - fivenum(Dur)[2]
#high.dur <- which(Dur > fivenum(Dur)[4] + IQR.dur*1.5)
#low.dur <- which(Dur < fivenum(Dur)[2] - IQR.dur*1.5)
#dear.data.1$Dur[high.dur] <- NA
#dear.data.1$Dur[low.dur] <- NA
# remove outliers
#high.dur; low.dur
#dear.data.1 <- dear.data.1[-c(high.dur, low.dur),]
#detach(dear.data.1)

dear.data.1$g.freq <- dear.data.1$Freq.omit
dear.data.1$g.freq[(which(dear.data.1$g.freq <= fivenum(dear.data.1$Freq.omit)[2]))] <- 0
dear.data.1$g.freq[(which(dear.data.1$g.freq > fivenum(dear.data.1$Freq.omit)[2] &
                          dear.data.1$g.freq <= fivenum(dear.data.1$Freq.omit)[4]))] <- 1
dear.data.1$g.freq[(which(dear.data.1$g.freq > fivenum(dear.data.1$Freq.omit)[4]))] <- 2
dear.data.1$g.freq <- as.factor(dear.data.1$g.freq)

#dear.data.1$g.dur <- dear.data.1$Dur
#dear.data.1$g.dur[(which(dear.data.1$g.dur <= fivenum(dear.data.1$Dur)[2]))] <- 0
#dear.data.1$g.dur[(which(dear.data.1$g.dur > fivenum(dear.data.1$Dur)[2] &
                            #dear.data.1$g.dur <= fivenum(dear.data.1$Dur)[4]))] <- 1
#dear.data.1$g.dur[(which(dear.data.1$g.dur > fivenum(dear.data.1$Dur)[4]))] <- 2
#dear.data.1$g.dur <- as.factor(dear.data.1$g.dur)

dear.data.1$g.meta <- dear.data.1$Metacog
dear.data.1$g.meta[(which(dear.data.1$g.meta <= fivenum(dear.data.1$Metacog)[2]))] <- 0
dear.data.1$g.meta[(which(dear.data.1$g.meta > fivenum(dear.data.1$Metacog)[2] &
                            dear.data.1$g.meta <= fivenum(dear.data.1$Metacog)[4]))] <- 1
dear.data.1$g.meta[(which(dear.data.1$g.meta > fivenum(dear.data.1$Metacog)[4]))] <- 2
dear.data.1$g.meta <- as.factor(dear.data.1$g.meta)


t.test(dear.data.1[which(dear.data.1$Cue == 0),]$MPD.MOV4, dear.data.1[which(dear.data.1$Cue == 1),]$MPD.MOV4, var.equal= TRUE, paired = FALSE)
wilcox.test(dear.data.1$Pre ~ dear.data.1$Cue)
shapiro.test(dear.data.1[which(dear.data.1$Cue == 1),]$Pre)
var.test(MPD.MOV4 ~ Cue, data = dear.data.1)


boxplot(MPD.MOV ~ Cue, data = dear.data.1)
chisq.test(table(dear.data.1$Cue, dear.data.1$Major), correct = FALSE)

dear.data.1 %>%
  group_by(Cue) %>%
  summarise(Mean = mean(MPD.MOV4), SD = sd(MPD.MOV4))

str(dear.data.1)

library(ggplot2)
ggplot(data =dear.data.1, aes(x = Cue, y = Pre)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  scale_x_discrete(labels = c("0" = "No Cue", "1" = "Cue")) +
  theme_bw() +
  ylab("Pre-test")

## correlation test between cognitive load(pupil size) and self-regulation #2
cor.test(MPD.MOV, Dur.SeekFR2, method = "spearman")

###### caution!! ######
IQR.freq.p <- fivenum(Freq.Paused)[4] - fivenum(Freq.Paused)[2]
dear.data.1$Freq.Paused[which(Freq.Paused > fivenum(Freq.Paused)[4] + IQR.freq.p*1.5)] <- NA
dear.data.1$Freq.Paused[which(Freq.Paused < fivenum(Freq.Paused)[2] - IQR.freq.p*1.5)] <- NA

###### caution!! ######
IQR.freq.s2 <- fivenum(Freq.SeekFR2)[4] - fivenum(Freq.SeekFR2)[2]
dear.data.1$Freq.SeekFR2[which(Freq.SeekFR2 > fivenum(Freq.SeekFR2)[4] + IQR.freq.s2*1.5)] <- NA
dear.data.1$Freq.SeekFR2[which(Freq.SeekFR2 < fivenum(Freq.SeekFR2)[2] - IQR.freq.s2*1.5)] <- NA
## the outliers of seekFR2 cannot be eliminated because of so much 0s 

IQR.dur.p <- fivenum(Dur.Paused)[4] - fivenum(Dur.Paused)[2]
dear.data.1$Dur.Paused[which(Dur.Paused > fivenum(Dur.Paused)[4] + IQR.dur.p*1.5)] <- NA
dear.data.1$Dur.Paused[which(Dur.Paused < fivenum(Dur.Paused)[2] - IQR.dur.p*1.5)] <- NA

###### caution!! ######
IQR.dur.s2 <- fivenum(Dur.SeekFR2)[4] - fivenum(Dur.SeekFR2)[2]
dear.data.1$Dur.SeekFR2[which(Dur.SeekFR2 > fivenum(Dur.SeekFR2)[4] + IQR.dur.s2*1.5)] <- NA
dear.data.1$Dur.SeekFR2[which(Dur.SeekFR2 < fivenum(Dur.SeekFR2)[2] - IQR.dur.s2*1.5)] <- NA

##visualisation using boxplots
ggplot(data = dear.data.1, aes(x = g.freq, y = Improv, group = g.freq)) +
  geom_boxplot(outlier.color = 'red', outlier.alpha = .5) +
  scale_x_discrete(labels = c("0" = "lower", "1"= "middle", "2" = "upper")) +
  labs(x = "", y = "Improvment(post - pre)") +
  theme_bw()
ggplot(data = dear.data.1, aes(x = Cue, y = Freq.SeekFR2, group = Cue)) +
  geom_boxplot(outlier.color = 'red', outlier.alpha = .5) +
  scale_x_discrete(labels = c("0" = "No-Cue", "1" = "Cue")) +
  labs(x = "", y = "Frequencies for re-reading") +
  theme_bw()

## regressed by visual cue
summary(lm(Improv ~ Cue, data = dear.data.1)) # non-significant - no direct effect of visual cue
## regressed by behavioral data
summary(lm(Improv ~ Dur, data = dear.data.1))
summary(lm(Improv ~ Freq, data = dear.data.1))
summary(lm(Improv ~ Dur + Freq, data = dear.data.1))
summary(lm(Improv ~ Dur.Paused, data = dear.data.1))
summary(lm(Improv ~ Dur.SeekFR2, data = dear.data.1))
## regressed by physiological data (pupil dialation)
summary(lm(Improv ~ MPD.MOV, data = dear.data.1)) # non-significant - no direct effect of visual cue
## regressed by both behavioral and physiological data
summary(lm(Improv ~ MPD.MOV*Dur + Cue, data = dear.data.1))
summary(lm(Improv ~ MPD.MOV*Freq.omit + Cue, data = dear.data.1))
summary(lm(Improv ~ MPD.MOV*Freq + MPD.MOV*Dur + Cue, data = dear.data.1))

## figuring out controllable variables for making direct effect of Cues
Anova(aov(Improv ~ Cue + MPD.MOV, data = dear.data.1), type = "III")

## is there a difference between post and pre?
shapiro.test(dear.data.1$Improv)
wilcox.test(dear.data.1$Improv, alternative = "greater") # p = 5.032e-06 ****

## is there a difference between post and pre depending on Cues?
shapiro.test(dear.data.1[which(dear.data.1$Cue == 0),]$Improv)
shapiro.test(dear.data.1[which(dear.data.1$Cue == 1),]$Improv)
wilcox.test(Improv ~ Cue, data = dear.data.1, alternative = "less")
ggplot(data = dear.data.1, aes(x = Cue, y = Improv)) + 
  geom_boxplot()

## is there an interaction between an interesting variable and Cue?
summary(aov(Improv ~ Cue*g.dur, data = dear.data.1))
shapiro.test(residuals(aov(Improv ~ Cue*g.dur, data = dear.data.1)))
var.test(Improv ~ Cue, data = dear.data.1)
bartlett.test(Improv ~ g.dur, data= dear.data.1)
TukeyHSD(aov(Improv ~ Cue*g.dur, data = dear.data.1))
ggplot(data = dear.data.1, aes(x = Cue, y = Improv, fill = g.dur)) +
  geom_boxplot(outlier.colour = 'red', position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  ylab("Improvement") +
  theme_bw()
summary(aov(Improv ~ Cue*g.meta, data = dear.data.1))
summary(aov(Improv ~ Cue*g.freq, data = dear.data.1))
shapiro.test(residuals(aov(Improv ~ Cue*g.freq, data = dear.data.1)))
var.test(Improv ~ Cue, data = dear.data.1)
bartlett.test(Improv ~ g.freq, data= dear.data.1)
TukeyHSD(aov(Improv ~ Cue*g.freq, data = dear.data.1))
ggplot(data = dear.data.1, aes(x = Cue, y = Improv, fill = g.freq)) +
  geom_boxplot(outlier.colour = 'red', position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  ylab("Improvement") +
  theme_bw()

## perhaps mediation?
# dummy coding of Cue for mediation analysis
dear.data.1$Cue <- as.numeric(dear.data.1$Cue)
dear.data.1$Cue[which(dear.data.1$Cue == 1)] <- 0
dear.data.1$Cue[which(dear.data.1$Cue == 2)] <- 1
psych::mediate(Improv ~ Cue + (Dur), data = dear.data.1, n.iter=10000) %>%
  print(short = FALSE)
psych::mediate(Improv ~ Cue + (Freq.omit), data = dear.data.1, n.iter=10000) %>%
  print(short = FALSE)
psych::mediate(Improv ~ Cue + (MPD.MOV), data = dear.data.1, n.iter=10000) %>%
  print(short = FALSE)
psych::mediate(Improv ~ Cue + (Metacog), data = dear.data.1, n.iter=10000) %>%
  print(short = FALSE)

IQR.przn <- fivenum(dear.data.2$Presence.Ave)[4] - fivenum(dear.data.2$Presence.Ave)[2]
IQR.dur.p <- fivenum(Dur.Paused)[4] - fivenum(Dur.Paused)[2]
dear.data.1$g.przn <- dear.data.2$Presence.Ave
dear.data.1$g.przn[which(Presence.Ave < fivenum(Presence.Ave)[2] - IQR.przn*1.5)] <- 0
dear.data.1$g.przn[which(Presence.Ave >= fivenum(Presence.Ave)[2] - IQR.przn*1.5) &
                             which(Presence.Ave <= fivenum(Presence.Ave)[4] + IQR.przn*1.5)] <- 1
dear.data.1$g.przn[which(Presence.Ave > fivenum(Presence.Ave)[4] + IQR.przn*1.5)] <- 2

## amazing results!!!!!!!!!!!!!!!!!
shapiro.test(dear.data.1$MPD.MOV1)
shapiro.test(dear.data.1$MPD.MOV4)
var.test(dear.data.1$MPD.MOV1, dear.data.1$MFC.MOV4)
t.test(dear.data.1$MPD.MOV1, dear.data.1$MPD.MOV4, var.equal = FALSE, paired = TRUE)

shapiro.test(dear.data.1[which(dear.data.1$Cue == 0),]$MPD.MOV1)
shapiro.test(dear.data.1[which(dear.data.1$Cue == 1),]$MPD.MOV1)
var.test(MPD.MOV1 ~ Cue, data = dear.data.1)
boxplot(dear.data.1$MPD.MOV1 ~ dear.data.1$Cue)
t.test(dear.data.1$MPD.MOV1 ~ dear.data.1$Cue, var.equal = TRUE, paired = FALSE)

shapiro.test(dear.data.1[which(dear.data.1$Cue == 0),]$MPD.MOV4)
shapiro.test(dear.data.1[which(dear.data.1$Cue == 1),]$MPD.MOV4)
var.test(MPD.MOV4 ~ Cue, data = dear.data.1)
boxplot(dear.data.1$MPD.MOV4 ~ dear.data.1$Cue)
t.test(dear.data.1$MPD.MOV4 ~ dear.data.1$Cue, var.equal = TRUE, paired = FALSE)

## pupil dialation by cue
colnames(dear.data.1)
data.bar.PD <- dear.data.1[, c(1, grep("MPD", colnames(dear.data.1)), 59:61)]
colnames(data.bar.PD)[c(2:5)] <- c("1st Section", "2nd Section", "3rd Section", "4th Section")
data.bar.PD_long <- tidyr::gather(data.bar.PD, MPD, value, -MPD.MOV, -Cue, -Freq.omit, -g.freq, -g.meta)
data.bar.PD_long$MPD <- as.factor(data.bar.PD_long$MPD)
# both groups (p = .002 ***; 1st<>3rd(p = .028), 1st<>4th(p = .004), 2nd<>4th(p = .040))
summary(aov(value ~ MPD, data = data.bar.PD_long))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long)))
bartlett.test(value ~ MPD, data = data.bar.PD_long)
TukeyHSD(aov(value ~ MPD, data = data.bar.PD_long))
# No Cue group (p = .002 **; 1st<>3rd(p = .023), 1st<>4th(p = .003))
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
kruskal.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),])
shapiro.test(data.bar.PD_long[which(data.bar.PD_long$Cue == 0 & data.bar.PD_long$MPD == "MPD.MOV3"),]$value)
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),])
PMCMR::posthoc.kruskal.nemenyi.test(x=data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]$value, 
                                    g=data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]$MPD, 
                                    method="Chisqure")
TukeyHSD(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
# Cue group (p = .326)
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))
kruskal.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),])
shapiro.test(data.bar.PD_long[which(data.bar.PD_long$Cue == 1 & data.bar.PD_long$MPD == "MPD.MOV4"),]$value)
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),])

# visualisation with boxplot
ggplot(data = data.bar.PD_long, aes(x = MPD, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=median,geom="smooth",aes(group=1), lwd = .5) +
  stat_boxplot(geom = "errorbar", width = .2) +
  theme_bw() +
  ylab("Pupil Dilation") +
  xlab("") +
  ylim(c(-.3, .6)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
  )
# boxplot with dividing groups by cue
ggplot(data = data.bar.PD_long, aes(x = MPD, y = value, fill = Cue)) +
  scale_fill_manual(values = c("White", "Grey"), label = c("No Cue", "Cue")) +
  geom_boxplot(position = position_dodge(.75), outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", position = position_dodge(.75), width = .2) +
  stat_summary(fun.y=median,geom="smooth",aes(group=Cue), lwd = .5) +
  theme_bw() +
  ylab("Pupil Dilation") +
  xlab("") +
  ylim(c(-.3, .6)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.direction = "vertical",
    legend.position = c(.8, .9),
    legend.title = element_blank()
  )

ggplot(data = data.bar.PD_long, aes(x = MPD, y = value, fill = Cue)) +
  stat_summary(fun.y=median,geom="smooth",aes(group=Cue), lwd = .5) +
  theme_bw() +
  ylab("Pupil Dilation") +
  xlab("") +
  ylim(c(-.3, .6)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.direction = "vertical",
    legend.position = c(.8, .9),
    legend.title = element_blank()
  )


summary(lm(MPD.MOV4 ~ Freq.omit, data = data.bar.PD))
ggplot(data = data.bar.PD, aes(x = Freq.omit, y = MPD.MOV))+
  geom_smooth(method = lm)



# low frequency group (p = .396)
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 0),]))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 0),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 0),])
# middle frequency group (p = 0.046; 1st<>4th(p = .077))
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 1),]))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 1),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 1),])
TukeyHSD(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 1),]))
# high frequency group (p = .139)
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 2),]))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 2),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.freq == 2),])
# visualisation
ggplot(data = data.bar.PD_long, aes(x = MPD, y = value, fill = g.freq, col = Cue)) +
  scale_fill_manual(values = c("white", "gray", "black")) +
  scale_color_manual(values = c("red", "blue"))+
  geom_boxplot(position = position_dodge(.75), outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", position = position_dodge(.75), width = .2) +
  theme_bw() +
  ylim(c(-.3, .6)) +
  ylab("") +
  xlab("")
  

# low duration group (p = .033*; 1st<>4th(p = .075))
kruskal.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 0),])
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 0),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 0),])
PMCMR::posthoc.kruskal.nemenyi.test(x=data.bar.PD_long[which(data.bar.PD_long$g.dur == 0),]$value, 
                                    g=data.bar.PD_long[which(data.bar.PD_long$g.dur == 0),]$MPD, 
                                    method="Chisqure")
# middle duration group (p = 0.169)
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 1),]))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 1),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 1),])
TukeyHSD(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 1),]))
# high duration group (p = 0.71)
summary(aov(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 2),]))
shapiro.test(residuals(lm(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 2),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$g.dur == 2),])
# visualisation
ggplot(data = data.bar.PD_long, aes(x = MPD, y = value, fill = g.dur, col = Cue)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set1") +
  geom_boxplot(position = "dodge", outlier.colour = 'red', outlier.size = .5) +
  theme_bw() +
  ylab("")

# interaction between cue and frequency
summary(lm((value-mean(data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]$value)) ~ MPD + Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
lm.beta::lm.beta(lm(value 
                   ~ MPD + Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
summary(lm(value ~ MPD*Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
summary(lm((value-mean(data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]$value)) ~ MPD + Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))
lm.beta::lm.beta(lm(value 
                    ~ MPD + Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))

summary(lm(value ~ MPD*Freq.omit, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))
summary(lm(value ~ Freq.omit*Cue, data = data.bar.PD_long))
summary(aov(value ~ MPD*Cue, data = data.bar.PD_long))
summary(aov(value ~ MPD*Cue*Freq.omit, data = data.bar.PD_long))
# interaction between cue and duration
summary(aov(value ~ MPD + g.dur, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
summary(aov(value ~ MPD*g.dur, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),]))
summary(aov(value ~ MPD + g.dur, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))
summary(aov(value ~ MPD*g.dur, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),]))
summary(aov(value ~ g.dur*Cue, data = data.bar.PD_long))
summary(aov(value ~ MPD*Cue, data = data.bar.PD_long))
summary(aov(value ~ MPD*Cue*g.dur, data = data.bar.PD_long))


shapiro.test(residuals(lm(value ~ MPD*g.freq, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),])))
bartlett.test(value ~ MPD, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),])
bartlett.test(value ~ g.freq, data = data.bar.PD_long[which(data.bar.PD_long$Cue == 0),])

ggplot(data = data.bar.PD_long[which(data.bar.PD_long$Cue == 1),], aes(x = Freq.omit, y = value, group = MPD)) +
  geom_smooth(aes(col = MPD), method = lm) +
  geom_point(aes(col = MPD), size = .5, alpha = .5) +
  theme_bw() +
  ylim(c(-.3, .6)) +
  ylab("")

sort(data.bar.PD_long[which(data.bar.PD_long$Cue ==0),]$Freq.omit)
t.test(data.bar.PD_long$Freq.omit ~ data.bar.PD_long$Cue)
shapiro.test(residuals(lm(Freq.omit ~ Cue, data = data.bar.PD_long)))
wilcox.test(Freq.omit ~ Cue, data = data.bar.PD_long)

data.bar.SC <- dear.data.2.1[, c(1, grep("MSC", colnames(dear.data.2.1)))]
data.bar.SC <- data.bar.SC[, -6]
colnames(data.bar.SC)[c(2:5)] <- c("1st Video", "2nd Video", "3rd Video", "4th Video")
data.bar.SC_long <- tidyr::gather(data.bar.SC, MSC, value, -Cue)

ggplot(data = data.bar.SC_long, aes(x = MSC, y = value, fill = Cue)) +
  geom_boxplot(position = "dodge", outlier.colour = 'red') +
  geom_point(position = position_jitterdodge(), alpha=.2) +
  ylab("")


barplot(MPD.MOV1, MPD.MOV2, MPD.MOV3, MPD.MOV4)

## re-defining freq.play and freq.rate
dear.data.2.1 <- dplyr::mutate(dear.data.2, Freq.omit = Freq - Freq.Play)

### dividing groups by frequencies based on Quartile
dear.data.3 <- dplyr::filter(dear.data.1, g.freq == 0 | g.freq == 2)

## test of meta-cognition depending on frequencies(self-regulation)
# normality test
shapiro.test(dear.data.3[which(dear.data.3$g.freq == 0),]$Metacog)
# homogeneity of variance test
var.test(Metacog ~ g.freq, data = dear.data.3)
# t-test > p = .02
t.test(Metacog ~ g.freq, data = dear.data.3, var.equal = TRUE, alternative = "greater", paired = FALSE)
#visualisation with boxplot
ggplot(data = dear.data.3, aes(x = g.freq, y = Metacog, group = g.freq)) +
  geom_boxplot(outlier.colour = 'red') +
  labs(x = "Frequencies", y = 'Meta Cognition') +
  scale_x_discrete(labels = c("0" = 'lower 25%', "2" = 'upper 75%')) +
  theme_bw()
dear.data.4 <- dear.data.3[-which(dear.data.3$Metacog > 4.2), ]

## test of cognitive-load depending on frequencies(self-regulation)
# normality test
shapiro.test(dear.data.4[which(dear.data.4$g.freq == 0),]$MPD.MOV)
# nonparametric t test : Wilcoxon test
wilcox.test(MPD.MOV ~ g.freq, data = dear.data.4)
# visualisation with boxplot
ggplot(data = dear.data.4, aes(x = g.freq, y = MPD.MOV, group = g.freq)) +
  geom_boxplot(outlier.colour = 'red') +
  labs(x = "Frequencies", y = 'Pupil Dialation') +
  scale_x_discrete(labels = c("0" = 'lower 25%', "2" = 'upper 75%')) +
  theme_bw()
# remove outliers based on the pupil dialation data
dear.data.5 <- dear.data.4[-which(dear.data.4$MPD.MOV > 0.5),]
# normality test with outliers removed data frame
shapiro.test(dear.data.5[which(dear.data.4$g.freq == 0),]$MPD.MOV)
# homogeneity of variance test
var.test(MPD.MOV ~ g.freq, data = dear.data.5)
# t-test > p= .3
t.test(MPD.MOV ~ g.freq, data = dear.data.5, var.equal = TRUE, paired = FALSE)
# visualisation with boxplot
ggplot(data = dear.data.5, aes(x = g.freq, y = MPD.MOV, group = g.freq)) +
  geom_boxplot(outlier.colour = 'red') +
  labs(x = "Frequencies", y = 'Pupil Dialation') +
  scale_x_discrete(labels = c("0" = 'lower 25%', "2" = 'upper 75%')) +
  theme_bw()

## re-test of meta-cognition depending on frequencies(self-regulation)
# normality test
shapiro.test(dear.data.5[which(dear.data.5$g.freq == 2),]$Metacog)
# homogeneity of variance test
var.test(Metacog ~ g.freq, data = dear.data.5)
# t-test > p = .02
t.test(Metacog ~ g.freq, data = dear.data.5, var.equal = TRUE, alternative = "greater", paired = FALSE)
#visualisation with boxplot
ggplot(data = dear.data.5, aes(x = g.freq, y = Metacog, group = g.freq)) +
  geom_boxplot(outlier.colour = 'red') +
  labs(x = "Frequencies", y = 'Meta Cognition') +
  scale_x_discrete(labels = c("0" = 'lower 25%', "2" = 'upper 75%')) +
  theme_bw()

## hypothetical model confirmation #1
summary(lm(MPD.MOV ~ Cue*Metacog, data = dear.data.5))
summary(glm(g.freq ~ Cue*Metacog, data = dear.data.5, family = "binomial"))

# dummy coding(Cue, g.freq)
dear.data.6 <- dear.data.5
dear.data.6$g.freq <- as.numeric(dear.data.6$g.freq)
dear.data.6$g.freq[which(dear.data.6$g.freq == 1)] <- 0
dear.data.6$g.freq[which(dear.data.6$g.freq == 3)] <- 1
dear.data.6$Cue <- as.numeric(dear.data.6$Cue)
dear.data.6$Cue[which(dear.data.6$Cue == 1)] <- 0
dear.data.6$Cue[which(dear.data.6$Cue == 2)] <- 1

psych::mediate(y = Improv ~ Cue + (g.freq), data = dear.data.6, 
               n.iter = 10000) %>%
  print(short = FALSE)

## hypothetical model confirmation #2
summary(lm(Improv ~ MPD.MOV*g.freq, data = dear.data.5))
summary(lm(Improv ~ Metacog*g.freq, data = dear.data.5))
summary(lm(Improv ~ Metacog*MPD.MOV, data = dear.data.5))


