library(tidyverse)
library(readxl)
library(scales)
library(cowplot)
library(RColorBrewer)

my_col <- brewer.pal(8,"Dark2")[3:7]

df0 <- read_excel("wsl.xlsx")
head(df0)
df0$types <- factor(df0$types, levels = c("PP","Nylon","PVC","PS","LDPE","PET"))
df0$place <- factor(df0$place, levels = c("Estuary","Inner Bay","Open Bay","Virgin MPs"),
                    labels = c("Estuary","Inner~Bay","Open~Bay","Pristine~MPs"))

df1 <- df0 %>% gather(key = "metal", value = "concentration", 7:11)
head(df1)
df1$metal <- factor(df1$metal, levels = c("Zn", "Cu", "Ni", "Pb","Cd"),
                    labels =  c("{}^68*Zn", "{}^65*Cu","{}^62*Ni", "{}^206*Pb","{}^114*Cd"))
df1$mesh <- factor(df1$mesh, levels = c("80", "200"),
                   labels =  c(">180 µm", "75-180 µm"))
df1_mean <- df1 %>% group_by(place, types,metal) %>% 
  summarize(mean = mean(concentration),
            sd =  sd(concentration))

# median concentrations
read_excel("wsl.xlsx") %>% 
  select(1:11) %>% 
  pivot_longer(cols=7:11, names_to ="metal", values_to = "conc") %>% 
  mutate(aged = ifelse(place=="Virgin MPs", "Pristine", "Aged"))  %>% 
  group_by(metal, aged) %>% 
  summarise(median=median(conc)) %>% 
  pivot_wider(names_from = "aged", values_from="median")
  

names(read_excel("wsl.xlsx"))
read_excel("wsl.xlsx") %>% 
  select(1:6, 12:16) %>% 
  pivot_longer(cols=7:11, names_to ="metal", values_to = "conc") %>% 
  mutate(aged = ifelse(place=="Virgin MPs", "Pristine", "Aged"))  %>% 
  group_by(metal, aged, ) %>% 
  summarise(median=median(conc)) %>% 
  pivot_wider(names_from = "aged", values_from="median")


read_excel("wsl.xlsx") %>% 
  select(1:11) %>% 
  pivot_longer(cols=7:11, names_to ="metal", values_to = "conc") %>% 
  mutate(aged = ifelse(place=="Virgin MPs", "Pristine", "Aged"))  %>% 
  group_by(metal, aged, types) %>% 
  summarise(mean=mean(conc)) %>% 
  group_by(metal, aged) %>% 
  summarise(median=median(mean)) %>% 
  pivot_wider(names_from = "aged", values_from="median") %>% 
  mutate(ratio=Aged/Pristine)

d_label <- read_excel("label_isotope.xlsx", sheet =1 )
d_label_2 <- read_excel("label_isotope.xlsx", sheet =2 )

d_label$metal <- factor(d_label$metal, levels = c("Zn", "Cu", "Ni", "Pb","Cd"),
                    labels =  c("{}^68*Zn", "{}^65*Cu","{}^62*Ni", "{}^206*Pb","{}^114*Cd"))
d_label$mesh <- factor(d_label$mesh, levels = c("80", "200"),
                   labels =  c(">180 µm", "75-180 µm"))

d_label_2$metal <- factor(d_label_2$metal, levels = c("Zn", "Cu", "Ni", "Pb","Cd"),
                    labels =  c("{}^68*Zn", "{}^65*Cu","{}^62*Ni", "{}^206*Pb","{}^114*Cd"))
d_label_2$mesh <- factor(d_label_2$mesh, levels = c("80", "200"),
                   labels =  c(">180 µm", "75-180 µm"))

d_label$place <- factor(d_label$place, levels = c("Estuary","Inner Bay","Open Bay","Pristine MPs"),
                    labels = c("Estuary","Inner~Bay","Open~Bay","Pristine~MPs"))

d_label_2$place <- factor(d_label_2$place, levels = c("Estuary","Inner Bay","Open Bay","Pristine MPs"),
                        labels = c("Estuary","Inner~Bay","Open~Bay","Pristine~MPs"))




ggplot(df1_mean, aes(types, mean)) +
  geom_col(aes(fill = metal), width = 0.8, alpha = 0.5, color="black", size = 0.3) +
  geom_errorbar(aes(types, ymin = mean, ymax = mean + sd, color = metal),
                size = 0.5, width = 0, alpha = 0.5) +
  geom_point(data = df1,
             aes(types,concentration, color = metal, shape = mesh),
             position = position_jitterdodge(jitter.width = 0.25)) +
  facet_grid(metal~place, scales = "free_y",labeller = label_parsed) +
  theme_bw() +
  scale_y_continuous(n.breaks = 4) +
  scale_color_manual(values = my_col, guide = "none") +
  scale_shape_manual(values = c(19, 1),
                     name = "Size",
                     labels = c(">180 µm", "75-180 µm")) +
  scale_fill_manual(values = my_col, guide = "none") +
  ylab(expression("Newly accumulated metal concentrations at equilibrium (µg g"^-1*" MPs)")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 12),
        panel.grid = element_blank()) +
  geom_rect(data=subset(d_label, types %in% c("Nylon", "PS", "PET")), fill="transparent", 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="grey50", size=0.1, inherit.aes = F)+
  geom_rect(data=subset(d_label, types %in% c("PP", "PVC", "LDPE")), fill="grey90", 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            inherit.aes = F)+
  geom_text(data=d_label, aes(x=types, y = y, label = label), size=2)+
  geom_text(data=d_label_2, aes(x=types, y =y_2, label = label),color="grey50", size=2)+ #"#6a040f"
  geom_hline(data=d_label,aes(yintercept=ymin), color="grey80", size=0.1)



ggsave("04_metal_isotope adsorption v3.png", width = 420/90, height = 541/90, dpi = 900, units = "in")
ggsave("04_metal_isotope adsorption v3.pdf", width = 420/90, height = 541/90, units = "in")



# statistical analysis
library(car)
library(multcomp)
library(multcompView)
library(broom)
library(tidyverse)

df1 <- df0 %>% gather(key = "metal", value = "concentration", 7:11)

names(df1)
head(df1)
str(df_aged)
unique(df1$metal)
# "Ni" "Cu" "Zn" "Cd" "Pb"


#Zn------------------------------------------------------
d_yy <- subset(df1, metal == "Zn" & mesh == 80)

head(d_yy)
table(d_yy$types)
ggplot(d_yy, aes(types, log(concentration)))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

ggplot(d_yy, aes(types, concentration))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

leveneTest(concentration ~ place * types, data = d_yy)
leveneTest(concentration ~ place, data = d_yy)
leveneTest(concentration ~ types, data = d_yy)

table(d_yy$place)
leveneTest(log(concentration) ~ place * types, data = d_yy)

mod.yy <- aov(log(concentration) ~ place * types , data = d_yy)


resid <- mod.yy$residuals
shapiro.test(resid)

summary(mod.yy)

yy.emm <- emmeans(mod.yy,  ~ types * place)
#emmeans(mod.yy,  ~ types | place)

#Zn place-----
pairs(yy.emm, simple = "place")

mc_place <- tidy(pairs(yy.emm, simple = "place"))

unique(mc_place$types)

p_0 <- mc_place %>% filter(types == "PP")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "Nylon")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PVC")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PS")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "LDPE")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_place %>% filter(types == "PET")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)





#Zn polymer----
pairs(yy.emm, simple = "types")

mc_kind <- tidy(pairs(yy.emm, simple = "types"))

unique(mc_kind$place)
p_0 <- mc_kind %>% filter(place == "Estuary")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Inner~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Open~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Pristine~MPs")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)






#Cu------------------------------------------------------
d_yy <- subset(df1, metal == "Cu" & mesh == 80)

head(d_yy)
table(d_yy$types)
ggplot(d_yy, aes(types, log(concentration)))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

ggplot(d_yy, aes(types, concentration))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

leveneTest(concentration ~ place * types, data = d_yy)
leveneTest(concentration ~ place, data = d_yy)
leveneTest(concentration ~ types, data = d_yy)

table(d_yy$place)
leveneTest(log(concentration) ~ place * types, data = d_yy)

mod.yy <- aov(log(concentration) ~ place * types , data = d_yy)


resid <- mod.yy$residuals
shapiro.test(resid)

summary(mod.yy)

yy.emm <- emmeans(mod.yy,  ~ types * place)
#emmeans(mod.yy,  ~ types | place)

#Cu place-----
pairs(yy.emm, simple = "place")

mc_place <- tidy(pairs(yy.emm, simple = "place"))

unique(mc_place$types)

p_0 <- mc_place %>% filter(types == "PP")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "Nylon")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PVC")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PS")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "LDPE")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_place %>% filter(types == "PET")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)





#Cu polymer----
pairs(yy.emm, simple = "types")

mc_kind <- tidy(pairs(yy.emm, simple = "types"))

unique(mc_kind$place)
p_0 <- mc_kind %>% filter(place == "Estuary")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Inner~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Open~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Pristine~MPs")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)









#Ni------------------------------------------------------
d_yy <- subset(df1, metal == "Ni" & mesh == 80)

head(d_yy)
table(d_yy$types)
ggplot(d_yy, aes(types, log(concentration)))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

ggplot(d_yy, aes(types, concentration))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

leveneTest(concentration ~ place * types, data = d_yy)
leveneTest(concentration ~ place, data = d_yy)
leveneTest(concentration ~ types, data = d_yy)

table(d_yy$place)
leveneTest(log(concentration) ~ place * types, data = d_yy)

mod.yy <- aov(log(concentration) ~ place * types , data = d_yy)


resid <- mod.yy$residuals
shapiro.test(resid)

summary(mod.yy)

yy.emm <- emmeans(mod.yy,  ~ types * place)
#emmeans(mod.yy,  ~ types | place)

#Ni place-----
pairs(yy.emm, simple = "place")

mc_place <- tidy(pairs(yy.emm, simple = "place"))

unique(mc_place$types)

p_0 <- mc_place %>% filter(types == "PP")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "Nylon")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PVC")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PS")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "LDPE")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_place %>% filter(types == "PET")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)





#Ni polymer----
pairs(yy.emm, simple = "types")

mc_kind <- tidy(pairs(yy.emm, simple = "types"))

unique(mc_kind$place)
p_0 <- mc_kind %>% filter(place == "Estuary")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Inner~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Open~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Pristine~MPs")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)














#Pb------------------------------------------------------
d_yy <- subset(df1, metal == "Pb" & mesh == 80)

head(d_yy)
table(d_yy$types)
ggplot(d_yy, aes(types, log(concentration)))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

ggplot(d_yy, aes(types, concentration))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

leveneTest(concentration ~ place * types, data = d_yy)
leveneTest(concentration ~ place, data = d_yy)
leveneTest(concentration ~ types, data = d_yy)

table(d_yy$place)
leveneTest(log(concentration) ~ place * types, data = d_yy)

mod.yy <- aov(log(concentration) ~ place * types , data = d_yy)


resid <- mod.yy$residuals
shapiro.test(resid)

summary(mod.yy)

yy.emm <- emmeans(mod.yy,  ~ types * place)
#emmeans(mod.yy,  ~ types | place)

#Pb place-----
pairs(yy.emm, simple = "place")

mc_place <- tidy(pairs(yy.emm, simple = "place"))

unique(mc_place$types)

p_0 <- mc_place %>% filter(types == "PP")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "Nylon")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PVC")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PS")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "LDPE")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_place %>% filter(types == "PET")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)





#Pb polymer----
pairs(yy.emm, simple = "types")

mc_kind <- tidy(pairs(yy.emm, simple = "types"))

unique(mc_kind$place)
p_0 <- mc_kind %>% filter(place == "Estuary")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Inner~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Open~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Pristine~MPs")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)












#Cd------------------------------------------------------
d_yy <- subset(df1, metal == "Cd" & mesh == 80)

head(d_yy)
table(d_yy$types)
ggplot(d_yy, aes(types, log(concentration)))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

ggplot(d_yy, aes(types, concentration))+
  geom_point(position=position_jitter(width=0.2), shape=1)+
  facet_wrap(~place)

leveneTest(concentration ~ place * types, data = d_yy)
leveneTest(concentration ~ place, data = d_yy)
leveneTest(concentration ~ types, data = d_yy)

table(d_yy$place)
leveneTest(log(concentration) ~ place * types, data = d_yy)

mod.yy <- aov(log(concentration) ~ place * types , data = d_yy)


resid <- mod.yy$residuals
shapiro.test(resid)

summary(mod.yy)

yy.emm <- emmeans(mod.yy,  ~ types * place)
#emmeans(mod.yy,  ~ types | place)

#Cd place-----
pairs(yy.emm, simple = "place")

mc_place <- tidy(pairs(yy.emm, simple = "place"))

unique(mc_place$types)

p_0 <- mc_place %>% filter(types == "PP")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "Nylon")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PVC")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "PS")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)


p_0 <- mc_place %>% filter(types == "LDPE")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_place %>% filter(types == "PET")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)





#Cd polymer----
pairs(yy.emm, simple = "types")

mc_kind <- tidy(pairs(yy.emm, simple = "types"))

unique(mc_kind$place)
p_0 <- mc_kind %>% filter(place == "Estuary")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Inner~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Open~Bay")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)

p_0 <- mc_kind %>% filter(place == "Pristine~MPs")
p_1 <- p_0$adj.p.value
names(p_1) <- gsub(" ", "", p_0$contrast, fixed = TRUE)
p_1 <- na.omit(p_1)
multcompLetters(p_1, compare="<", threshold=0.05, reversed = F)
