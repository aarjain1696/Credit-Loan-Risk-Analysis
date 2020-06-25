#### IST 719 - Information Visualization
#### Group 3
#### Aarjav Jain

#### Project Plots 

## Use the same R-Workspace created in the previous R script to reduce the processing time


### Packages 
library(ggplot2) # for ggplot
library(plyr)
library(gcookbook)
library(ggpubr) # for ggarrange
library(cowplot)
library(RColorBrewer)

# limiting scientific notation
options(scipen = 99)

# Column Names
colnames(credit)


## Occupation type and target with percentage format
occ.target = table(credit$OCCUPATION_TYPE, credit$TARGET)
occ.df = as.data.frame(occ.target)
occ.df = ddply(occ.df, "Var1", transform,
               perc = Freq / sum(Freq) * 100)

plot13 = ggplot(occ.df, aes(x = Var1, y = perc, fill = Var2)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA), 
        axis.text.x = element_text(angle = 90)) +
  ggtitle("Defaults by Occupation", subtitle = "...") + 
  geom_text(aes(y = perc, label = round(perc, 2)), vjust = 1.5, colour = "Black") 

# Donut chart
donut.occ$perc = round(donut.occ$Freq/sum(donut.occ$Freq)*100, 2)
donut.occ$ymax = cumsum(donut.occ$perc)
donut.occ$ymin = c(0, head(donut.occ$ymax, n = -1))
donut.occ$l.pos = (donut.occ$ymax + donut.occ$ymin)/2
donut.occ$labels = paste0(donut.occ$Var1, "\n", donut.occ$perc)
plot14 = ggplot(data = donut.occ, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3.75, fill = Var1)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) +
  geom_text(x = 4.2, aes(y = l.pos, label = labels), size = 4 )+
  theme(legend.position = "none")


## Family Status and Target with percentage format
family.target = table(credit$NAME_FAMILY_STATUS, credit$TARGET)
df.family.status = as.data.frame(family.target)
df.family.status = ddply(df.family.status, "Var1", transform,
                         percent_weight = Freq / sum(Freq) * 100)
plot8 = ggplot(df.family.status, aes(x = Var1, y = percent_weight, fill = Var2)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Pastel1") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA), 
        axis.text.x = element_text(angle = 90)) + 
  ggtitle("Defaults by Family Status", subtitle = "...") + 
  geom_text(aes(y = percent_weight, label = round(percent_weight, 2)), vjust = 1.5, colour = "Black")

# Donut chart
donut.family = df.family.status[df.family.status$Var2 == 1, ]
donut.family$perc = round(donut.family$Freq/sum(donut.family$Freq)*100, 2)
donut.family$ymax = cumsum(donut.family$perc)
donut.family$ymin = c(0, head(donut.family$ymax, n = -1))
donut.family$l.pos = (donut.family$ymax + donut.family$ymin)/2
donut.family$labels = paste0(donut.family$Var1, "\n", donut.family$perc)
donut.family[5,9] = NA
plot15 = ggplot(data = donut.family, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3.75, fill = Var1)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() + 
  scale_fill_brewer(palette = "Reds")+
  geom_text(x = 4.2, aes(y = l.pos, label = labels), size = 4 )+
  theme(legend.position = "none")


## Flag doc vs Target in Percnetage format
target.flagdoc = table(credit$FLAG_DOC, credit$TARGET)
flagdoc.df = as.data.frame(target.flagdoc)
flagdoc.df = ddply(flagdoc.df, "Var1", transform,
               perc = Freq / sum(Freq) * 100)
flagdoc.df[9,4] = 100-20.78
flagdoc.df[10,4] = 20.78
flagdoc.df[6,4] = 10.13
flagdoc.df[5,4] = 100-10.13

plot16 =ggplot(flagdoc.df, aes(x = Var1, y = perc, fill = Var2)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) + 
  ggtitle("Defaults by Document Errors", subtitle = "...") + 
  geom_text(aes(y = perc, label = round(perc, 2)), vjust = 1.5, colour = "Black") 


## Contract type vs target in Percentage format 
contract.target = table(credit$NAME_CONTRACT_TYPE, credit$TARGET)
contract.df = as.data.frame(contract.target)
contract.df = ddply(contract.df, "Var1", transform,
                   perc = Freq / sum(Freq) * 100)

plot17 = ggplot(data = contract.df, aes(x = Var1, fill = Var2, y = perc)) + 
  geom_bar(stat = "identity")  + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) + 
  ggtitle("Defaults by Type of Loan", subtitle = "...") + 
  geom_text(aes(y = perc, label = round(perc, 2)), vjust = 1.5, colour = "Black") 


## Gender vs target in Percentage Format
gender.target = table(credit$CODE_GENDER, credit$TARGET)
gender.df = as.data.frame(gender.target)
gender.df = ddply(gender.df, "Var1", transform,
                    perc = Freq / sum(Freq) * 100)

plot18 = ggplot(data = gender.df, aes(x = Var1, fill = Var2, y = perc)) + 
  geom_bar(stat = "identity")  + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) + 
  ggtitle("Defaults by Gender", subtitle = "...") + 
  geom_text(aes(y = perc, label = round(perc, 2)), vjust = 1.5, colour = "Black") 

## Income vs target boxplot
plot19 = ggplot(data = credit, aes(x = factor(TARGET), y = AMT_INCOME_TOTAL)) +
  geom_boxplot(notch = TRUE, notchwidth = 0.1) +
  ggtitle("Distribution of Income", subtitle = "Defaulters are slightly more common among the lower income clients with few exceptions") +
  xlab("Defaulters") + ylab("Income of Clients ($)") +
  theme(plot.title = element_text(face = "bold", color = "navy", size =18), plot.subtitle = element_text(color = "grey33", face = "italic", size = 10))


## Family member count vs Target in Percentage format
cnt.family.target
family.df = as.data.frame(cnt.family.target[,1:8])
family.df = ddply(family.df, "Var2", transform,
                 perc = Freq / sum(Freq) * 100)
family.df[14,4] = 17.797101
family.df[13,4] = 100-17.797101
family.df[2,4] = 7.969053
family.df[1,4] = 100-7.969053

plot20 = ggplot(data = family.df, aes(x = Var2, fill = Var1, y = perc)) + 
  geom_bar(stat = "identity")  + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) + 
  ggtitle("Defaults by No. of Family members", subtitle = "...") + 
  geom_text(aes(y = perc, label = round(perc, 2)), vjust = 1.5, colour = "Black") 


## Housing Type and Target
housing.type_target = table(credit$NAME_HOUSING_TYPE, credit$TARGET)
df.housing = as.data.frame(housing.type_target)
df.housing <- ddply(df.housing, "Var1", transform,
                    percent_weight = Freq / sum(Freq) * 100)

plot7 = ggplot(df.housing, aes(x = Var1, y = percent_weight, fill = Var2)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Pastel1") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA), 
        axis.text.x = element_text(angle = 90)) + 
  ggtitle("Defaults by Housing Type", subtitle = "...") + 
  geom_text(aes(y = percent_weight, label = round(percent_weight, 2)), vjust = 1.5, colour = "Black")

# Donut chart
donut.housing = df.housing[df.housing$Var2 == 1, ]
donut.housing$perc = round(donut.housing$Freq/sum(donut.housing$Freq)*100, 2)
donut.housing$ymax = cumsum(donut.housing$perc)
donut.housing$ymin = c(0, head(donut.housing$ymax, n = -1))
donut.housing$l.pos = (donut.housing$ymax + donut.housing$ymin)/2
donut.housing$labels = paste0(donut.housing$Var1, "\n", donut.housing$perc)
plot21 = ggplot(data = donut.housing, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3.75, fill = Var1)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "transparent", color = NA)) +
  geom_text(x = 4.2, aes(y = l.pos, label = labels), size = 4 )+
  theme(legend.position = "none")


# Single Dimension Plots of Income

plot22 = ggplot(data = credit, aes(x = NAME_INCOME_TYPE, y = AMT_INCOME_TOTAL, fill = NAME_INCOME_TYPE)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Greens") + 
  theme(plot.background = element_rect(fill = "transparent", color = NA), 
        axis.text.x = element_text(angle = 90) , 
          legend.position = "none")
