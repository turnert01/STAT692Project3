##################################
##################################
######### Load Packages ##########
##################################
##################################
library(readxl)
library(ggplot2)
library(dplyr)
library(asbio)
library(jmuOutlier)

##################################
##################################
########### Load Data ############
##################################
##################################

##Kidney Score Data 

###Group A 
groupAkidney = read_xlsx("Project 3-2.xlsx", #Path 
                         sheet="Kidney scores", #Sheet 
                         range="A3:C12", #Cells to extract
                         col_names=c("Sex",
                                     "MOUSE",
                                     "ScoreKidneys"))

###Group B
groupBkidney = read_xlsx("Project 3-2.xlsx", #Path
                         sheet="Kidney scores", #Sheet
                         range="A15:C23",
                         col_names=c("Sex",
                                     "MOUSE",
                                     "ScoreKidneys"))

###Group C 
groupCkidney = read_xlsx("Project 3-2.xlsx", #Path
                         sheet="Kidney scores", #Sheet
                         range="A26:C34",
                         col_names=c("Sex",
                                     "MOUSE",
                                     "ScoreKidneys"))

###Group D 
groupDkidney = read_xlsx("Project 3-2.xlsx", #Path
                         sheet="Kidney scores", #Sheet
                         range="A37:C45",
                         col_names=c("Sex",
                                     "MOUSE",
                                     "ScoreKidneys"))

##Joint Score Data

###Group A 
groupAjoint = read_xlsx("Project 3-2.xlsx", #Path 
                         sheet="Joint scores", #Sheet 
                         range="A3:D12", #Cells to extract
                         col_names=c("Sex",
                                     "Infected",
                                     "MOUSE",
                                     "AdaptedScore"))

###Group B 
groupBjoint = read_xlsx("Project 3-2.xlsx", #Path 
                        sheet="Joint scores", #Sheet 
                        range="A15:D23", #Cells to extract
                        col_names=c("Sex",
                                    "Infected",
                                    "MOUSE",
                                    "AdaptedScore"))

###Group C
groupCjoint = read_xlsx("Project 3-2.xlsx", #Path 
                        sheet="Joint scores", #Sheet 
                        range="A26:D34", #Cells to extract
                        col_names=c("Sex",
                                    "Infected",
                                    "MOUSE",
                                    "AdaptedScore"))

###Group D
groupDjoint = read_xlsx("Project 3-2.xlsx", #Path 
                        sheet="Joint scores", #Sheet 
                        range="A37:D45", #Cells to extract
                        col_names=c("Sex",
                                    "Infected",
                                    "MOUSE",
                                    "AdaptedScore"))

##################################
##################################
######## Data Manipulation #######
##################################
##################################

#Add group column to each data frame
#Add ID column to each data frame

##Kidney Data

###Group A
groupAkidney$"Group"= rep("A",10)

###Group B
groupBkidney$"Group"= rep("B",9)


###Group C
groupCkidney$"Group"= rep("C",9)


###Group D
groupDkidney$"Group"= rep("D",9)


#Now, merge all kidney group data frames into one
kidney = bind_rows(groupAkidney,
                   groupBkidney,
                   groupCkidney,
                   groupDkidney)
kidney$Sex = as.factor(kidney$Sex)
kidney$Group = as.factor(kidney$Group)

##Joint Data

###Group A
groupAjoint$"Group"= rep("A",10)

###Group B
groupBjoint$"Group"= rep("B",9)

###Group C
groupCjoint$"Group"= rep("C",9)

###Group D
groupDjoint$"Group"= rep("D",9)


#Now, merge all kidney group data frames into one
joint = bind_rows(groupAjoint,
                   groupBjoint,
                   groupCjoint,
                   groupDjoint)
joint$Sex = as.factor(joint$Sex)
joint$Group = as.factor(joint$Group)

##################################
##################################
############## EDA ###############
##################################
##################################

##Group A

###Kidney Score Data
ggplot(data=groupAkidney,
       aes(x=as.factor(Sex),
           y=ScoreKidneys,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Kidney Scores")+
  xlab("Sex")+
  ggtitle("Kidney Scores for Group A: Infected C3H Mice")+
  theme_minimal()

###Joint Score Data
ggplot(data=groupAjoint,
       aes(x=as.factor(Sex),
           y=AdaptedScore,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Adapted Joint Scores")+
  xlab("Sex")+
  ggtitle("Adapted Joint Scores for Group A: Infected C3H Mice")+
  theme_minimal()



##Group B

###Kidney Score Data
ggplot(data=groupBkidney,
       aes(x=as.factor(Sex),
           y=ScoreKidneys,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Kidney Scores")+
  xlab("Sex")+
  ggtitle("Kidney Scores for Group B: Uninfected C3H Mice")+
  theme_minimal()

###Joint Score Data
ggplot(data=groupBjoint,
       aes(x=as.factor(Sex),
           y=AdaptedScore,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Adapted Joint Scores")+
  xlab("Sex")+
  ggtitle("Adapted Joint Scores for Group B: Uninfected C3H Mice")+
  theme_minimal()


##Group C

###Kidney Score Data
ggplot(data=groupCkidney,
       aes(x=as.factor(Sex),
           y=ScoreKidneys,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Kidney Scores")+
  xlab("Sex")+
  ggtitle("Kidney Scores for Group C: Infected Peromyscus Mice")+
  theme_minimal()

###Joint Score Data
ggplot(data=groupCjoint,
       aes(x=as.factor(Sex),
           y=AdaptedScore,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Adapted Joint Scores")+
  xlab("Sex")+
  ggtitle("Adapted Joint Scores for Group C: Infected Peromyscus Mice")+
  theme_minimal()

##Group D

###Kidney Score Data
ggplot(data=groupDkidney,
       aes(x=as.factor(Sex),
           y=ScoreKidneys,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Kidney Scores")+
  xlab("Sex")+
  ggtitle("Kidney Scores for Group D: Uninfected Peromyscus Mice")+
  theme_minimal()
  
###Joint Score Data
ggplot(data=groupDjoint,
       aes(x=as.factor(Sex),
           y=AdaptedScore,
           group=as.factor(Sex),
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  ylab("Adapted Joint Scores")+
  xlab("Sex")+
  ggtitle("Adapted Joint Scores for Group D: Uninfected Peromyscus mice")+
  theme_minimal()



#Plots of comparisons, including all values

##Compare groups A and B

###Kidney Data
ggplot(data=kidney[kidney$Group=="A" | kidney$Group=="B",],
       aes(x=as.factor(Group),
           y=ScoreKidneys,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Kidney Score")+
  ggtitle("Kidney Scores for Groups A and B")

###Joint data
ggplot(data=joint[joint$Group=="A" | joint$Group=="B",],
       aes(x=as.factor(Group),
           y=AdaptedScore,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Adapted Joint Scores")+
  ggtitle("Adapted Joint Scores for Groups A and B")


##Compare groups C and D

###Kidney Data
ggplot(data=kidney[kidney$Group=="C" | kidney$Group=="D",],
       aes(x=as.factor(Group),
           y=ScoreKidneys,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Kidney Score")+
  ggtitle("Kidney Scores for Groups C and D")

###Joint data
ggplot(data=joint[joint$Group=="C" | joint$Group=="D",],
       aes(x=as.factor(Group),
           y=AdaptedScore,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Adapted Joint Scores")+
  ggtitle("Adapted Joint Scores for Groups C and D")


##Compare groups A and C

###Kidney Data
ggplot(data=kidney[kidney$Group=="A" | kidney$Group=="C",],
       aes(x=as.factor(Group),
           y=ScoreKidneys,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Kidney Score")+
  ggtitle("Kidney Scores for Groups A and C")

###Joint data
ggplot(data=joint[joint$Group=="A" | joint$Group=="C",],
       aes(x=as.factor(Group),
           y=AdaptedScore,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Adapted Joint Scores")+
  ggtitle("Adapted Joint Scores for Groups A and C")

##Compare groups B and D

###Kidney Data
ggplot(data=kidney[kidney$Group=="B" | kidney$Group=="D",],
       aes(x=as.factor(Group),
           y=ScoreKidneys,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Kidney Score")+
  ggtitle("Kidney Scores for Groups B and D")

###Joint data
ggplot(data=joint[joint$Group=="B" | joint$Group=="D",],
       aes(x=as.factor(Group),
           y=AdaptedScore,
           fill=as.factor(Sex)))+
  geom_dotplot(binaxis="y",
               stackdir="center",
               position="dodge")+
  scale_fill_discrete(name = "Sex")+
  theme_minimal()+
  xlab("Group")+
  ylab("Adapted Joint Scores")+
  ggtitle("Adapted Joint Scores for Groups B and D")



#Summary statistics

## Kidney Scores

###Group A
####Male
summary(groupAkidney[groupAkidney$Sex=="Male",
                     "ScoreKidneys"])
sd(groupAkidney[groupAkidney$Sex=="Male",
                "ScoreKidneys"]$ScoreKidneys)
length(groupAkidney[groupAkidney$Sex=="Male",
                    "ScoreKidneys"]$ScoreKidneys)

####Female
summary(groupAkidney[groupAkidney$Sex=="Female",
                     "ScoreKidneys"])
sd(groupAkidney[groupAkidney$Sex=="Female",
                "ScoreKidneys"]$ScoreKidneys)
length(groupAkidney[groupAkidney$Sex=="Female",
                    "ScoreKidneys"]$ScoreKidneys)

###Group B
####Male
summary(groupBkidney[groupBkidney$Sex=="Male",
                     "ScoreKidneys"])
sd(groupBkidney[groupBkidney$Sex=="Male",
                "ScoreKidneys"]$ScoreKidneys)
length(groupBkidney[groupBkidney$Sex=="Male",
                    "ScoreKidneys"]$ScoreKidneys)

####Female
summary(groupBkidney[groupBkidney$Sex=="Female",
                     "ScoreKidneys"])
sd(groupBkidney[groupBkidney$Sex=="Female",
                "ScoreKidneys"]$ScoreKidneys)
length(groupBkidney[groupBkidney$Sex=="Female",
                    "ScoreKidneys"]$ScoreKidneys)

###Group C
####Male
summary(groupCkidney[groupCkidney$Sex=="Male",
                     "ScoreKidneys"])
sd(groupCkidney[groupCkidney$Sex=="Male",
                "ScoreKidneys"]$ScoreKidneys)
length(groupCkidney[groupCkidney$Sex=="Male",
                    "ScoreKidneys"]$ScoreKidneys)

####Female
summary(groupCkidney[groupCkidney$Sex=="Female",
                     "ScoreKidneys"])
sd(groupCkidney[groupCkidney$Sex=="Female",
                "ScoreKidneys"]$ScoreKidneys)
length(groupCkidney[groupCkidney$Sex=="Female",
                    "ScoreKidneys"]$ScoreKidneys)

###Group D
####Males
summary(groupDkidney[groupDkidney$Sex=="Male",
                     "ScoreKidneys"])
sd(groupDkidney[groupDkidney$Sex=="Male",
                "ScoreKidneys"]$ScoreKidneys)
length(groupDkidney[groupDkidney$Sex=="Male",
                    "ScoreKidneys"]$ScoreKidneys)

####Females
summary(groupDkidney[groupDkidney$Sex=="Female",
                     "ScoreKidneys"])
sd(groupDkidney[groupDkidney$Sex=="Female",
                "ScoreKidneys"]$ScoreKidneys)
length(groupDkidney[groupDkidney$Sex=="Female",
                    "ScoreKidneys"]$ScoreKidneys)

##Joint Scores

###Group A
####Male
summary(groupAjoint[groupAjoint$Sex=="Male",
                     "AdaptedScore"])
sd(groupAjoint[groupAjoint$Sex=="Male",
                "AdaptedScore"]$AdaptedScore)
length(groupAjoint[groupAjoint$Sex=="Male",
                    "AdaptedScore"]$AdaptedScore)

####Female
summary(groupAjoint[groupAjoint$Sex=="Female",
                     "AdaptedScore"])
sd(groupAjoint[groupAjoint$Sex=="Female",
               "AdaptedScore"]$AdaptedScore)
length(groupAjoint[groupAjoint$Sex=="Female",
                   "AdaptedScore"]$AdaptedScore)

###Group B
####Male
summary(groupBjoint[groupBjoint$Sex=="Male",
                    "AdaptedScore"])
sd(groupBjoint[groupBjoint$Sex=="Male",
               "AdaptedScore"]$AdaptedScore)
length(groupBjoint[groupBjoint$Sex=="Male",
                   "AdaptedScore"]$AdaptedScore)

####Female
summary(groupBjoint[groupBjoint$Sex=="Female",
                    "AdaptedScore"])
sd(groupBjoint[groupBjoint$Sex=="Female",
               "AdaptedScore"]$AdaptedScore)
length(groupBjoint[groupBjoint$Sex=="Female",
                   "AdaptedScore"]$AdaptedScore)


###Group C
####Male
summary(groupCjoint[groupCjoint$Sex=="Male",
                    "AdaptedScore"])
sd(groupCjoint[groupCjoint$Sex=="Male",
               "AdaptedScore"]$AdaptedScore)
length(groupCjoint[groupCjoint$Sex=="Male",
                   "AdaptedScore"]$AdaptedScore)

####Female
summary(groupCjoint[groupCjoint$Sex=="Female",
                    "AdaptedScore"])
sd(groupCjoint[groupCjoint$Sex=="Female",
               "AdaptedScore"]$AdaptedScore)
length(groupCjoint[groupCjoint$Sex=="Female",
                   "AdaptedScore"]$AdaptedScore)

###Group D
####Male
summary(groupDjoint[groupDjoint$Sex=="Male",
                    "AdaptedScore"])
sd(groupDjoint[groupDjoint$Sex=="Male",
               "AdaptedScore"]$AdaptedScore)
length(groupDjoint[groupDjoint$Sex=="Male",
                   "AdaptedScore"]$AdaptedScore)

####Female
summary(groupDjoint[groupDjoint$Sex=="Female",
                    "AdaptedScore"])
sd(groupDjoint[groupDjoint$Sex=="Female",
               "AdaptedScore"]$AdaptedScore)
length(groupDjoint[groupDjoint$Sex=="Female",
                   "AdaptedScore"]$AdaptedScore)


##################################
##################################
###### Statistical Analysis ######
##################################
##################################

#Kidney Data

#Perform permutation test for factorial 
#experiment
kid1=perm.fact.test(Y=kidney$ScoreKidneys,
               X1=kidney$Group,
               X2=kidney$Sex)

latex_table <- xtable(kid1$Table,
                      caption="Two Factor Permutation Test on Kidney Experiment")
print(latex_table,
      booktabs = TRUE)

#Overall, sex is not a significant factor
##There is not difference between male and female
##But the interaction effect is significant
##Difference in sexes depends on group


#Make comparisons

#Comparison 1: Groups A and B 
##Male
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Male",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="A" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="B" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Female
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Female",]$ScoreKidneys)-
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="A" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          y=kidney[kidney$Group=="B" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)

#Comparison 2: Groups C and D 
##Male
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Male",]$ScoreKidneys)
perm.test(x=kidney[kidney$Group=="C" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="D" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Female
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Female",]$ScoreKidneys)-
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="C" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          y=kidney[kidney$Group=="D" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)


#Comparison 3: Groups A and C 
##Male
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Male",]$ScoreKidneys)
perm.test(x=kidney[kidney$Group=="A" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="C" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Female
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Female",]$ScoreKidneys)-
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="A" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          y=kidney[kidney$Group=="C" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)


#Comparison 4: Groups B and D
##Male
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Male",]$ScoreKidneys)
perm.test(x=kidney[kidney$Group=="B" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="D" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Female
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Female",]$ScoreKidneys)-
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="B" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          y=kidney[kidney$Group=="D" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)


#Comparison 5: Male vs. Female for each group
##Group A
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="A" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="A" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="A" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Group B
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="B" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="B" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="B" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)

##Group C
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="C" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="C" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="C" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)


##Group D
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Male",]$ScoreKidneys)-
mean(kidney[kidney$Group=="D" &
              kidney$Sex=="Female",]$ScoreKidneys)

perm.test(x=kidney[kidney$Group=="D" &
                     kidney$Sex=="Male",]$ScoreKidneys,
          y=kidney[kidney$Group=="D" &
                     kidney$Sex=="Female",]$ScoreKidneys,
          perm=5000,
          plot=F)


#Between group differences depended on sex
#Differences between sex within group were all non-significant
#12 tests conducted for equality of means
#Bonferrni correction will be .05/12 = 0.00417




#Kidney Data

#Perform permutation test for factorial 
#experiment
kid1=perm.fact.test(Y=kidney$ScoreKidneys,
                    X1=kidney$Group,
                    X2=kidney$Sex)

latex_table <- xtable(kid1$Table,
                      caption="Two Factor Permutation Test on Kidney Experiment")
print(latex_table,
      booktabs = TRUE)

#Overall, sex is not a significant factor
##There is not difference between male and female
##But the interaction effect is significant
##Difference in sexes depends on group


#Joint data

#Perform permutation test for factorial 
#experiment
joint1=perm.fact.test(Y=joint$AdaptedScore,
                    X1=joint$Group,
                    X2=joint$Sex)

latex_table <- xtable(joint1$Table,
                      caption="Two Factor Permutation Test on Joint Experiment")
print(latex_table,
      booktabs = TRUE)


#Make group comparisons

##Group A - Group B
mean(joint[joint$Group=="A",]$AdaptedScore)-
  mean(joint[joint$Group=="B" ,]$AdaptedScore)

perm.test(x=joint[joint$Group=="A",]$AdaptedScore,
          y=joint[joint$Group=="B",]$AdaptedScore,
          perm=5000,
          plot=F)

##Group C - Group D
mean(joint[joint$Group=="C",]$AdaptedScore)-
  mean(joint[joint$Group=="D" ,]$AdaptedScore)

perm.test(x=joint[joint$Group=="C",]$AdaptedScore,
          y=joint[joint$Group=="D",]$AdaptedScore,
          perm=5000,
          plot=F)

##Group A - Group C
mean(joint[joint$Group=="A",]$AdaptedScore)-
  mean(joint[joint$Group=="C" ,]$AdaptedScore)

perm.test(x=joint[joint$Group=="A",]$AdaptedScore,
          y=joint[joint$Group=="C",]$AdaptedScore,
          perm=5000,
          plot=F)


##Group B - Group D
mean(joint[joint$Group=="B",]$AdaptedScore)-
  mean(joint[joint$Group=="D" ,]$AdaptedScore)

perm.test(x=joint[joint$Group=="B",]$AdaptedScore,
          y=joint[joint$Group=="D",]$AdaptedScore,
          perm=5000,
          plot=F)

