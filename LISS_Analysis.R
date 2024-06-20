library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(car)
library(lme4)
library(lmerTest)
library(plm)
library(panelr)
library(lattice)
library(texreg)

####--------------------------------------------------------------------------------------------------------
##Dependent Variable: prejudice against working mother
##Independent Variable: Married, WithChildren, SatisfactionRelationship (Partner)
##Control: AgeGroup, Male, Urbanization, NewIncomeGroup, EduGroup
##InteractionTerm:Gender*Satisfaction

####--------------------------------------------------------------------------------------------------------
##Univariate descriptive:
summary(Complete_15Waves)

####--------------------------------------------------------------------------------------------------------
##bivariate descriptive

#Prejudice against working mother and the Independent variable:
#
#Prejudice & Married:
tapply(Complete_15Waves$PrejWorkMumStand, factor(Complete_15Waves$Married), mean)
MarriedANOVA<-aov(PrejWorkMumStand~factor(Married), data = Complete_15Waves)
summary(MarriedANOVA)
##No married -0.256 VS. married: 0.0829 --> significantly different
#
#Prejudice & with children
tapply(Complete_15Waves$PrejWorkMumStand, factor(Complete_15Waves$WithChildren), mean)
ChildrenANOVA<-aov(PrejWorkMumStand~factor(WithChildren), data = Complete_15Waves)
summary(ChildrenANOVA)
##no children 0.046 VS. with children: -0.069 --> significantly different
#
#Prejudice & relationship satisfaction
cor.test(Complete_15Waves$PrejWorkMumStand, Complete_15Waves$SatisfactionRelationship, method="pearson")
##0.005, not significant
cor.test(Complete_15Waves$PrejWorkMumStand, Complete_15Waves$urbanization, method="pearson")
cor.test(Complete_15Waves$SatisfactionRelationship, Complete_15Waves$urbanization, method="pearson")


#Prejudice against working mother and control variable:
#
#Prejudice & Age Group:
tapply(Complete_15Waves$PrejWorkMumStand, Complete_15Waves$AgeGroupCat, mean)
AgeGroupANOVA<-aov(PrejWorkMumStand~AgeGroupCat, data = Complete_15Waves)
summary(AgeGroupANOVA)
##significantly differ across age group
#
#Prejudice & gender
tapply(Complete_15Waves$PrejWorkMumStand, factor(Complete_15Waves$male), mean)
GenderANOVA<-aov(PrejWorkMumStand~factor(male), data = Complete_15Waves)
summary(GenderANOVA)
##women: -0.107 VS. male: 0.113 --> significantly different
#
#Prejudice & urbanization
cor.test(Complete_15Waves$PrejWorkMumStand, Complete_15Waves$urbanization, method="pearson")
#-0.061 --> significant
#
#Prejudice & New Income group
tapply(Complete_15Waves$PrejWorkMumStand, factor(Complete_15Waves$NewIncomeGroup), mean)
IncomeANOVA<-aov(PrejWorkMumStand~factor(NewIncomeGroup), data = Complete_15Waves)
summary(IncomeANOVA)
#low: 0.144 VS. middle: -0.118 VS. high: -0.255 --> significantly different
#
#Prejudice & Edu Group
tapply(Complete_15Waves$PrejWorkMumStand, factor(Complete_15Waves$EduGroup), mean)
EduANOVA<-aov(PrejWorkMumStand~factor(EduGroup), data = Complete_15Waves)
summary(EduANOVA)
#low edu: 0.348 VS. mid edu: 0.013 VS. high edu: -0.297 --> significantly different

##Kendall's tau b for income, education, and age group:
cor.test(Complete_15Waves$AgeGroup, Complete_15Waves$IncomeGroup, method="kendall")
cor.test(as.numeric(Complete_15Waves$AgeGroup), as.numeric(Complete_15Waves$EduGroup), method="kendall")
cor.test(as.numeric(Complete_15Waves$EduGroup), as.numeric(Complete_15Waves$IncomeGroup), method="kendall")

####--------------------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------------------
##multilevel model
####--------------------------------------------------------------------------------------------------------

##First, we need to see if the multilevel structure is needed:
#
#ICC:
MLM_empty<-lmer(formula = PrejWorkMumStand~1+(1|IndividualID),
                data = Complete_15Waves)
summary(MLM_empty)
0.7260/(0.7260+0.2752)
#ICC=72.51%
#
#Likelihood ratio test:
OLS_empty<-lm(formula = PrejWorkMumStand~1,
              data = Complete_15Waves)
summary(OLS_empty)
anova(MLM_empty, OLS_empty, test="Chisq")
#Chi2=45324, df=1, p<0.001
#
#Breusch-Pagan Test 
Panel_Complete<-panel_data(data = Complete_15Waves,
                           id=IndividualID,
                           wave = wave)
Prej_RE<-plm(formula = PrejWorkMumStand~Married+WithChildren+SatisfactionRelationship,
             data = Panel_Complete,
             model = "random")
plmtest(Prej_RE,type="bp")
#significant
##Conclusion: need multilevel structure

####--------------------------------------------------------------------------------------------------------
###Then, we inspect the time trend
#
#We need to first construct the time variable:
Complete_15Waves<-Complete_15Waves %>%
        mutate(time=wave-1)
#Then we need to construct the time-square variable:
Complete_15Waves<-Complete_15Waves %>%
        mutate(timesq=time^2)
##Next, we use a different data frame to have 14 time dummies to see if we can see any trend
Complete_15Waves_TimeDummies<-Complete_15Waves %>%
        mutate(Wave1=case_when(
                wave == 1 ~ 1,
                wave != 1 ~ 0
        ),
        Wave2=case_when(
                wave == 2 ~ 1,
                wave != 2 ~ 0
        ),
        Wave3=case_when(
                wave == 3 ~ 1,
                wave != 3 ~ 0
        ),
        Wave4=case_when(
                wave == 4 ~ 1,
                wave != 4 ~ 0
        ),
        Wave5=case_when(
                wave == 5 ~ 1,
                wave != 5 ~ 0
        ),
        Wave6=case_when(
                wave == 6 ~ 1,
                wave != 6 ~ 0
        ),
        Wave7=case_when(
                wave == 7 ~ 1,
                wave != 7 ~ 0
        ),
        Wave8=case_when(
                wave == 8 ~ 1,
                wave != 8 ~ 0
        ),
        Wave9=case_when(
                wave == 9 ~ 1,
                wave != 9 ~ 0
        ),
        Wave10=case_when(
                wave == 10 ~ 1,
                wave != 10 ~ 0
        ),
        Wave11=case_when(
                wave == 11 ~ 1,
                wave != 11 ~ 0
        ),
        Wave12=case_when(
                wave == 12 ~ 1,
                wave != 12 ~ 0
        ),
        Wave13=case_when(
                wave == 13 ~ 1,
                wave != 13 ~ 0
        ),
        Wave14=case_when(
                wave == 14 ~ 1,
                wave != 14 ~ 0
        ),
        Wave15=case_when(
                wave == 15 ~ 1,
                wave != 15 ~ 0
        ))
#Now, we use time dummies to see if there are any time trend or turning point:
PrejModel_TimeDummies<-lmer(formula = PrejWorkMumStand~1+Wave2+Wave3+Wave4+Wave5+
                                    Wave6+Wave7+Wave8+Wave9+Wave10+Wave11+Wave12+
                                    Wave13+Wave14+Wave15+(1|IndividualID),
                            data = Complete_15Waves_TimeDummies)
summary(PrejModel_TimeDummies)
##The results shows a consistent downward trend
#
#Use only time:
PrejModel_Time<-lmer(formula = PrejWorkMumStand~1+time+(1+time|IndividualID),
                     data = Complete_15Waves_TimeDummies)
summary(PrejModel_Time)
#Time negative effect: -0.037, significant
#
#Since there seems to be a lot of variations, we will also ask for the caterpillar plot
uj<-ranef(PrejModel_Time, condvar=TRUE)
dotplot(uj, scales(list(x=list(relation='free'))))
#Due to the high number of individuals, the caterpillar plot does not reveal anything
#
#Now try with timesq:
PrejModel_TimeSQ<-lmer(formula = PrejWorkMumStand~1+time+timesq+(1+time+timesq|IndividualID),
                     data = Complete_15Waves_TimeDummies)
summary(PrejModel_TimeSQ)
#
#
#
##Predict without random effect
Complete_15Waves_TimeDummies$AvePred<-predict(PrejModel_TimeSQ, re.form=NA)
#
##Predict with random effect
Complete_15Waves_TimeDummies$pred<-predict(PrejModel_TimeSQ)
#
##Plotting:
#For appendix
ggplot(data = Complete_15Waves_TimeDummies)+
        geom_line(aes(x=time, y=pred, group=IndividualID))+
        geom_line(data = Complete_15Waves_TimeDummies, aes(x=time, y=AvePred),color="red", linewidth=1.5)
#
#Select the people who participate in all 15 rounds for the plotting
participation<-Complete_15Waves_TimeDummies %>%
        group_by(IndividualID) %>%
        summarise(count=n())
participation<-participation %>%
        filter(count==15)
Complete_15Waves_TimeDummies<-merge(Complete_15Waves_TimeDummies, participation,
                                    by.x = "IndividualID",
                                    by.y = "IndividualID")
#plot the people who participate in 15 waves:
ggplot(data = Complete_15Waves_TimeDummies)+
        geom_line(aes(x=time, y=pred, group=IndividualID))+
        geom_line(data = Complete_15Waves_TimeDummies, aes(x=time, y=AvePred),color="red", linewidth=1.5)
#
#Select only 100 people:
Complete_15Waves_TimeDummies<-Complete_15Waves_TimeDummies[1:1500,]
#plot only 100 people who participate in 15 waves:
ggplot(data = Complete_15Waves_TimeDummies)+
        geom_line(aes(x=time, y=pred, group=IndividualID, color=IndividualID))+
        geom_line(data = Complete_15Waves_TimeDummies, aes(x=time, y=AvePred),color="red", linewidth=1.5)+
        xlab("time point")+ylab("predicted prejudice against working mother")+
        labs(legend=FALSE)
#
##For the main text
ggplot(data = Complete_15Waves_TimeDummies, aes(x=time, y=AvePred))+
        geom_line(linewidth=1, color="red")+
        xlab("time")+ylab("Average Prejudice against working mother")+
        labs(title = "Average Evolution of Prejudice Against Working Mother Over Time")
#
##We will keep the model with time and timesq both ranef for the subsequent analysis

####--------------------------------------------------------------------------------------------------------
##Now we start the modeling step by step:

####--------------------------------------------------------------------------------------------------------
#M0: with time and timesq (Put in table):
Prej_M0_Time<-lmer(formula = PrejWorkMumStand~1+time+timesq+(1+time+timesq | IndividualID),
              data = Complete_15Waves)
summary(Prej_M0_Time)

####--------------------------------------------------------------------------------------------------------
#Fixed effect (NOT put in the table):
#first, make the data as panel data:
PrejPanel_Complete<-pdata.frame(Complete_15Waves,
                                index = c("IndividualID","wave"),
                                drop.index = TRUE)
##Fixed Effect Model
Prej_FE<-plm(PrejWorkMumStand~Married+WithChildren+SatisfactionRelationship,
             data = PrejPanel_Complete,
             model = "within")
summary(Prej_FE)
##Fixed effect model shows all three factors are significant
#
##Random effect model:
Prej_RE<-plm(PrejWorkMumStand~Married+WithChildren+SatisfactionRelationship,
             data = PrejPanel_Complete,
             model = "random")
summary(Prej_RE)
#
##Hausman test:
phtest(Prej_FE, Prej_RE)
##The random effect model is biased.
#
#
##Mundlak model to confirm:
#First request the mean across the three independent variables:
Complete_15Waves<-Complete_15Waves %>%
        group_by(IndividualID) %>%
        mutate(Married_mean=mean(Married),
               WithChildren_mean=mean(WithChildren),
               Satisfaction_mean=mean(SatisfactionRelationship))
#
#Fit the Mundlak model:
Prej_Mundlak<-lmer(PrejWorkMumStand~1+Married+WithChildren+SatisfactionRelationship+
                          Married_mean+WithChildren_mean+Satisfaction_mean+(1|IndividualID),
                  data = Complete_15Waves)
summary(Prej_Mundlak)
##The effect of Married_mean, WithChildren_mean, Satisfaction_mean is the between and within difference
#
screenreg(list(Prej_FE, Prej_Mundlak))
##Mundlak model suggests that there are between and within effect difference
#
#Decide to do the decomposition of within and between effect in the next step
rm(Prej_RE)

####--------------------------------------------------------------------------------------------------------
#The within-between model testing and trying with time as well:
#
#First we decompose the Married, With Children, Satisfaction:
#
##In the Mundlak model, we have already did the mean
Complete_15Waves<-Complete_15Waves %>%
        group_by(IndividualID) %>%
        mutate(Married_mean=mean(Married),
               WithChildren_mean=mean(WithChildren),
               Satisfaction_mean=mean(SatisfactionRelationship))
#
#Now, we need to de-mean them:
Complete_15Waves<-Complete_15Waves %>%
        mutate(Married_dm=Married-Married_mean,
               WithChildren_dm=WithChildren-WithChildren_mean,
               Satisfaction_dm=SatisfactionRelationship-Satisfaction_mean)
#
##M1 without time: Between-Within model:
Prej_M1_noTime<-lmer(formula = PrejWorkMumStand~1+Married_dm+WithChildren_dm+Satisfaction_dm+
                             Married_mean+WithChildren_mean+Satisfaction_mean+
                             (1|IndividualID),
                     data = Complete_15Waves)
summary(Prej_M1_noTime)
#
#M1 with time variables:
Prej_M1<-lmer(formula = PrejWorkMumStand~1+time+timesq+Married_dm+WithChildren_dm+Satisfaction_dm+
                      Married_mean+WithChildren_mean+Satisfaction_mean+
                      (1+time+timesq|IndividualID),
              data = Complete_15Waves)
summary(Prej_M1)
#Slide P141: as the growth curve is a constant characteristic of each unit, 
##it conceptually makes only sense to have time-constant variables as influences on them (Married_mean, WithChildren_mean, Satisfaction_mean)
##time-varying variables, however, can be included in the model, to partial out their effects
#That's why the time-varying variable (Married_dm, WithChildren_dm, Satisfaction_dm) behaves weird and can only be seen as partialing out the effects
#
##For this reason, we will only use model without time from now on:

####--------------------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------------------
##The correct specification for the within-between model for the report:
#
#same decomposition of the Married, With Children, Satisfaction:
#
##In the Mundlak model, we have already did the mean
Complete_15Waves<-Complete_15Waves %>%
        group_by(IndividualID) %>%
        mutate(Married_mean=mean(Married),
               WithChildren_mean=mean(WithChildren),
               Satisfaction_mean=mean(SatisfactionRelationship))
#
#Now, we need to de-mean them:
Complete_15Waves<-Complete_15Waves %>%
        mutate(Married_dm=Married-Married_mean,
               WithChildren_dm=WithChildren-WithChildren_mean,
               Satisfaction_dm=SatisfactionRelationship-Satisfaction_mean)
#
##M0: Empty model:
Prej_M0<-lmer(formula = PrejWorkMumStand~1+(1|IndividualID),
              data = Complete_15Waves)
summary(Prej_M0)
#
##M1: Within-Between Model without control:
Prej_M1<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                      WithChildren_dm+WithChildren_mean+
                      Satisfaction_dm+Satisfaction_mean+(1|IndividualID),
              data = Complete_15Waves)
summary(Prej_M1)
#
##M2: with control variable:
Prej_M2<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                      WithChildren_dm+WithChildren_mean+
                      Satisfaction_dm+Satisfaction_mean+
                      AgeGroupCat+male+urbanization+NewIncomeGroup+EduGroup+
                      (1|IndividualID),
              data = Complete_15Waves)
summary(Prej_M2)
#
#Interaction: Male with Married_mean
Prej_M3_MarriedInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                      WithChildren_dm+WithChildren_mean+
                      Satisfaction_dm+Satisfaction_mean+
                      AgeGroupCat+male+male*Married_mean+
                      urbanization+NewIncomeGroup+EduGroup+
                      (1|IndividualID),
              data = Complete_15Waves)
summary(Prej_M3_MarriedInt)
#Not significant
#
#Interaction: Male with Married_dm
Prej_M3_MarriedDMInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                 WithChildren_dm+WithChildren_mean+
                                 Satisfaction_dm+Satisfaction_mean+
                                 AgeGroupCat+male+male*Married_dm+
                                 urbanization+NewIncomeGroup+EduGroup+
                                 (1+Married_dm|IndividualID),
                         data = Complete_15Waves)
summary(Prej_M3_MarriedDMInt)
#Not significant
#
#Interaction:Male with WithChildren_mean
Prej_M3_ChildInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                      WithChildren_dm+WithChildren_mean+
                      Satisfaction_dm+Satisfaction_mean+
                      AgeGroupCat+male+male*WithChildren_mean+
                      urbanization+NewIncomeGroup+EduGroup+
                      (1|IndividualID),
              data = Complete_15Waves)
summary(Prej_M3_ChildInt)
#The negative between-effect of having children is stronger among males
#
##Interaction:Male with WithChildren_dm
Prej_M3_ChildDMInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                               WithChildren_dm+WithChildren_mean+
                               Satisfaction_dm+Satisfaction_mean+
                               AgeGroupCat+male+male*WithChildren_dm+
                               urbanization+NewIncomeGroup+EduGroup+
                               (1+WithChildren_dm|IndividualID),
                       data = Complete_15Waves)
summary(Prej_M3_ChildDMInt)
f#Not significant
#
#Interaction: Male with Satisfaction_mean:
Prej_M3_SatisInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                               WithChildren_dm+WithChildren_mean+
                               Satisfaction_dm+Satisfaction_mean+
                               AgeGroupCat+male+male*Satisfaction_mean+
                               urbanization+NewIncomeGroup+EduGroup+
                               (1|IndividualID),
                       data = Complete_15Waves)
summary(Prej_M3_SatisInt)
#The negative between-effect of satisfaction is weaker among males
#
##Interaction: Male with Satisfaction_dm:
Prej_M3_SatisDMInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                               WithChildren_dm+WithChildren_mean+
                               Satisfaction_dm+Satisfaction_mean+
                               AgeGroupCat+male+male*Satisfaction_dm+
                               urbanization+NewIncomeGroup+EduGroup+
                               (1+Satisfaction_dm|IndividualID),
                       data = Complete_15Waves)
summary(Prej_M3_SatisDMInt)
##Not significant

##Results of the significant results model
screenreg(list(Prej_M0, Prej_M1, Prej_M2, Prej_M3_ChildInt, Prej_M3_SatisInt), digits = 5)


###-----------------------------------------------------
##To confirm that the control is really explaining away some variances of 
#the effect of three dm variable:
#We make random slope for without control and with control
#Random slope testing:Married_dm without control
Prej_MarriedDMSlope_NoCon<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                     WithChildren_dm+WithChildren_mean+
                                     Satisfaction_dm+Satisfaction_mean+
                                     (1+Married_dm|IndividualID),
                             data = Complete_15Waves)
summary(Prej_MarriedDMSlope_NoCon)
#
#Random Slope testing: Married_dm with control
Prej_MarriedDMSlope_Con<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                        WithChildren_dm+WithChildren_mean+
                                        Satisfaction_dm+Satisfaction_mean+
                                        AgeGroupCat+male+
                                        urbanization+NewIncomeGroup+EduGroup+
                                        (1+Married_dm|IndividualID),
                                data = Complete_15Waves)
summary(Prej_MarriedDMSlope_Con)
#
#Random Slope testing: Married_dm with control as interaction effect
Prej_MarriedDMSlope_ConInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                      WithChildren_dm+WithChildren_mean+
                                      Satisfaction_dm+Satisfaction_mean+
                                      AgeGroupCat+male+
                                      urbanization+NewIncomeGroup+EduGroup+
                                      Married_dm*AgeGroupCat+Married_dm*male+
                                         Married_dm*urbanization+Married_dm*NewIncomeGroup+
                                         Married_dm*EduGroup+
                                      (1+Married_dm|IndividualID),
                              data = Complete_15Waves)
summary(Prej_MarriedDMSlope_ConInt)
#
screenreg(list(Prej_MarriedDMSlope_NoCon, Prej_MarriedDMSlope_Con), digits=5)
##Exaplained variance of the random slope: 16.79%
(0.18363-0.1528)/0.18363
#
#
#Random slope testing:WithChildren_dm without control
Prej_ChildDMSlope_NoCon<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                        WithChildren_dm+WithChildren_mean+
                                        Satisfaction_dm+Satisfaction_mean+
                                        (1+WithChildren_dm|IndividualID),
                                data = Complete_15Waves)
summary(Prej_ChildDMSlope_NoCon)
#
#Random Slope testing: WithChildren_dm with control
Prej_ChildDMSlope_Con<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                      WithChildren_dm+WithChildren_mean+
                                      Satisfaction_dm+Satisfaction_mean+
                                      AgeGroupCat+male+
                                      urbanization+NewIncomeGroup+EduGroup+
                                      (1+WithChildren_dm|IndividualID),
                              data = Complete_15Waves)
summary(Prej_ChildDMSlope_Con)
#
#Random Slope testing: WithChildren_dm with control and interaction
Prej_ChildDMSlope_ConInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                    WithChildren_dm+WithChildren_mean+
                                    Satisfaction_dm+Satisfaction_mean+
                                    AgeGroupCat+male+
                                    urbanization+NewIncomeGroup+EduGroup+
                                       WithChildren_dm*AgeGroupCat+WithChildren_dm*male+
                                       WithChildren_dm*urbanization+WithChildren_dm*NewIncomeGroup+
                                       WithChildren_dm*EduGroup+
                                    (1+WithChildren_dm|IndividualID),
                            data = Complete_15Waves)
summary(Prej_ChildDMSlope_ConInt)
#
screenreg(list(Prej_ChildDMSlope_NoCon, Prej_ChildDMSlope_Con), digits=5)
##Exaplained variance of the random slope: 26.21%
(0.13823-0.10200)/0.13823
#
#
#Random slope testing:Satisfaction_dm without control
Prej_SatisDMSlope_NoCon<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                      WithChildren_dm+WithChildren_mean+
                                      Satisfaction_dm+Satisfaction_mean+
                                      (1+Satisfaction_dm|IndividualID),
                              data = Complete_15Waves)
summary(Prej_SatisDMSlope_NoCon)
#
#Random Slope testing: Satisfaction_dm with control
Prej_SatisDMSlope_Con<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                    WithChildren_dm+WithChildren_mean+
                                    Satisfaction_dm+Satisfaction_mean+
                                    AgeGroupCat+male+
                                    urbanization+NewIncomeGroup+EduGroup+
                                    (1+Satisfaction_dm|IndividualID),
                            data = Complete_15Waves)
summary(Prej_SatisDMSlope_Con)
#
#Random Slope testing: Satisfaction_dm with control and interaction
Prej_SatisDMSlope_ConInt<-lmer(formula = PrejWorkMumStand~1+Married_dm+Married_mean+
                                    WithChildren_dm+WithChildren_mean+
                                    Satisfaction_dm+Satisfaction_mean+
                                    AgeGroupCat+male+
                                    urbanization+NewIncomeGroup+EduGroup+
                                    Satisfaction_dm*AgeGroupCat+Satisfaction_dm*male+
                                    Satisfaction_dm*urbanization+Satisfaction_dm*NewIncomeGroup+
                                    Satisfaction_dm*EduGroup+
                                    (1+Satisfaction_dm|IndividualID),
                            data = Complete_15Waves)
summary(Prej_SatisDMSlope_ConInt)
#
screenreg(list(Prej_SatisDMSlope_NoCon, Prej_SatisDMSlope_Con), digits=5)
##Exaplained variance of the random slope: 14.94%
(0.01640-0.01395)/0.01640
