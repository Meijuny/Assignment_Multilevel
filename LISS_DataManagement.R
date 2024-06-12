library(haven)
library(dplyr)
library(lavaan)

####--------------------------------------------------------------------------------------------------------------
##Dataset with individual change of having children (without the change in number of children)
##Wave 15
##Select the sexism item:
Sexism_W15<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave15.sav")

Sexism_W15<-Sexism_W15 %>%
        dplyr::select(nomem_encr, cv23o110, cv23o111, cv23o113)

Sexism_W15$Wave<-rep(15, times=6221)

##Select the family background item:
Family_W15<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave15.sav")

Family_W15<-Family_W15 %>%
        dplyr::select(nomem_encr, cf22o030, cf22o180)

##Select the demographic items:
Demo_W15<-haven::read_sav("./LISS Data/Background info/Background_Wave15.sav")

Demo_W15<-Demo_W15 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W15<-merge(Sexism_W15, Family_W15, 
                         by.x = "nomem_encr", 
                         by.y = "nomem_encr")
Complete_W15<-merge(Complete_W15, Demo_W15, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W15<-na.omit(Complete_W15)

##Rename the variables:
colnames(Complete_W15)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                               "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W15<-Complete_W15 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W15","Family_W15","Demo_W15"))


####--------------------------------------------------------------------------------------------------------------
##Wave 14
##Select the sexism item:
Sexism_W14<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave14.sav")

Sexism_W14<-Sexism_W14 %>%
        dplyr::select(nomem_encr, cv22n110, cv22n111, cv22n113)

Sexism_W14$Wave<-rep(14, times=5626)

##Select the family background item:
Family_W14<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave14.sav")

Family_W14<-Family_W14 %>%
        dplyr::select(nomem_encr, cf21n030, cf21n180)

##Select the demographic items:
Demo_W14<-haven::read_sav("./LISS Data/Background info/Background_Wave14.sav")

Demo_W14<-Demo_W14 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W14<-merge(Sexism_W14, Family_W14, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W14<-merge(Complete_W14, Demo_W14, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W14<-na.omit(Complete_W14)

##Rename the variables:
colnames(Complete_W14)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                          "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W14<-Complete_W14 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W14","Family_W14","Demo_W14"))


####--------------------------------------------------------------------------------------------------------------
##Wave 13
##Select the sexism item:
Sexism_W13<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave13.sav")

Sexism_W13<-Sexism_W13 %>%
        dplyr::select(nomem_encr, cv21m110, cv21m111, cv21m113)

Sexism_W13$Wave<-rep(13, times=6323)

##Select the family background item:
Family_W13<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave13.sav")

Family_W13<-Family_W13 %>%
        dplyr::select(nomem_encr, cf20m030, cf20m180)

##Select the demographic items:
Demo_W13<-haven::read_sav("./LISS Data/Background info/Background_Wave13.sav")

Demo_W13<-Demo_W13 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W13<-merge(Sexism_W13, Family_W13, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W13<-merge(Complete_W13, Demo_W13, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W13<-na.omit(Complete_W13)

##Rename the variables:
colnames(Complete_W13)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                          "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W13<-Complete_W13 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W13","Family_W13","Demo_W13"))

####--------------------------------------------------------------------------------------------------------------
##Wave 12
##Select the sexism item:
Sexism_W12<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave12.sav")

Sexism_W12<-Sexism_W12 %>%
        dplyr::select(nomem_encr, cv20l110, cv20l111, cv20l113)

Sexism_W12$Wave<-rep(12, times=6286)

##Select the family background item:
Family_W12<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave12.sav")

Family_W12<-Family_W12 %>%
        dplyr::select(nomem_encr,  cf19l030, cf19l180)

##Select the demographic items:
Demo_W12<-haven::read_sav("./LISS Data/Background info/Background_Wave12.sav")

Demo_W12<-Demo_W12 %>%
        dplyr::select(nomem_encr,partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W12<-merge(Sexism_W12, Family_W12, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W12<-merge(Complete_W12, Demo_W12, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W12<-na.omit(Complete_W12)

##Rename the variables:
colnames(Complete_W12)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                          "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W12<-Complete_W12 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W12","Family_W12","Demo_W12"))

####--------------------------------------------------------------------------------------------------------------
##Wave 11
##Select the sexism item:
Sexism_W11<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave11.sav")

Sexism_W11<-Sexism_W11 %>%
        dplyr::select(nomem_encr, cv19k110, cv19k111, cv19k113)

Sexism_W11$Wave<-rep(11, times=5641)

##Select the family background item:
Family_W11<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave11.sav")

Family_W11<-Family_W11 %>%
        dplyr::select(nomem_encr, cf18k030, cf18k180)

##Select the demographic items:
Demo_W11<-haven::read_sav("./LISS Data/Background info/Background_Wave11.sav")

Demo_W11<-Demo_W11 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W11<-merge(Sexism_W11, Family_W11, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W11<-merge(Complete_W11, Demo_W11, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W11<-na.omit(Complete_W11)

##Rename the variables:
colnames(Complete_W11)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                          "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W11<-Complete_W11 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W11","Family_W11","Demo_W11"))


####--------------------------------------------------------------------------------------------------------------
##Wave 10
##Select the sexism item:
Sexism_W10<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave10.sav")

Sexism_W10<-Sexism_W10 %>%
        dplyr::select(nomem_encr, cv18j110, cv18j111, cv18j113)

Sexism_W10$Wave<-rep(10, times=6263)

##Select the family background item:
Family_W10<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave10.sav")

Family_W10<-Family_W10 %>%
        dplyr::select(nomem_encr, cf17j030, cf17j180)

##Select the demographic items:
Demo_W10<-haven::read_sav("./LISS Data/Background info/Background_Wave10.sav")

Demo_W10<-Demo_W10 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W10<-merge(Sexism_W10, Family_W10, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W10<-merge(Complete_W10, Demo_W10, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W10<-na.omit(Complete_W10)

##Rename the variables:
colnames(Complete_W10)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                          "wave", "Married", "SatisfactionRelationship","partner",
                          "domesticSituation", "AgeGroup","Gender", 
                          "Rev_Urban","IncomeGroup","Edu")

Complete_W10<-Complete_W10 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W10","Family_W10","Demo_W10"))


####--------------------------------------------------------------------------------------------------------------
##Wave 9
##Select the sexism item:
Sexism_W9<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave9.sav")

Sexism_W9<-Sexism_W9 %>%
        dplyr::select(nomem_encr, cv17i110, cv17i111, cv17i113)

Sexism_W9$Wave<-rep(9, times=5592)

##Select the family background item:
Family_W9<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave9.sav")

Family_W9<-Family_W9 %>%
        dplyr::select(nomem_encr, cf16i030, cf16i180)

##Select the demographic items:
Demo_W9<-haven::read_sav("./LISS Data/Background info/Background_Wave9.sav")

Demo_W9<-Demo_W9 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W9<-merge(Sexism_W9, Family_W9, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")
Complete_W9<-merge(Complete_W9, Demo_W9, 
                    by.x = "nomem_encr", 
                    by.y = "nomem_encr")

Complete_W9<-na.omit(Complete_W9)

##Rename the variables:
colnames(Complete_W9)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W9<-Complete_W9 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W9","Family_W9","Demo_W9"))

####--------------------------------------------------------------------------------------------------------------
##Wave 8
##Select the sexism item:
Sexism_W8<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave8.sav")

Sexism_W8<-Sexism_W8 %>%
        dplyr::select(nomem_encr, cv16h110, cv16h111, cv16h113)

Sexism_W8$Wave<-rep(8, times=6092)

##Select the family background item:
Family_W8<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave8.sav")

Family_W8<-Family_W8 %>%
        dplyr::select(nomem_encr, cf15h030, cf15h180)

##Select the demographic items:
Demo_W8<-haven::read_sav("./LISS Data/Background info/Background_Wave8.sav")

Demo_W8<-Demo_W8 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W8<-merge(Sexism_W8, Family_W8, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W8<-merge(Complete_W8, Demo_W8, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W8<-na.omit(Complete_W8)

##Rename the variables:
colnames(Complete_W8)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W8<-Complete_W8 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W8","Family_W8","Demo_W8"))


####--------------------------------------------------------------------------------------------------------------
##Wave 7
##Select the sexism item:
Sexism_W7<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave7.sav")

Sexism_W7<-Sexism_W7 %>%
        dplyr::select(nomem_encr, cv14g110, cv14g111, cv14g113)

Sexism_W7$Wave<-rep(7, times=5690)

##Select the family background item:
Family_W7<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave7.sav")

Family_W7<-Family_W7 %>%
        dplyr::select(nomem_encr, cf14g030, cf14g180)

##Select the demographic items:
Demo_W7<-haven::read_sav("./LISS Data/Background info/Background_Wave7.sav")

Demo_W7<-Demo_W7 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W7<-merge(Sexism_W7, Family_W7, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W7<-merge(Complete_W7, Demo_W7, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W7<-na.omit(Complete_W7)

##Rename the variables:
colnames(Complete_W7)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W7<-Complete_W7 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W7","Family_W7","Demo_W7"))


####--------------------------------------------------------------------------------------------------------------
##Wave 6
##Select the sexism item:
Sexism_W6<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave6.sav")

Sexism_W6<-Sexism_W6 %>%
        dplyr::select(nomem_encr, cv13f110, cv13f111, cv13f113)

Sexism_W6$Wave<-rep(6, times=5732)

##Select the family background item:
Family_W6<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave6.sav")

Family_W6<-Family_W6 %>%
        dplyr::select(nomem_encr, cf13f030, cf13f180)

##Select the demographic items:
Demo_W6<-haven::read_sav("./LISS Data/Background info/Background_Wave6.sav")

Demo_W6<-Demo_W6 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W6<-merge(Sexism_W6, Family_W6, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W6<-merge(Complete_W6, Demo_W6, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W6<-na.omit(Complete_W6)

##Rename the variables:
colnames(Complete_W6)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W6<-Complete_W6 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W6","Family_W6","Demo_W6"))

####--------------------------------------------------------------------------------------------------------------
##Wave 5
##Select the sexism item:
Sexism_W5<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave5.sav")

Sexism_W5<-Sexism_W5 %>%
        dplyr::select(nomem_encr, cv12e110, cv12e111, cv12e113)

Sexism_W5$Wave<-rep(5, times=5932)

##Select the family background item:
Family_W5<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave5.sav")

Family_W5<-Family_W5 %>%
        dplyr::select(nomem_encr, cf12e030, cf12e180)

##Select the demographic items:
Demo_W5<-haven::read_sav("./LISS Data/Background info/Background_Wave5.sav")

Demo_W5<-Demo_W5 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W5<-merge(Sexism_W5, Family_W5, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W5<-merge(Complete_W5, Demo_W5, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W5<-na.omit(Complete_W5)

##Rename the variables:
colnames(Complete_W5)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W5<-Complete_W5 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W5","Family_W5","Demo_W5"))


####--------------------------------------------------------------------------------------------------------------
##Wave 4
##Select the sexism item:
Sexism_W4<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave4.sav")

Sexism_W4<-Sexism_W4 %>%
        dplyr::select(nomem_encr, cv11d110, cv11d111, cv11d113)

Sexism_W4$Wave<-rep(4, times=5394)

##Select the family background item:
Family_W4<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave4.sav")

Family_W4<-Family_W4 %>%
        dplyr::select(nomem_encr, cf11d030, cf11d180)

##Select the demographic items:
Demo_W4<-haven::read_sav("./LISS Data/Background info/Background_Wave4.sav")

Demo_W4<-Demo_W4 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W4<-merge(Sexism_W4, Family_W4, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W4<-merge(Complete_W4, Demo_W4, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W4<-na.omit(Complete_W4)

##Rename the variables:
colnames(Complete_W4)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W4<-Complete_W4 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W4","Family_W4","Demo_W4"))


####--------------------------------------------------------------------------------------------------------------
##Wave 3
##Select the sexism item:
Sexism_W3<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave3.sav")

Sexism_W3<-Sexism_W3 %>%
        dplyr::select(nomem_encr, cv10c110, cv10c111, cv10c113)

Sexism_W3$Wave<-rep(3, times=6386)

##Select the family background item:
Family_W3<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave3.sav")

Family_W3<-Family_W3 %>%
        dplyr::select(nomem_encr, cf10c030, cf10c180)

##Select the demographic items:
Demo_W3<-haven::read_sav("./LISS Data/Background info/Background_Wave3.sav")

Demo_W3<-Demo_W3 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W3<-merge(Sexism_W3, Family_W3, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W3<-merge(Complete_W3, Demo_W3, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W3<-na.omit(Complete_W3)

##Rename the variables:
colnames(Complete_W3)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W3<-Complete_W3 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W3","Family_W3","Demo_W3"))


####--------------------------------------------------------------------------------------------------------------
##Wave 2
##Select the sexism item:
Sexism_W2<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave2.sav")

Sexism_W2<-Sexism_W2 %>%
        dplyr::select(nomem_encr, cv09b110, cv09b111, cv09b113)

Sexism_W2$Wave<-rep(2, times=6037)

##Select the family background item:
Family_W2<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave2.sav")

Family_W2<-Family_W2 %>%
        dplyr::select(nomem_encr, cf09b030, cf09b180)

##Select the demographic items:
Demo_W2<-haven::read_sav("./LISS Data/Background info/Background_Wave2.sav")

Demo_W2<-Demo_W2 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W2<-merge(Sexism_W2, Family_W2, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W2<-merge(Complete_W2, Demo_W2, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W2<-na.omit(Complete_W2)

##Rename the variables:
colnames(Complete_W2)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W2<-Complete_W2 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W2","Family_W2","Demo_W2"))

####--------------------------------------------------------------------------------------------------------------
##Wave 1
##Select the sexism item:
Sexism_W1<-haven::read_sav("./LISS Data/Politics and values/Politics_Values_Wave1.sav")

Sexism_W1<-Sexism_W1 %>%
        dplyr::select(nomem_encr, cv08a110, cv08a111, cv08a113)

Sexism_W1$Wave<-rep(1, times=6811)

##Select the family background item:
Family_W1<-haven::read_sav("./LISS Data/Family and Household/Family_Household_Wave1.sav")

Family_W1<-Family_W1 %>%
        dplyr::select(nomem_encr, cf08a030, cf08a180)

##Select the demographic items:
Demo_W1<-haven::read_sav("./LISS Data/Background info/Background_Wave1.sav")

Demo_W1<-Demo_W1 %>%
        dplyr::select(nomem_encr, partner, woonvorm, lftdcat, geslacht, sted, nettocat, oplcat)

##Merge three data:
Complete_W1<-merge(Sexism_W1, Family_W1, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")
Complete_W1<-merge(Complete_W1, Demo_W1, 
                   by.x = "nomem_encr", 
                   by.y = "nomem_encr")

Complete_W1<-na.omit(Complete_W1)

##Rename the variables:
colnames(Complete_W1)<-c("IndividualID", "sexism1", "sexism2", "sexism3",
                         "wave", "Married", "SatisfactionRelationship","partner",
                         "domesticSituation", "AgeGroup","Gender", 
                         "Rev_Urban","IncomeGroup","Edu")

Complete_W1<-Complete_W1 %>%
        mutate_at(c("IndividualID", "sexism1", "sexism2", "sexism3",
                    "wave", "Married", "SatisfactionRelationship","partner",
                    "domesticSituation", "AgeGroup","Gender", 
                    "Rev_Urban","IncomeGroup","Edu"), as.numeric)

##Leave only the complete data in the environment for merging later:
rm(list = c("Sexism_W1","Family_W1","Demo_W1"))


####--------------------------------------------------------------------------------------------------------
##Put all the data together in long format:
Complete_15Waves<-rbind(Complete_W1, Complete_W2, Complete_W3, Complete_W4,
                        Complete_W5, Complete_W6, Complete_W7, Complete_W8, 
                        Complete_W9, Complete_W10, Complete_W11, Complete_W12,
                        Complete_W13, Complete_W14, Complete_W15)

rm(list=c("Complete_W1","Complete_W2","Complete_W3","Complete_W4","Complete_W5",
          "Complete_W6","Complete_W7","Complete_W8","Complete_W9","Complete_W10",
          "Complete_W11","Complete_W12","Complete_W13","Complete_W14","Complete_W15"))

##Find out who has participated for more than two rounds
Participation<-Complete_15Waves %>%
        group_by(IndividualID) %>%
        summarise(count=n()) %>%
        arrange(-count)


##We will keep those who participated in at least two rounds:
Participation<-Participation %>%
        filter(count>=2)

Complete_15Waves<-merge(Complete_15Waves, Participation,
                                 by.x = "IndividualID",
                                 by.y="IndividualID")
rm(list=c("Participation"))

Complete_15Waves<-Complete_15Waves %>%
        dplyr::select(-count) %>%
        arrange(IndividualID, wave)


####--------------------------------------------------------------------------------------------------------
##Operationalization of the variable
##Dependent variable: prejudice against working mother:
#CFA of the 3 items:
Prej_CFA_model<-'
Prej=~sexism1+sexism2+sexism3
'
Prej_CFA<-cfa(model = Prej_CFA_model, 
              data = Complete_15Waves)
summary(Prej_CFA, standardized=TRUE, fit.measures=TRUE)
#
#factor loading: 0.828, 0.836 and 0.607
#
Complete_15Waves<-Complete_15Waves %>%
        mutate(PrejWorkMum=(sexism1+sexism2+sexism3)/3)
#Standardization:
Complete_15Waves$PrejWorkMumStand<-scale(Complete_15Waves$PrejWorkMum)

##Independent variables:
#
#Married:
#Are you married to this partner? (1 yes; 2 no)
Complete_15Waves$Married<-as.numeric(sub(2,0,Complete_15Waves$Married))
#
#SatisfactionRelationship
#How satisfied are you with your current relationship? (0: entirely dissatisfied-10: entirely satisfied)
#Standardization: 
Complete_15Waves$SatisfactionRelationship<-scale(Complete_15Waves$SatisfactionRelationship)
#
#partner:
#The household head lives together with a partner (wedded or unwedded): 0:no 1:yes
#No need to change
#
#domesticSituation
#Domestic situation: 1 single; 2 (Un)married co-habitation, without child(ren);
#3 (Un)married co-habitation, with child(ren), 4 Single, with child(ren); 5 other
#1 and 2 are without children, coded 0; 3 and 4 are with children, coded 1; 5 will be missing
Complete_15Waves<-Complete_15Waves %>%
        mutate(WithChildren=case_when(
                domesticSituation %in% c(1,2) ~ 0,
                domesticSituation %in% c(3,4) ~ 1,
                domesticSituation == 5 ~ NA
        ))
Complete_15Waves<-na.omit(Complete_15Waves)
#
#AgeGroup
#Age in CBS (Statistics Netherlands) categories: 1: 14 years and younger;
#2: 15 - 24 years; 3: 25 - 34 years; 4: 35 - 44 years; 5: 45 - 54 years;
#6: 55 - 64 years; 7: 65 years and older
Complete_15Waves<-Complete_15Waves %>%
        mutate(AgeGroupCat=case_when(
                AgeGroup == 1 ~ "14yo and below",
                AgeGroup == 2 ~ "15-24yo",
                AgeGroup == 3 ~ "25-34yo",
                AgeGroup == 4 ~ "35-44yo",
                AgeGroup == 5 ~ "45-54yo",
                AgeGroup == 6 ~ "55-64yo",
                AgeGroup == 7 ~ "65Above"
        ))
Complete_15Waves$AgeGroupCat<-factor(Complete_15Waves$AgeGroupCat,
                                     levels = c("15-24yo","25-34yo","35-44yo",
                                                "45-54yo", "55-64yo", "65Above"))
#
#Gender:
#1 Male; 2 Female;
Complete_15Waves$male<-as.numeric(sub(2, 0, Complete_15Waves$Gender))
#
#Urbanization
#Rev_Urban: 1: Extremely urban - 5: Not urban
Complete_15Waves<-Complete_15Waves %>% 
        mutate(urbanization=(-1)*Rev_Urban+6)
#
##Income
#Eurostat: minimum wage 1635 EUR
#Average net income: 2000-2600 --> lower than 1500 is low, higher than 3000 is high
Complete_15Waves<-Complete_15Waves %>%
        mutate(NewIncomeGroup=case_when(
                IncomeGroup <=3 ~ "low",
                IncomeGroup >=4 & IncomeGroup<=6 ~ "middle",
                IncomeGroup >= 7 ~ "high"
        ))
Complete_15Waves$NewIncomeGroup<-factor(Complete_15Waves$NewIncomeGroup, levels = c("low","middle","high"))
#
#Edu:(sourced from City of Amsterdam)
##The minimum education requirement of all children is 1. primary school and 2. vmbo --> "low"
##Selectively secondary education is not compulsory but one step higher: 3 havo/vmo and 4 mbo --> "middle"
##The most advanced level is 5 hbo (university of applied sciences) and 6 wo (research university) --> "high"
Complete_15Waves<-Complete_15Waves %>%
        mutate(EduGroup=case_when(
                Edu <3 ~ "lowEdu",
                Edu >=3 & Edu<=4 ~ "MidEdu",
                Edu > 4 ~ "HighEdu"
        ))
Complete_15Waves$EduGroup<-factor(Complete_15Waves$EduGroup, levels = c("lowEdu", "MidEdu","HighEdu"))

rm(list=c("Prej_CFA","Prej_CFA_model"))
