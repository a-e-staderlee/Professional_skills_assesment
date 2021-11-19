#Professional Skills Course
#Quantative Skills Assessment

#Load and explore data----
inga_traits<-read.csv("Uni/Year 4/Professional Skills/QuantSk/Assessments/Inga_traits.csv")
View(inga_traits)

#Libraries----
library(tidyverse) #for tidying
library(praise) #for moral support


##### Excercise 1: Histograms ####
#1.a----
#Please make and present a histogram 
#for leaf area of these species. 
#What can you say about this distribution in statistical terms? 
#Does leaf size appear to be normally distributed? 

#Histogram of Leaf Area of the species

(hist1a<-ggplot(inga_traits, aes(x=Leaf_Area))+
  geom_histogram(bins=150)+
   xlab(bquote(Leaf_Area(cm^2)))+
   ylab("Frequency"))


#1.b----
#Try log-transforming leaf area and make and present 
#a histogram of log-transformed leaf area

inga_traits$log_la<-log(inga_traits$Leaf_Area)

(hist1b<-ggplot(inga_traits, aes(x=log_la))+
  geom_histogram(bins=150)+
  labs(caption="Histogram of Logged Leaf Area")+
  xlab("Logged Leaf Area")+
       ylab("Frequency"))
#1.c----
#Now, in simple terms, how would you describe the distribution 
#of leaf sizes across trees in this region to a non-scientist? 




#### Excercise 2: Boxplots and ANOVA ####
#2.a1----
#Now let’s see how species in different habitats might differ 
#in leaf chemical composition. 
#Make and present a boxplot of leaf phosphorous concentration 
#versus habitat in which a species is found. 

(box_2a1<-ggplot(inga_traits, aes(x=Habitat, y=P_Leaf))+
   geom_boxplot()+
   labs(caption="Leaf Phosphorous Content By Habitat")+
   xlab("Habitat")+
   ylab("Phosphorous Concentration (mg/g)"))
#2.a2----
#Now statistically test if species found in different habitats 
#have significantly different phosphorous concentrations in 
#their leaves. 
#Report the F Statistic, p-value and degrees of freedom 
#for your test. 
#Then, tell me what these two measures mean in general and
#what the specific values mean in the context of this analysis.

anova_phos<-lm(P_Leaf~Habitat, data=inga_traits)

anova(anova_phos)
#F value= 8.5979
#p value= 0.001291
#degrees of freedom= habitat:2, residuals:27

#2.b----
#Try and conduct an evaluation of your model. 
#I do not need to see any model validation figures, 
#but I do want some written explanation of why 
#you think your model is good (or not). 
#Have you likely violated any of the assumptions of ANOVA? 
#If so, which ones? 


#2.c----
#How might you improve your model? 
#Try doing so and report the revised F Statistic and p-value. 

#2.d----
#Now, provide an explanation of your analysis, 
#the results and what they mean, 
#in non-technical terms that would be accessible 
#to a relative or someone you meet in a pub 
#(or elevator if you don’t frequent pubs). 
#Your explanation should cover why species in different habitats 
#might (or might not) have different amounts of P in their leaves. 




#### Excercise 3: Multiple Explanetory Values ####
#3.a----
#Make a plot of leaf phosphorous concentrations versus
#leaf carbon concentrations (with leaf phosphorous on the y-axis). 
#Use different symbols for species in each habitat category 
#(floodplain, upland and generalist), 
#and place a best fit trendline (linear) 
#on the plot for each group of species. 
#Let me know in the figure legend (the text at the bottom of the figure) 
#which symbols and lines belong to each group. 

(scatter_3a<-ggplot(inga_traits, aes(x=C_Leaf, y=P_Leaf, shape=Habitat, colour=Habitat))+
  geom_point()+
  geom_smooth(method="lm")+
  guides(shape = guide_legend(title = "Habitat")))
