

geef<-read.csv("gean.csv")
str(geef)
install.packages("geepack")
library(geepack)
gemod1<-geeglm(Hospital.Aniety ~ group + factor(t), family= gaussian, data=geef,id= Patient.ID,
               corstr="independence")
gemod2<-geeglm(Hospital.Aniety ~ group + factor(t), family= gaussian, data=geef,id= Patient.ID,
               corstr="exchangeable")
gemod3<-geeglm(Hospital.Aniety ~ group + factor(t)+(group*factor(t)), family= gaussian, data=geef,id= Patient.ID,
               corstr="exchangeable")
gemod4<-gemod3<-geeglm(Hospital.Aniety ~ group + factor(t)+(group*factor(t)), family= gaussian, data=geef,id= Patient.ID,
                       corstr="independence")
install.packages("MESS")
library(MESS)
QIC(gemod1)
QIC(gemod2)
QIC(gemod3)
QIC(gemod4)  # qic of model4 is the least so it is selected for further statistical analysis 
summary(gemod4) #p_value indicates to strong association between the variables in this model.
#the odds of hospital.anexiety per increase in time Unit is  4.167
confint(gemod4)
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-mult*Std.err,
                      upr=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

confint.geeglm(gemod4) 

library(ggplot2)

plot(gemod4)
gemod4p<-gemod4$plot
print(gemod4p+ ggplot2::theme(legend.position = 'top'))
summary(gemod4, printAutoCorPars = TRUE)
pred <- predict(gemod4, newdata = geef)
pred
