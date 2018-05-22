#_________________________________________________________________________________________________
#_________________________PREP____________________________________________________________________
#_________________________________________________________________________________________________

data = masterfile_nans

#---factors

data$patient = as.factor(data$patient)
data$experiment = as.factor(data$experiment)
data$mode = as.factor(data$mode)

# declaring variables
data$CCD = 60/data$HR
data$FCW_peak_corrected = data$FCW_peak*data$CCD*data$CCD

data$backward_wave = data$BCW_peak + data$BEW_peak


#---quilty

quality_level = 0.5

data = subset(data, data$QA > quality_level)

data = subset(data, data$`FCW (integral)` > 0)
data = subset(data, data$FCW_peak > 10)
data = subset(data, data$WRI >= 0)
data = subset(data, data$WRI < 1)

#---subsets

data00 = subset(data, experiment == 'flow00')
data25 = subset(data, experiment == 'flow25')
data66 = subset(data, experiment == 'flow66')

data_viagra = subset(data, mode == 'viagra')
data_base = subset(data, mode == 'base')

data00base = subset(data00, mode == 'base')
data00viagra = subset(data00, mode == 'viagra')
data25base = subset(data25, mode == 'base')
data25viagra = subset(data25, mode == 'viagra')
data66base = subset(data66, mode == 'base')
data66viagra = subset(data66, mode == 'viagra')


#_________________________________________________________________________________________________
#_________________________anova___________________________________________________________________
#_________________________________________________________________________________________________
mode = data$mode
experiment = data$experiment
HR = data$HR

variable_list = list(data$`FCW (integral)`,
                     data$`BCW(integral)`,
                     data$`FEW(integral)`,
                     data$`BEW(integral)`,
                     data$WRI,
                     data$FCW_peak,
                     data$FCW_peak_time,
                     data$FCW_FWHM,
                     data$BCW_peak,
                     data$BCW_peak_time,
                     data$BCW_FWHM,
                     data$FEW_peak,
                     data$FEW_peak_time,
                     data$FEW_FWHM,
                     data$BEW_peak,
                     data$BEW_peak_time,
                     data$BEW_FWHM,
                     data$NFCW_peak,
                     data$NFCW_time,
                     data$NBW_peak,
                     data$NBW_time,
                     data$NFEW_peak,
                     data$NFEW_time,
                     data$HR,
                     data$FCW_peak_corrected)

i = 27  #starting name of variable
par(mar=c(1,1,1,1))
par(mfrow = c(5,5))

for(variable in variable_list){
current_name = names(data)[i]  
print(current_name)
i = i+1
model = lm(variable ~ experiment + mode + experiment*mode)
anova = aov(model)
print(summary(anova))
boxplot(variable ~ experiment + mode,
        main = current_name)
print("----------------------------------")
}



#_________________________________________________________________________________________________
#____________________INTERACTION_anova____________________________________________________________
#_________________________________________________________________________________________________

#add variables of interest into list for each futher analysis

#-------------------------------------
#--------------base
#-------------------------------------
interaction_list = list(data_base$FCW..integral.,
                        data_base$BCW.integral.)

base_experiment = data_base$experiment

j = 1
for(variable in interaction_list){
  
  print("data_base")
  print(j)
  j = j+1
  model = lm(variable ~ base_experiment)
  print(summary(model))
  print(pairwise.t.test(variable,base_experiment, p.adjust.method = "bonferroni"))
  print("-------------------------------------------------------------------------")
}

#-------------------------------------
#--------------viagra
#-------------------------------------
interaction_list = list()                        

viagra_experiment = data_viagra$experiment

j = 1
for(variable in interaction_list){
  
  print("data_viagra")
  print(j)
  j = j+1
  model = lm(variable ~ viagra_experiment)
  print(summary(model))
  print(pairwise.t.test(variable,viagra_experiment, p.adjust.method = "bonferroni"))
  print("-------------------------------------------------------------------------")
}


#-------------------------------------
#--------------flow00 
#-------------------------------------
interaction_list = list(data00$`FCW (integral)`,
                        data00$`BCW(integral)`,
                        data00$`FEW(integral)`,
                        data00$`BEW(integral)`,
                        data00$WRI,
                        data00$FCW_peak,
                        data00$FCW_peak_time,
                        data00$FCW_FWHM,
                        data00$BCW_peak,
                        data00$BCW_peak_time,
                        data00$BCW_FWHM,
                        data00$FEW_peak,
                        data00$FEW_peak_time,
                        data00$FEW_FWHM,
                        data00$BEW_peak,
                        data00$BEW_peak_time,
                        data00$BEW_FWHM,
                        data00$NFCW_peak,
                        data00$NFCW_time,
                        data00$NBW_peak,
                        data00$NBW_time,
                        data00$NFEW_peak,
                        data00$NFEW_time,
                        data00$HR,
                        data00$FCW_peak_corrected,
                        data00$backward_wave)

mode00 = data00$mode

j = 1
for(variable in interaction_list){
  
  print("data_00")
  print(j)
  j = j+1
  #model = lm(variable ~ mode00)
  #print(summary(model))
  print(pairwise.t.test(variable,mode00, p.adjust.method = "bonferroni"))
  print("-------------------------------------------------------------------------")
}


#-------------------------------------
#--------------flow25 
#-------------------------------------
interaction_list = list(data25$`FCW (integral)`,
                        data25$`BCW(integral)`,
                        data25$`FEW(integral)`,
                        data25$`BEW(integral)`,
                        data25$WRI,
                        data25$FCW_peak,
                        data25$FCW_peak_time,
                        data25$FCW_FWHM,
                        data25$BCW_peak,
                        data25$BCW_peak_time,
                        data25$BCW_FWHM,
                        data25$FEW_peak,
                        data25$FEW_peak_time,
                        data25$FEW_FWHM,
                        data25$BEW_peak,
                        data25$BEW_peak_time,
                        data25$BEW_FWHM,
                        data25$NFCW_peak,
                        data25$NFCW_time,
                        data25$NBW_peak,
                        data25$NBW_time,
                        data25$NFEW_peak,
                        data25$NFEW_time,
                        data25$HR,
                        data25$FCW_peak_corrected,
                        data25$backward_wave)

mode25 = data25$mode

j = 1
for(variable in interaction_list){
  
  print("data_25")
  print(j)
  j = j+1
  #model = lm(variable ~ mode25)
  #print(summary(model))
  print(pairwise.t.test(variable,mode25, p.adjust.method = "bonferroni"))
  print("-------------------------------------------------------------------------")
}

#-------------------------------------
#--------------flow66 
#_____________________________________

interaction_list = list(data66$`FCW (integral)`,
                        data66$`BCW(integral)`,
                        data66$`FEW(integral)`,
                        data66$`BEW(integral)`,
                        data66$WRI,
                        data66$FCW_peak,
                        data66$FCW_peak_time,
                        data66$FCW_FWHM,
                        data66$BCW_peak,
                        data66$BCW_peak_time,
                        data66$BCW_FWHM,
                        data66$FEW_peak,
                        data66$FEW_peak_time,
                        data66$FEW_FWHM,
                        data66$BEW_peak,
                        data66$BEW_peak_time,
                        data66$BEW_FWHM,
                        data66$NFCW_peak,
                        data66$NFCW_time,
                        data66$NBW_peak,
                        data66$NBW_time,
                        data66$NFEW_peak,
                        data66$NFEW_time,
                        data66$HR,
                        data66$FCW_peak_corrected,
                        data66$backward_wave)

mode66 = data66$mode

j = 1
for(variable in interaction_list){
  
  print("data_66")
  print(j)
  j = j+1
  #model = lm(variable ~ mode66)
  #print(summary(model))
  print(pairwise.t.test(variable,mode66, p.adjust.method = "bonferroni"))
  print("-------------------------------------------------------------------------")
}

dev.off()
boxplot(data$WRI)

boxplot(data$`BCW(integral)`)
boxplot(data$`FCW (integral)`)
plot(data$`FCW (integral)`)
boxplot(data$FCW_peak, ylim = c(0,200))
plot(data$FCW_peak)


mean(data00base$`FCW (integral)`)
mean(data00base$`BCW(integral)`)

mean(data00base$FCW_peak)
median(data00base$BCW_peak)
plot(data00base$BCW_peak)
