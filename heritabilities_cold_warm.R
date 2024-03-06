#### Calculate heritabilities for plants from colder/warmer areas in uheated/heated treatment ###

# Distribution of mid-parental temperatures and cateogrization

nrow(data_exp%>%filter(!is.na(temp_midP)&!is.na(FFD))) # Sample size = 844
hist(data_exp$temp_midP)
summary(data_exp$temp_midP)
nrow(data_exp%>%filter(temp_midP>=14.45&temp_midP>=24))

data_exp_thirds<-data_exp%>%
  mutate(temp_midP_cat=case_when(
    between(ntile(temp_midP, 3), 1, 1) ~ "colder",
    between(ntile(temp_midP, 3), 2, 2) ~ "middle",
    between(ntile(temp_midP, 3), 3, 3) ~ "warmer"
  ))

nrow(data_exp_thirds%>%filter(temp_midP_cat=="colder"&!is.na(FFD)))
nrow(data_exp_thirds%>%filter(temp_midP_cat=="warmer"&!is.na(FFD)))
nrow(data_exp_thirds%>%filter(temp_midP_cat=="middle"&!is.na(FFD)))
nrow(data_exp_thirds%>%filter(temp_midP_cat=="colder"|temp_midP_cat=="warmer")%>%
       filter(!is.na(FFD))) # Sample size = 563

# Models mean responses (pred 1)

model1a_veg_uh_cold<-lmer(mean_sh_growth~1+(1|sire)+(1|dam),
                     #(1|sire:dam) # Removed to avoid singular fit
                     subset(data_exp_thirds,
                            treat=="unheated"&temp_midP_cat=="colder"))
model1a_veg_h_cold<-lmer(mean_sh_growth~1+(1|sire)+(1|dam)+(1|sire:dam),
                    subset(data_exp_thirds,
                           treat=="heatmat"&temp_midP_cat=="colder"))
model1a_veg_uh_warm<-lmer(mean_sh_growth~1+(1|sire)+(1|dam),
                          #(1|sire:dam) # Removed to avoid singular fit
                          subset(data_exp_thirds,
                                 treat=="unheated"&temp_midP_cat=="warmer"))
model1a_veg_h_warm<-lmer(mean_sh_growth~1+
                           #(1|sire)+(1|dam)+
                           (1|sire:dam),
                         subset(data_exp_thirds,
                                treat=="heatmat"&temp_midP_cat=="warmer"))
model1a_FFD_uh_cold<-lmer(FFD~1+(1|sire)+(1|dam),
                          #(1|sire:dam)
                     subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="colder"))
model1a_FFD_h_cold<-lmer(FFD~1+
                           #(1|sire)
                           (1|dam)+(1|sire:dam),
                    subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="colder"))
model1a_FFD_uh_warm<-lmer(FFD~1+(1|sire)+(1|dam)+(1|sire:dam),
                          subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="warmer"))
model1a_FFD_h_warm<-lmer(FFD~1+
                           #(1|sire)+(1|dam)
                           (1|sire:dam),
                         subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="warmer"))
model1a_LFD_uh_cold<-lmer(LFD~1+
                            #(1|sire)
                            (1|dam)+(1|sire:dam),
                     subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="colder"))
model1a_LFD_h_cold<-lmer(LFD~1+
                           #(1|sire)
                           (1|dam),
                           #(1|sire:dam)
                    subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="colder"))
model1a_LFD_uh_warm<-lmer(LFD~1+(1|sire)+(1|dam),
                          #(1|sire:dam)
                          subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="warmer"))
model1a_LFD_h_warm<-lmer(LFD~1+(1|sire)+(1|dam),
                         #(1|sire:dam)
                         subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="warmer"))
model1a_MFD_uh_cold<-lmer(MFD~1+(1|sire)+(1|dam),
                          #(1|sire:dam)
                          subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="colder"))
model1a_MFD_h_cold<-lmer(MFD~1+(1|sire)+(1|dam),
                         #(1|sire:dam)
                         subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="colder"))
model1a_MFD_uh_warm<-lmer(MFD~1+(1|sire)+(1|dam)+(1|sire:dam),
                     subset(data_exp_thirds,treat=="unheated"&temp_midP_cat=="warmer"))
model1a_MFD_h_warm<-lmer(MFD~1+(1|sire)+
                           #(1|dam)
                           (1|sire:dam),
                    subset(data_exp_thirds,treat=="heatmat"&temp_midP_cat=="warmer"))
summary(model1a_veg_uh_cold)
summary(model1a_veg_h_cold)
summary(model1a_veg_uh_warm)
summary(model1a_veg_h_warm)
summary(model1a_FFD_uh_cold)
summary(model1a_FFD_h_cold)
summary(model1a_FFD_uh_warm)
summary(model1a_FFD_h_warm)
summary(model1a_LFD_uh_cold)
summary(model1a_LFD_h_cold)
summary(model1a_LFD_uh_warm)
summary(model1a_LFD_h_warm)
summary(model1a_MFD_uh_cold)
summary(model1a_MFD_h_cold)
summary(model1a_MFD_uh_warm)
summary(model1a_MFD_h_warm)

# Proportions of variance mean responses

Variance_veg_uh_cold<-as.data.frame(VarCorr(model1a_veg_uh_cold))[,c(1,4)]
Variance_FFD_uh_cold<-as.data.frame(VarCorr(model1a_FFD_uh_cold))[,c(1,4)]
Variance_LFD_uh_cold<-as.data.frame(VarCorr(model1a_LFD_uh_cold))[,c(1,4)]
Variance_MFD_uh_cold<-as.data.frame(VarCorr(model1a_MFD_uh_cold))[,c(1,4)]
Variance_veg_h_cold<-as.data.frame(VarCorr(model1a_veg_h_cold))[,c(1,4)]
Variance_FFD_h_cold<-as.data.frame(VarCorr(model1a_FFD_h_cold))[,c(1,4)]
Variance_LFD_h_cold<-as.data.frame(VarCorr(model1a_LFD_h_cold))[,c(1,4)]
Variance_MFD_h_cold<-as.data.frame(VarCorr(model1a_MFD_h_cold))[,c(1,4)]
Variance_veg_uh_warm<-as.data.frame(VarCorr(model1a_veg_uh_warm))[,c(1,4)]
Variance_FFD_uh_warm<-as.data.frame(VarCorr(model1a_FFD_uh_warm))[,c(1,4)]
Variance_LFD_uh_warm<-as.data.frame(VarCorr(model1a_LFD_uh_warm))[,c(1,4)]
Variance_MFD_uh_warm<-as.data.frame(VarCorr(model1a_MFD_uh_warm))[,c(1,4)]
Variance_veg_h_warm<-as.data.frame(VarCorr(model1a_veg_h_warm))[,c(1,4)]
Variance_FFD_h_warm<-as.data.frame(VarCorr(model1a_FFD_h_warm))[,c(1,4)]
Variance_LFD_h_warm<-as.data.frame(VarCorr(model1a_LFD_h_warm))[,c(1,4)]
Variance_MFD_h_warm<-as.data.frame(VarCorr(model1a_MFD_h_warm))[,c(1,4)]

PropVar_veg_uh_cold <- Variance_veg_uh_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="unheated",origin="cold")
PropVar_FFD_uh_cold <- Variance_FFD_uh_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="unheated",origin="cold")
PropVar_LFD_uh_cold <- Variance_LFD_uh_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="unheated",origin="cold")
PropVar_MFD_uh_cold <- Variance_MFD_uh_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="unheated",origin="cold")
PropVar_veg_h_cold <- Variance_veg_h_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="heated",origin="cold")
PropVar_FFD_h_cold <- Variance_FFD_h_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="heated",origin="cold")
PropVar_LFD_h_cold <- Variance_LFD_h_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="heated",origin="cold")
PropVar_MFD_h_cold <- Variance_MFD_h_cold%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="heated",origin="cold")
PropVar_veg_uh_warm <- Variance_veg_uh_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="unheated",origin="warm")
PropVar_FFD_uh_warm <- Variance_FFD_uh_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="unheated",origin="warm")
PropVar_LFD_uh_warm <- Variance_LFD_uh_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="unheated",origin="warm")
PropVar_MFD_uh_warm <- Variance_MFD_uh_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="unheated",origin="warm")
PropVar_veg_h_warm <- Variance_veg_h_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="heated",origin="warm")
PropVar_FFD_h_warm <- Variance_FFD_h_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="heated",origin="warm")
PropVar_LFD_h_warm <- Variance_LFD_h_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="heated",origin="warm")
PropVar_MFD_h_warm <- Variance_MFD_h_warm%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="heated",origin="warm")

Props_var_thirds<-rbind(PropVar_veg_uh_cold,PropVar_FFD_uh_cold,
                        PropVar_LFD_uh_cold,PropVar_MFD_uh_cold,
                        PropVar_veg_h_cold,PropVar_FFD_h_cold,
                        PropVar_LFD_h_cold,PropVar_MFD_h_cold,
                        PropVar_veg_uh_warm,PropVar_FFD_uh_warm,
                        PropVar_LFD_uh_warm,PropVar_MFD_uh_warm,
                        PropVar_veg_h_warm,PropVar_FFD_h_warm,
                        PropVar_LFD_h_warm,PropVar_MFD_h_warm)

ggplot(Props_var_thirds,aes(x=variable,y=propvar,fill=grp))+
  geom_col()+facet_wrap(~treat+origin,labeller="label_both")+
  ggtitle("Mean responses: Proportions of variance")

# Heritability and maternal effects mean responses

her_veg_uh_cold<-as.numeric(4*subset(PropVar_veg_uh_cold,grp=="sire")[3])
her_veg_h_cold<-as.numeric(4*subset(PropVar_veg_h_cold,grp=="sire")[3])
her_FFD_uh_cold<-as.numeric(4*subset(PropVar_FFD_uh_cold,grp=="sire")[3])
her_FFD_h_cold<-0
her_LFD_uh_cold<-0
her_LFD_h_cold<-0
her_MFD_uh_cold<-as.numeric(4*subset(PropVar_MFD_uh_cold,grp=="sire")[3])
her_MFD_h_cold<-as.numeric(4*subset(PropVar_MFD_h_cold,grp=="sire")[3])
her_veg_uh_warm<-as.numeric(4*subset(PropVar_veg_uh_warm,grp=="sire")[3])
her_veg_h_warm<-0
her_FFD_uh_warm<-as.numeric(4*subset(PropVar_FFD_uh_warm,grp=="sire")[3])
her_FFD_h_warm<-0
her_LFD_uh_warm<-as.numeric(4*subset(PropVar_LFD_uh_warm,grp=="sire")[3])
her_LFD_h_warm<-as.numeric(4*subset(PropVar_LFD_h_warm,grp=="sire")[3])
her_MFD_uh_warm<-as.numeric(4*subset(PropVar_MFD_uh_warm,grp=="sire")[3])
her_MFD_h_warm<-as.numeric(4*subset(PropVar_MFD_h_warm,grp=="sire")[3])

her_thirds<-data.frame(value=rbind(her_veg_uh_cold,her_veg_h_cold,
                                   her_FFD_uh_cold,her_FFD_h_cold,
                                   her_LFD_uh_cold,her_LFD_h_cold,
                                   her_MFD_uh_cold,her_MFD_h_cold,
                                   her_veg_uh_warm,her_veg_h_warm,
                                   her_FFD_uh_warm,her_FFD_h_warm,
                                   her_LFD_uh_warm,her_LFD_h_warm,
                                   her_MFD_uh_warm,her_MFD_h_warm))%>%
  rownames_to_column()%>%
  mutate(variable=c("veg","veg","FFD","FFD","LFD","LFD","MFD","MFD",
                    "veg","veg","FFD","FFD","LFD","LFD","MFD","MFD"),
         treat=c("unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated",
                 "unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated"),
         origin=c("cold","cold","cold","cold","cold","cold","cold","cold",
                  "warm","warm","warm","warm","warm","warm","warm","warm"),
         effect="Heritability")

mat_veg_uh_cold<-(Variance_veg_uh_cold[2,2]-Variance_veg_uh_cold[1,2])/
  as.numeric(Variance_veg_uh_cold%>%summarise(sum(vcov)))
mat_veg_h_cold<-(Variance_veg_h_cold[2,2]-Variance_veg_h_cold[3,2])/
  as.numeric(Variance_veg_h_cold%>%summarise(sum(vcov)))
mat_FFD_uh_cold<-(Variance_FFD_uh_cold[1,2]-Variance_FFD_uh_cold[2,2])/
  as.numeric(Variance_FFD_uh_cold%>%summarise(sum(vcov)))
mat_FFD_h_cold<-(Variance_FFD_h_cold[2,2]-0)/
  as.numeric(Variance_FFD_h_cold%>%summarise(sum(vcov)))
mat_LFD_uh_cold<-(Variance_LFD_uh_cold[2,2]-0)/
  as.numeric(Variance_LFD_uh_cold%>%summarise(sum(vcov)))
mat_LFD_h_cold<-(Variance_LFD_h_cold[1,2]-0)/
  as.numeric(Variance_LFD_h_cold%>%summarise(sum(vcov)))
mat_MFD_uh_cold<-(Variance_MFD_uh_cold[1,2]-Variance_MFD_uh_cold[2,2])/
  as.numeric(Variance_MFD_uh_cold%>%summarise(sum(vcov)))
mat_MFD_h_cold<-(Variance_MFD_h_cold[1,2]-Variance_MFD_h_cold[2,2])/
  as.numeric(Variance_MFD_h_cold%>%summarise(sum(vcov)))
mat_veg_uh_warm<-(Variance_veg_uh_warm[1,2]-Variance_veg_uh_warm[2,2])/
  as.numeric(Variance_veg_uh_warm%>%summarise(sum(vcov)))
mat_veg_h_warm<-(0-0)/
  as.numeric(Variance_veg_h_warm%>%summarise(sum(vcov)))
mat_FFD_uh_warm<-(Variance_FFD_uh_warm[2,2]-Variance_FFD_uh_warm[3,2])/
  as.numeric(Variance_FFD_uh_warm%>%summarise(sum(vcov)))
mat_FFD_h_warm<-(0-0)/
  as.numeric(Variance_FFD_h_warm%>%summarise(sum(vcov)))
mat_LFD_uh_warm<-(Variance_LFD_uh_warm[1,2]-Variance_LFD_uh_warm[2,2])/
  as.numeric(Variance_LFD_uh_warm%>%summarise(sum(vcov)))
mat_LFD_h_warm<-(Variance_LFD_h_warm[1,2]-Variance_LFD_h_warm[2,2])/
  as.numeric(Variance_LFD_h_warm%>%summarise(sum(vcov)))
mat_MFD_uh_warm<-(Variance_MFD_uh_warm[2,2]-Variance_MFD_uh_warm[3,2])/
  as.numeric(Variance_MFD_uh_warm%>%summarise(sum(vcov)))
mat_MFD_h_warm<-(0-Variance_MFD_h_warm[2,2])/
  as.numeric(Variance_MFD_h_warm%>%summarise(sum(vcov)))

mat_thirds<-data.frame(value=rbind(mat_veg_uh_cold,mat_veg_h_cold,
                                   mat_FFD_uh_cold,mat_FFD_h_cold,
                                   mat_LFD_uh_cold,mat_LFD_h_cold,
                                   mat_MFD_uh_cold,mat_MFD_h_cold,
                                   mat_veg_uh_warm,mat_veg_h_warm,
                                   mat_FFD_uh_warm,mat_FFD_h_warm,
                                   mat_LFD_uh_warm,mat_LFD_h_warm,
                                   mat_MFD_uh_warm,mat_MFD_h_warm))%>%
  rownames_to_column()%>%
  mutate(variable=c("veg","veg","FFD","FFD","LFD","LFD","MFD","MFD",
                    "veg","veg","FFD","FFD","LFD","LFD","MFD","MFD"),
         treat=c("unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated",
                 "unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated"),
         origin=c("cold","cold","cold","cold","cold","cold","cold","cold",
                  "warm","warm","warm","warm","warm","warm","warm","warm"),
         effect="Maternal effects")

her_mat_thirds<-rbind(her_thirds,mat_thirds)
her_mat_thirds

ggplot(her_mat_thirds,aes(x=variable,y=value,fill=effect))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~treat+origin,labeller="label_both")+
  ggtitle("Mean responses: Heritability and maternal effects")

# Models plasticity (pred 2)

model1b_veg_cold<-lmer(mean_sh_growth~treat+
                         (1|sire)+(1|dam)+
                         (1|treat:sire)+
                         #(1|treat:dam)+
                         (1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="colder"))
model1b_FFD_cold<-lmer(FFD~treat+
                         (1|sire)+(1|dam)+
                         #(1|treat:sire)+
                         (1|treat:dam)+
                         (1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="colder"))
model1b_LFD_cold<-lmer(LFD~treat+
                         #(1|sire)+
                         (1|dam)+
                         #(1|treat:sire)+ 
                         (1|treat:dam)+
                         (1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="colder"))
model1b_MFD_cold<-lmer(MFD~treat+
                    (1|sire)+(1|dam)+
                    #(1|treat:sire)+
                    (1|treat:dam),
                    #(1|sire:dam),
                    subset(data_exp_thirds,temp_midP_cat=="colder"))
model1b_veg_warm<-lmer(mean_sh_growth~treat+
                         #(1|sire)+
                         #(1|dam)+
                         #(1|treat:sire)+
                         #(1|treat:dam)+
                         (1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="warmer"))
model1b_FFD_warm<-lmer(FFD~treat+
                         #(1|sire)+
                         (1|dam)+
                         (1|treat:sire)+
                         (1|treat:dam)+
                         (1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="warmer"))
# Convergence warnings!
model1b_LFD_warm<-lmer(LFD~treat+
                         (1|sire)+
                         (1|dam),
                         #(1|treat:sire)+ 
                         #(1|treat:dam)+
                         #(1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="warmer"))
model1b_MFD_warm<-lmer(MFD~treat+
                         (1|sire)+(1|dam)+
                         (1|treat:sire),
                         #(1|treat:dam)+
                         #(1|sire:dam),
                       subset(data_exp_thirds,temp_midP_cat=="warmer"))

summary(model1b_veg_cold)
summary(model1b_FFD_cold)
summary(model1b_LFD_cold)
summary(model1b_MFD_cold)
summary(model1b_veg_warm)
summary(model1b_FFD_warm)
summary(model1b_LFD_warm)
summary(model1b_MFD_warm)

# Proportions of variance plasticities

Variance_veg_treat_cold <- as.data.frame(VarCorr(model1b_veg_cold))[,c(1,4)]  
Variance_FFD_treat_cold <- as.data.frame(VarCorr(model1b_FFD_cold))[,c(1,4)]  
Variance_LFD_treat_cold <- as.data.frame(VarCorr(model1b_LFD_cold))[,c(1,4)]  
Variance_MFD_treat_cold <- as.data.frame(VarCorr(model1b_MFD_cold))[,c(1,4)]  
Variance_veg_treat_warm <- as.data.frame(VarCorr(model1b_veg_warm))[,c(1,4)]  
Variance_FFD_treat_warm <- as.data.frame(VarCorr(model1b_FFD_warm))[,c(1,4)]  
Variance_LFD_treat_warm <- as.data.frame(VarCorr(model1b_LFD_warm))[,c(1,4)]  
Variance_MFD_treat_warm <- as.data.frame(VarCorr(model1b_MFD_warm))[,c(1,4)]  

PropVar_veg_treat_cold <- Variance_veg_treat_cold%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="veg",origin="cold")
PropVar_FFD_treat_cold <- Variance_FFD_treat_cold%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="FFD",origin="cold")
PropVar_LFD_treat_cold <- Variance_LFD_treat_cold%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="LFD",origin="cold")
PropVar_MFD_treat_cold <- Variance_MFD_treat_cold%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="MFD",origin="cold")
PropVar_veg_treat_warm <- Variance_veg_treat_warm%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="veg",origin="warm")
PropVar_FFD_treat_warm <- Variance_FFD_treat_warm%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="FFD",origin="warm")
PropVar_LFD_treat_warm <- Variance_LFD_treat_warm%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="LFD",origin="warm")
PropVar_MFD_treat_warm <- Variance_MFD_treat_warm%>%
  mutate(propvar=vcov/sum(vcov))%>%mutate(variable="MFD",origin="warm")

Props_var_treat_thirds<-rbind(PropVar_veg_treat_cold,PropVar_FFD_treat_cold,
                              PropVar_LFD_treat_cold,PropVar_MFD_treat_cold,
                              PropVar_veg_treat_warm,PropVar_FFD_treat_warm,
                              PropVar_LFD_treat_warm,PropVar_MFD_treat_warm)

ggplot(Props_var_treat_thirds,aes(x=variable,y=propvar,fill=grp))+
  geom_col()+facet_wrap(~origin,labeller="label_both")+
  ggtitle("Plasticities: Proportions of variance")

# Heritability and maternal effects plasticities

her_veg_treat_cold<-4*subset(PropVar_veg_treat_cold,grp=="treat:sire")[3]
her_FFD_treat_cold<-0
her_LFD_treat_cold<-0
her_MFD_treat_cold<-0
her_veg_treat_warm<-0
her_FFD_treat_warm<-4*subset(PropVar_FFD_treat_warm,grp=="treat:sire")[3]
her_LFD_treat_warm<-0
her_MFD_treat_warm<-4*subset(PropVar_MFD_treat_warm,grp=="treat:sire")[3]


her_treat_thirds<-data.frame(value=rbind(her_veg_treat_cold,her_FFD_treat_cold,
                                         her_LFD_treat_cold,her_MFD_treat_cold,
                                         her_veg_treat_warm,her_FFD_treat_warm,
                                         her_LFD_treat_warm,her_MFD_treat_warm))%>%
  mutate(variable=c("veg","FFD","LFD","MFD","veg","FFD","LFD","MFD"),
         effect="Heritability",
         origin=c("cold","cold","cold","cold","warm","warm","warm","warm"))%>%
  rename(value=propvar)

mat_veg_treat_cold<-(0-
                       subset(Variance_veg_treat_cold,grp=="treat:sire")[2])/
  Variance_veg_treat_cold%>%summarise(sum(vcov))
mat_FFD_treat_cold<-(subset(Variance_FFD_treat_cold,grp=="treat:dam")[2]-
                  0)/
  Variance_FFD_treat_cold%>%summarise(sum(vcov))
mat_LFD_treat_cold<-(subset(Variance_LFD_treat_cold,grp=="treat:dam")[2]-
                  0)/
  Variance_LFD_treat_cold%>%summarise(sum(vcov))
mat_MFD_treat_cold<-(subset(Variance_MFD_treat_cold,grp=="treat:dam")[2]-
                  0)/
  Variance_MFD_treat_cold%>%summarise(sum(vcov))
mat_veg_treat_warm<-(0-0)/
  Variance_veg_treat_warm%>%summarise(vcov=sum(vcov))
mat_FFD_treat_warm<-(subset(Variance_FFD_treat_warm,grp=="treat:dam")[2]-
                       subset(Variance_FFD_treat_warm,grp=="treat:sire")[2])/
  Variance_FFD_treat_warm%>%summarise(sum(vcov))
mat_LFD_treat_warm<-(0-0)/
  Variance_LFD_treat_warm%>%summarise(vcov=sum(vcov))
mat_MFD_treat_warm<-(0-
  subset(Variance_MFD_treat_warm,grp=="treat:sire")[2])/
  Variance_MFD_treat_warm%>%summarise(sum(vcov))

mat_treat_thirds<-data.frame(value=rbind(mat_veg_treat_cold,mat_FFD_treat_cold,
                                         mat_LFD_treat_cold,mat_MFD_treat_cold,
                                         mat_veg_treat_warm,mat_FFD_treat_warm,
                                         mat_LFD_treat_warm,mat_MFD_treat_warm))%>%
  mutate(variable=c("veg","FFD","LFD","MFD","veg","FFD","LFD","MFD"),
         effect="Maternal effects",
         origin=c("cold","cold","cold","cold","warm","warm","warm","warm"))%>%
  rename(value=vcov)

her_mat_treat_thirds<-rbind(her_treat_thirds,mat_treat_thirds)
her_mat_treat_thirds

ggplot(her_mat_treat_thirds,aes(x=variable,y=value,fill=effect))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~origin,labeller="label_both")+
  ggtitle("Plasticities: heritability and maternal effects")


