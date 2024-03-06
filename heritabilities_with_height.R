### Models (with mean_sh_h_date2 )

model1a_veg_uh_height<-lmer(mean_sh_growth~mean_sh_h_date2 +
                              #(1|sire)
                              +(1|dam),
                         #(1|sire:dam) # Removed to avoid singular fit
                         subset(data_exp,treat=="unheated"))
model1a_veg_h_height<-lmer(mean_sh_growth~mean_sh_h_date2 +
                             #(1|sire)+
                             (1|dam)+(1|sire:dam),
                        subset(data_exp,treat=="heatmat"))
model1a_FFD_uh_height<-lmer(FFD~mean_sh_h_date2 +(1|sire)+(1|dam)+(1|sire:dam),
                         subset(data_exp,treat=="unheated"))
model1a_FFD_h_height<-lmer(FFD~mean_sh_h_date2 +(1|sire)+(1|dam)+(1|sire:dam),
                        subset(data_exp,treat=="heatmat"))
model1a_LFD_uh_height<-lmer(LFD~mean_sh_h_date2 +(1|sire)+(1|dam)+(1|sire:dam),
                         subset(data_exp,treat=="unheated"))
model1a_LFD_h_height<-lmer(LFD~mean_sh_h_date2 +(1|dam)+(1|sire:dam)+
                        (1|sire),
                        subset(data_exp,treat=="heatmat"))
model1a_MFD_uh_height<-lmer(MFD~mean_sh_h_date2 +(1|sire)+(1|dam)+(1|sire:dam),
                         subset(data_exp,treat=="unheated"))
model1a_MFD_h_height<-lmer(MFD~mean_sh_h_date2 +(1|sire)+(1|dam)+(1|sire:dam),
                        subset(data_exp,treat=="heatmat"))
summary(model1a_veg_uh_height)
summary(model1a_veg_h_height)
summary(model1a_FFD_uh_height)
summary(model1a_FFD_h_height)
summary(model1a_LFD_uh_height)
summary(model1a_LFD_h_height)
summary(model1a_MFD_uh_height)
summary(model1a_MFD_h_height)

Variance_veg_uh_height<-as.data.frame(VarCorr(model1a_veg_uh_height))[,c(1,4)]
Variance_FFD_uh_height<-as.data.frame(VarCorr(model1a_FFD_uh_height))[,c(1,4)]
Variance_LFD_uh_height<-as.data.frame(VarCorr(model1a_LFD_uh_height))[,c(1,4)]
Variance_MFD_uh_height<-as.data.frame(VarCorr(model1a_MFD_uh_height))[,c(1,4)]
Variance_veg_h_height<-as.data.frame(VarCorr(model1a_veg_h_height))[,c(1,4)]
Variance_FFD_h_height<-as.data.frame(VarCorr(model1a_FFD_h_height))[,c(1,4)]
Variance_LFD_h_height<-as.data.frame(VarCorr(model1a_LFD_h_height))[,c(1,4)]
Variance_MFD_h_height<-as.data.frame(VarCorr(model1a_MFD_h_height))[,c(1,4)]
# Intra-class correlations
PropVar_veg_uh_height <- Variance_veg_uh_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="unheated")
PropVar_FFD_uh_height <- Variance_FFD_uh_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="unheated")
PropVar_LFD_uh_height <- Variance_LFD_uh_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="unheated")
PropVar_MFD_uh_height <- Variance_MFD_uh_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="unheated")
PropVar_veg_h_height <- Variance_veg_h_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="veg",treat="heated")
PropVar_FFD_h_height <- Variance_FFD_h_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="FFD",treat="heated")
PropVar_LFD_h_height <- Variance_LFD_h_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="LFD",treat="heated")
PropVar_MFD_h_height <- Variance_MFD_h_height%>%mutate(propvar=vcov/sum(vcov))%>%
  mutate(variable="MFD",treat="heated")
Props_var_height<-rbind(PropVar_veg_uh_height,PropVar_FFD_uh_height,PropVar_LFD_uh_height,
                     PropVar_MFD_uh_height,PropVar_veg_h_height,PropVar_FFD_h_height,
                     PropVar_LFD_h_height,PropVar_MFD_h_height)

ggplot(Props_var_height,aes(x=variable,y=propvar,fill=grp))+
  geom_col()+facet_wrap(~treat)

#### Heritability and maternal effects

# h^2 (paternal effects)
her_veg_uh_height<-0
her_veg_h_height<-0
her_FFD_uh_height<-as.numeric(4*subset(PropVar_FFD_uh_height,grp=="sire")[3])
her_FFD_h_height<-as.numeric(4*subset(PropVar_FFD_h_height,grp=="sire")[3])
her_LFD_uh_height<-as.numeric(4*subset(PropVar_LFD_uh_height,grp=="sire")[3])
her_LFD_h_height<-as.numeric(4*subset(PropVar_LFD_h_height,grp=="sire")[3])
her_MFD_uh_height<-as.numeric(4*subset(PropVar_MFD_uh_height,grp=="sire")[3])
her_MFD_h_height<-as.numeric(4*subset(PropVar_MFD_h_height,grp=="sire")[3])
# Because the additive genetic variance, VA,
# is expected to be four times the among pollen‐donor variance 
# (Falconer & Mackay, 1996; Lynch & Walsh, 1998)
her_height<-data.frame(value=rbind(her_veg_uh_height,her_veg_h_height,her_FFD_uh_height,
                                her_FFD_h_height,her_LFD_uh_height,her_LFD_h_height,
                                her_MFD_uh_height,her_MFD_h_height))%>%
  rownames_to_column()%>%
  mutate(variable=c("veg","veg","FFD","FFD","LFD","LFD","MFD","MFD"),
         treat=c("unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated"),
         effect="Heritability")

# maternal effects

mat_veg_uh_height<-(Variance_veg_uh_height[1,2]-0)/
  as.numeric(Variance_veg_uh_height%>%summarise(sum(vcov)))
mat_veg_h_height<-(Variance_veg_h_height[2,2]-0)/
  as.numeric(Variance_veg_h_height%>%summarise(sum(vcov)))
mat_FFD_uh_height<-(Variance_FFD_uh_height[2,2]-Variance_FFD_uh_height[3,2])/
  as.numeric(Variance_FFD_uh_height%>%summarise(sum(vcov)))
mat_FFD_h_height<-(Variance_FFD_h_height[2,2]-Variance_FFD_h_height[3,2])/
  as.numeric(Variance_FFD_h_height%>%summarise(sum(vcov)))
mat_LFD_uh_height<-(Variance_LFD_uh_height[2,2]-Variance_LFD_uh_height[3,2])/
  as.numeric(Variance_LFD_uh_height%>%summarise(sum(vcov)))
mat_LFD_h_height<-(Variance_LFD_h_height[2,2]-Variance_LFD_h_height[3,2])/
  as.numeric(Variance_LFD_h_height%>%summarise(sum(vcov)))
mat_MFD_uh_height<-(Variance_MFD_uh_height[2,2]-Variance_MFD_uh_height[3,2])/
  as.numeric(Variance_MFD_uh_height%>%summarise(sum(vcov)))
mat_MFD_h_height<-(Variance_MFD_h_height[2,2]-Variance_MFD_h_height[3,2])/
  as.numeric(Variance_MFD_h_height%>%summarise(sum(vcov)))
mat_height<-data.frame(value=rbind(mat_veg_uh_height,mat_veg_h_height,mat_FFD_uh_height,
                                mat_FFD_h_height,mat_LFD_uh_height,mat_LFD_h_height,
                                mat_MFD_uh_height,mat_MFD_h_height))%>%
  rownames_to_column()%>%
  mutate(variable=c("veg","veg","FFD","FFD","LFD","LFD","MFD","MFD"),
         treat=c("unheated","heated","unheated","heated","unheated","heated",
                 "unheated","heated"),
         effect="Maternal effects")


her_mat_height<-rbind(her_height,mat_height)
her_mat_height

ggplot(her_mat_height,aes(x=variable,y=value,fill=effect))+
  geom_bar(stat="identity",position="dodge")+facet_wrap(~treat)

ggplot(rbind(her_mat%>%mutate(height="Without height"),
             her_mat_height%>%mutate(height="With hieght")),
       aes(x=variable,y=value,fill=height))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~treat+effect)



