ggplot(data_exp,aes(total_n_fl,mean_sh_growth))+
  geom_point()+geom_smooth(method=lm)
ggplot(data_exp,aes(mean_sh_h_date1,mean_sh_growth))+
  geom_point()+geom_smooth(method=lm)
ggplot(data_exp,aes(mean_sh_h_date2,mean_sh_growth))+
  geom_point()+geom_smooth(method=lm)

summary(lmer(mean_sh_growth~total_n_fl+(1|sire)+(1|dam),data_exp))
summary(lmer(mean_sh_growth~mean_sh_h_date1+(1|sire)+(1|dam),data_exp))
summary(lmer(mean_sh_growth~mean_sh_h_date2+(1|sire)+(1|dam),data_exp))

plot(ggpredict(lmer(mean_sh_growth~total_n_fl+(1|sire)+(1|dam),data_exp)),add.data=T)
plot(ggpredict(lmer(mean_sh_growth~mean_sh_h_date1+(1|sire)+(1|dam),data_exp)),add.data=T)
plot(ggpredict(lmer(mean_sh_growth~mean_sh_h_date2+(1|sire)+(1|dam),data_exp)),add.data=T)

summary(lmer(FFD~total_n_fl+(1|sire)+(1|dam),data_exp))
summary(lmer(FFD~mean_sh_h_date1+(1|sire)+(1|dam),data_exp))
summary(lmer(FFD~mean_sh_h_date2+(1|sire)+(1|dam),data_exp))
summary(lmer(MFD~total_n_fl+(1|sire)+(1|dam),data_exp))
summary(lmer(MFD~mean_sh_h_date1+(1|sire)+(1|dam),data_exp))
summary(lmer(MFD~mean_sh_h_date2+(1|sire)+(1|dam),data_exp))
summary(lmer(LFD~total_n_fl+(1|sire)+(1|dam),data_exp))
summary(lmer(LFD~mean_sh_h_date1+(1|sire)+(1|dam),data_exp))
summary(lmer(LFD~mean_sh_h_date2+(1|sire)+(1|dam),data_exp))

data_exp%>%
  dplyr::select(FFD,MFD,LFD,total_n_fl,mean_sh_h_date1,mean_sh_h_date2)%>%
  pivot_longer(cols=FFD:LFD,names_to="phen_measure",values_to="phen_value")%>%
  pivot_longer(cols=total_n_fl:mean_sh_h_date2,
               names_to="condition_measure",values_to="condition_value")%>%
  ggplot(aes(x=condition_value,phen_value))+
  geom_point()+geom_smooth(method="lm")+
  facet_grid2(rows=vars(phen_measure),cols=vars(condition_measure),
              scales="free_x")
