grid.arrange(ggplot(data.frame(ggpredict(model_offpsring_FFD,terms=c("treat"))),
                    aes(x=x,y=predicted,ymin=predicted-std.error,ymax=predicted+std.error))+
               geom_point(size=3)+geom_errorbar(linewidth=1,width=.1)+
               xlab("Treatment")+ylab("FFD")+my_theme()+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_FFD,
                                                                       terms=c("treat")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_LFD,terms=c("treat"))),
                    aes(x=x,y=predicted,ymin=predicted-std.error,ymax=predicted+std.error))+
               geom_point(size=3)+geom_errorbar(linewidth=1,width=.1)+
               xlab("Treatment")+ylab("LFD")+my_theme()+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_FFD,
                                                                       terms=c("treat")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_date50,terms=c("treat"))),
                    aes(x=x,y=predicted,ymin=predicted-std.error,ymax=predicted+std.error))+
               geom_point(size=3)+geom_errorbar(linewidth=1,width=.1)+
               xlab("Treatment")+ylab("date50")+my_theme()+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_FFD,
                                                                       terms=c("treat")))$x))),
             ncol=3)

ggplot(data.frame(ggpredict(model_offpsring_date50,terms=c("temp_father"))),
       aes(x=x,y=predicted,ymin=predicted-std.error,ymax=predicted+std.error))+
  geom_ribbon(fill="lightgrey")+geom_line(linewidth=2)+my_theme()+
  xlab("Temperature of the father")+ylab("Median date of flowering (date50)")


plot(ggpredict(model_offpsring_FFD,terms=c("treat","temp_mother[minmax]")))

grid.arrange(ggplot(data.frame(ggpredict(model_offpsring_FFD,terms=c("treat","temp_mother[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("FFD")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_FFD,
                                                                       terms=c("treat","temp_mother[minmax]")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_LFD,terms=c("treat","temp_mother[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("LFD")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_LFD,
                                                                       terms=c("treat","temp_mother[minmax]")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_date50,terms=c("treat","temp_mother[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("date50")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_date50,
                                                                       terms=c("treat","temp_mother[minmax]")))$x))),
             ncol=3)

grid.arrange(ggplot(data.frame(ggpredict(model_offpsring_FFD,terms=c("treat","temp_father[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("FFD")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_FFD,
                                                                       terms=c("treat","temp_father[minmax]")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_LFD,terms=c("treat","temp_father[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("LFD")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_LFD,
                                                                       terms=c("treat","temp_father[minmax]")))$x))),
             ggplot(data.frame(ggpredict(model_offpsring_date50,terms=c("treat","temp_father[minmax]"))),
                    aes(x=x,y=predicted,color=group,group=group))+
               geom_point(size=3,position=position_dodge(0.1))+
               geom_line(linewidth=1,position=position_dodge(0.1))+
               geom_errorbar(aes(ymin=predicted-std.error,ymax=predicted+std.error),
                             linewidth=1,position=position_dodge(0.1),width=.1)+
               xlab(NULL)+ylab("date50")+my_theme()+
               scale_colour_manual(values=c("blue","red"))+
               scale_x_discrete(limits=rev(levels(data.frame(ggpredict(model_offpsring_date50,
                                                                       terms=c("treat","temp_father[minmax]")))$x))),
             ncol=3)

