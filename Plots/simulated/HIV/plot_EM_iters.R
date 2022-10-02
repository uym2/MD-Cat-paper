require(ggplot2)
require(reshape2)

d = read.table("EM_iters.txt",header=T)
d$clockModel =factor(d$clockModel,levels = c("unif","exp","gamma","lognorm",
                                               "clock4","clock3","clock1","clock2",
                                               "trilnormcave","trilnormvex","trilnorm",
                                               "quartlnorm"),
                      labels = c("Uniform","Exponential","Gamma","Lognormal",
                                 "Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4",
                                 "Trimodal 1","Trimodal 2","Trimodal 3",
                                 "Quartmodal"))

d$iniType =factor(d$iniType,labels = c("RTT","wLogDate"))

m = merge(d,dcast(d,clockModel~.,value.var = "llh",fun.aggregate = max))
ggplot(d,aes(x=iter,y=llh,group=interaction(iniType,iniID),color=iniType,
             alpha=paste(clockModel,iniType,iniID) %in% paste(m[m$llh == m$.,1],m[m$llh == m$.,2],m[m$llh == m$.,3]))) + 
  geom_line(size=1) + scale_y_continuous(trans="identity") +
  scale_x_continuous(trans="log10") +
  scale_alpha_manual(name="empty",values = c(0.04,1)) + 
  scale_color_brewer(palette = "Set2",name="empty") +
  facet_wrap(~clockModel,scale="free",nrow=2) + theme_classic() +
  xlab("iteration") + ylab("log-likelihood") +
  theme(legend.title = element_blank(),legend.position = "bottom")
ggsave("EM_iters.pdf",width=5,height=4)
