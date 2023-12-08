setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)

d = read.table("MDCat_Angiosperm_vary_k_nodeAge.txt",header=T)

d$error = abs(d$trueNodeAge-d$estNodeAge)/140

ggplot(d,aes(x=k,y=error*100,color=scenario)) +
  stat_summary() + geom_line(stat="summary") + 
  scale_x_log10(breaks=c(2,5,10,25,50,100)) +  xlab("# rate categories") + ylab("divergence time error (%)") + 
  theme_classic() + theme(legend.title = element_blank(),legend.position = c(0.8,0.8))
ggsave("MDCat_Angiosperm_vary_k.pdf",width = 6,height=4)

########################################
require(reshape2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


d = read.table("MDCat_Angiosperm_vary_k_with_crossval.txt",header=T)
d$error = abs(d$trueNodeAge-d$estNodeAge)/140
d$selected = "Others"
d[d$k == "kselected",]$selected = "Cross-validation"
d[d$k == "k50",]$selected = "Default (k=50)"
d$selected = factor(d$selected, levels=c("Default (k=50)","Cross-validation","Others"))




qplot((sub("k","",merge(d[d$k == "kselected" & d$nodeName =="I0",c(1,2,4,5,6,7)],d[d$k != "kselected"& d$nodeName =="I0",c(1,2,3,4,5,6,7)],
      by=c("scenario","rep","nodeName","trueNodeAge","estNodeAge","error"))$k)
))+theme_bw()+xlab("k")+ylab("# replicates")
ggsave("selectedk_angio.pdf",width=3.2, height = 4)

d2 = dcast(selected+scenario+rep+k~"error",data=d,value.var = "error",fun.aggregate = mean)

require(scales)

head(d2)
ggplot(d2,aes(x=scenario,y=error,fill=selected)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width = 0.9),size=0.3) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank(),
                          legend.title = element_blank(),legend.position = "bottom") + 
  ylab("divergence time error")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = percent)
nrow(d2[d2$selected=="Default (k=50)",])
ggsave("MDCat_Angiosperm_crossval_k.pdf",width=6, height =4 )
