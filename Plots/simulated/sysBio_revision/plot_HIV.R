setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)
require(reshape2)

d = read.table("MDCat_HIVsim_vary_k.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

d = merge(d,h)
d$error = abs(d$trueAge-d$estAge)/d$height
d$clockModel = factor(d$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))

d2= dcast(k+treeModel+clockModel+rep~"error",data=d,value.var = "error",fun.aggregate = mean)
ggplot(d2,aes(x=k,y=100*error,color=treeModel)) +
  stat_summary() + geom_line(stat="summary") + 
  scale_x_log10(breaks=c(2,5,10,25,50)) + xlab("# rate categories (k)") + ylab("divergence time error (%)") +
  scale_color_brewer(palette = "Set2")+
  facet_wrap(~clockModel,scale="free") +
  theme_classic() + theme(legend.title = element_blank(),legend.position = "bottom")
ggsave("MDCat_HIVsim_vary_k.pdf",width = 6,height=4)
with(d2,t.test(d2[k==2,"error"],d2[k==5,"error"]))

summary(aov(error ~ k*(treeModel+clockModel),data=d2[d2$k %in% c(2,5),]))
summary(aov(error ~ k*(treeModel+clockModel),data=d2[d2$k %in% c(5,10),]))
##################################################################
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
d = read.table("MDCat_HIVsim_vary_k_with_crossval.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))
head(d)
d = merge(d,h)
d$error = abs(d$trueAge-d$estAge)/d$height
d$clockModel = factor(d$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))
d$selected = "Others"
d[d$k == "selected",]$selected = "Cross-validation"
d[d$k == 50,]$selected = "Default (k=50)"
d$selected = factor(d$selected, levels=c("Default (k=50)","Cross-validation","Others"))


qplot((sub("k","",merge(d[d$k == "selected" & d$nodeName =="I1",c(1,2,3,4,6,7,9,13)],
                                  d[d$k != "selected" & d$nodeName =="I1",c(1,2,3,4,5,6,7,9,13)],
                                  by=c("treeModel","clockGroup","rep","clockModel","nodeName","trueAge","estAge","error"))$k)
))+theme_bw()+xlab("k")+ylab("# replicates")
ggsave("selectedk_HIV.pdf",width=2.4, height = 4)


require(reshape2)
require(scales)
d2 = dcast(selected+treeModel+clockModel+rep+k~"error",data=d,value.var = "error",fun.aggregate = mean)

ggplot(d2,aes(x=reorder(treeModel,error),y=error,fill=selected)) + #geom_boxplot(outlier.size = 0.2) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width = 0.9),size=0.3) + 
  xlab("") + scale_y_continuous(name = "divergence time error",labels = percent) + 
  facet_wrap(~clockModel,scale="free") + theme_classic() + 
  scale_fill_brewer(name="",palette = "Set2")+
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),legend.position = "none")
ggsave("MDCat_HIVsim_crossval_k.pdf",width=8, height=4)
