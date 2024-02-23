setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)
require(reshape2)
require(tidyverse)

d = read.table("MDCat_HIVsim_vary_k_revised.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

d = merge(d,h)
#d$error = abs(d$trueAge-d$estAge)/d$height
d$clockModel = factor(d$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))

#d2= dcast(k+treeModel+clockModel+rep~"error",data=d,value.var = "error",fun.aggregate = mean)

d2 = d %>% group_by(treeModel    ,    clockGroup ,   rep , clockModel , k) %>%
  summarise(e = sqrt(mean( (trueAge-estAge)^2) ),height=unique(height)) %>%
  mutate(error = e/height)
d2
ggplot(d2[d2$k != 75,],aes(x=k,y=error,color=treeModel)) +
  stat_summary() + geom_line(stat="summary") + 
  scale_x_log10(breaks=c(2,5,10,25,50)) + xlab("# rate categories (k)") + ylab("divergence time RSME (height normalized)") +
  scale_color_brewer(palette = "Set2")+
  facet_wrap(~clockModel,scale="free") +
  theme_classic() + theme(legend.title = element_blank(),legend.position = "bottom")
ggsave("MDCat_HIVsim_vary_k_revised.pdf",width = 7.1,height=4)

with(d2,t.test(d2[k==2,]$error,d2[k==5,]$error,paired = T))
with(d2,t.test(d2[k==5,]$error,d2[k==10,]$error,paired = T))
with(d2,t.test(d2[k==5,]$error,d2[k==50,]$error,paired = T))


##################################################################
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
hkc = read.table("MDCat_HIVsim_vary_k_with_model_selection.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))
head(hkc)
hkc = merge(hkc,h)
hkc$error = abs(hkc$trueNodeAge-hkc$estNodeAge)/hkc$height
hkc$clockModel = factor(hkc$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))
hkc$selected = "Others"
hkc[hkc$k == "crossval",]$selected = "Cross-validation"
hkc[hkc$k == "aic",]$selected = "AIC"
hkc[hkc$k == "bic",]$selected = "BIC"
hkc[hkc$k == 50,]$selected = "Default (k=50)"
hkc$selected = factor(hkc$selected, levels=c("Default (k=50)","Cross-validation","AIC","BIC","Others"))

dl=merge(hkc[hkc$k == "crossval" & hkc$nodeName =="I1",c(1,2,3,4,6,7,9,13)-1],
         hkc[! hkc$k %in%  c("crossval","aic","bic") & hkc$nodeName =="I1",c(1,2,3,4,5,6,7,9,13)-1],
         by=c("treeModel","rep","clockModel","nodeName","trueNodeAge","estNodeAge","error"))$k

qplot(factor(sub("k","",dl),levels=c(2,5,10,25,50,100)))+
  theme_bw()+xlab("k")+ylab("# replicates")
ggsave("selectedk_HIV_revised.pdf",width=3, height = 4)

dl=merge(hkc[hkc$k == "aic" & hkc$nodeName =="I1",c(1,2,3,4,6,7,9,13)-1],
         hkc[! hkc$k %in%  c("crossval","aic","bic") & hkc$nodeName =="I1",c(1,2,3,4,5,6,7,9,13)-1],
         by=c("treeModel","rep","clockModel","nodeName","trueNodeAge","estNodeAge","error"))$k

qplot(factor(sub("k","",dl),levels=c(2,5,10,25,50,100)))+
  theme_bw()+xlab("k")+ylab("# replicates")
ggsave("aic_k_HIV.pdf",width=3, height = 4)

dl=merge(hkc[hkc$k == "bic" & hkc$nodeName =="I1",c(1,2,3,4,6,7,9,13)-1],
         hkc[! hkc$k %in%  c("crossval","aic","bic") & hkc$nodeName =="I1",c(1,2,3,4,5,6,7,9,13)-1],
         by=c("treeModel","rep","clockModel","nodeName","trueNodeAge","estNodeAge","error"))$k
qplot(factor(sub("k","",dl),levels=c(2,5,10,25,50,100)))+
  theme_bw()+xlab("k")+ylab("# replicates")
ggsave("bic_k_HIV.pdf",width=3, height = 4)

require(reshape2)
require(scales)

#d2 = dcast(selected+treeModel+clockModel+rep+k~"error",data=hkc,value.var = "error",fun.aggregate = mean)
hkc %>% group_by(treeModel    ,    rep , clockModel , k, selected) %>%
  summarise(e = sqrt(mean( (trueNodeAge-estNodeAge)^2) ) , height=unique(height)) %>%

ggplot(aes(x=clockModel,y=e/height,fill=selected)) + #geom_boxplot(outlier.size = 0.2) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width = 0.9),size=0.3) + 
  xlab("") + scale_y_continuous(name = "divergence time error",labels = percent) + 
  facet_wrap(~reorder(treeModel,e/height),scale="free") + theme_classic() + 
  scale_fill_brewer(name="",palette = "Set2")+
  theme(axis.text.x = element_text(angle = 0),
        legend.title = element_blank()) + 
  theme(legend.position = "bottom")
ggsave("MDCat_HIVsim_model_selection.pdf",width=8, height=5)


hkc %>% group_by(treeModel,  clockModel , k, selected, rep) %>%
  summarise(e = sqrt(mean( (trueNodeAge-estNodeAge)^2) ) , height=unique(height)) %>%
  mutate(rmse=e/height) %>% 
  group_by( k, selected) %>% 
  summarise(error=mean(rmse))

hkc %>% filter(k %in% c("aic","crossval","bic") ) %>% 
  group_by(treeModel , clockModel , k, rep) %>%
  summarise(e = sqrt(mean( (trueNodeAge-estNodeAge)^2) ) , height=unique(height)) %>%
  mutate(rmse=e/height) %>% 
  select(treeModel,clockModel, k  ,rep ,   rmse) %>%
  pivot_wider(names_from = k,values_from = rmse) %>%
  group_by() %>%
  summarize(t.test(`bic`,`crossval`,paired=T)$p.value)

