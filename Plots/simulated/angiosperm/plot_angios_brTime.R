require(ggplot2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

d1 = read.table("results_brTime.txt",header=T)
d2 = read.table("results_brTime_moreBEAST.txt",header=T)
d=rbind(d1,d2)
d$method = factor(d$method,levels=c("wlogdate","emd","Blnorm","BEAST_strict","BEAST_rcla","reltime","BEAST_lognorm"),
                  labels = c("wLogDate","MD-Cat","BEAST_lognorm","BEAST_strict","BEAST_rcla","RelTime","BEAST_lnorm_old"))

ggplot(d[d$method != "BEAST_lnorm",],aes(x=trueLength,y=estLength,color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  stat_summary(size=0.1, alpha=0.3) + 
  #stat_summary(geom="line") + 
  geom_smooth(method="lm") + 
  geom_abline(linetype=2) + 
  scale_x_log10() + scale_y_log10() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~clockModel) + 
  theme_classic() + theme(legend.position = c(0.85,0.2),legend.title = element_blank()) + 
  xlab("true branch length (million years)") + ylab("estimated branch length (million years)")
ggsave("angios_brTime.pdf",width = 4.5,height = 4)

