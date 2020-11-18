library(ggplot2)

#input data
df<-read.csv("input/DataFrame.csv")

#aggregate to 1 m distance by ID
df.ag<-aggregate(Z~floor(layer)+ID, FUN=function(x) quantile(x,0.50), df)
df.ag$Z999<-aggregate(Z~floor(layer)+ID, FUN=function(x) quantile(x,0.999), df)[,3]

#plot
ggplot(df, aes(layer, Z))+
  geom_hex(bins=100, fill="grey", color="grey")+
  stat_smooth(data=df.ag[df.ag$`floor(layer)`>=10,], aes(x=`floor(layer)`, y=Z999), 
              method="lm", color="blue", linetype=1)+
  stat_smooth(data=df.ag[df.ag$`floor(layer)`<=10,], aes(x=`floor(layer)`, y=Z999), 
              method="lm", color="blue", linetype=1)+
  geom_path(data=df.ag, aes(x=`floor(layer)`, y=Z999), color="red")+
  facet_wrap(~ID, scales="free_y")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("Distance from Island Edge (m)")+
  ylab("Canopy Height (m)")

m<-lm(Z999~`floor(layer)`:ID, data=df.ag[df.ag$`floor(layer)`>=10,])
summary(m)

slopes<-data.frame(coef=coef(m),
           names=names(coef(m)),
           se=summary(m)$coefficients[,2])
slopes$names<-substr(slopes$names,nchar("`floor(layer)`: "), 100)

plot(density(slopes$coef[-1]))

ggplot(slopes[-1,],aes(x=reorder(names, coef), y=coef))+
  geom_bar(stat="identity", fill="forestgreen", alpha=0.7, color="black")+
  geom_errorbar(aes(x= names, ymin=coef-se*1.96, 
                    ymax=coef+se*1.96), width=0.5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1))+
  geom_hline(aes(yintercept=0), linetype=2)+
  xlab("")+ylab("Coefficient")
  

#now aggregate all
df.ag<-aggregate(Z~floor(layer), FUN=function(x) quantile(x,0.50), df)
df.ag$Z999<-aggregate(Z~floor(layer), FUN=function(x) quantile(x,0.999), df)[,2]

m<-lm(log(Z999)~`floor(layer)`, data=df.ag)

ggplot(df, aes(layer, Z))+
  geom_hex(bins=100, fill="grey", color="grey")+
  stat_smooth(data=df.ag[df.ag$`floor(layer)`>=15,], aes(x=`floor(layer)`, y=Z999),
              method="lm", formula= y~poly(x,2),color="blue", linetype=1)+
  # stat_smooth(data=df.ag[df.ag$`floor(layer)`<=15,], aes(x=`floor(layer)`, y=Z999), 
  #             method="lm", color="blue", linetype=1)+
  # geom_path(data=df.ag, aes(x=`floor(layer)`, y=Z), color="red")+
  geom_path(data=df.ag, aes(x=`floor(layer)`, y=Z999), color="red")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("Distance from Island Edge (m)")+
  ylab("Canopy Height (m)")+
  ylim(0,15)

lm(Z~`floor(layer)`, df.ag[df.ag$`floor(layer)`>=5,])

lm(Z999~poly(`floor(layer)`,2), data=df.ag)
