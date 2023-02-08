
df_out <- d_surv
df_out <- df_out %>% arrange(left_period_e)
df_out$ID=1:nrow(df_out)
df_out$Sex <- as.factor(df_out$sex)
levels(df_out$Sex) <- c("Female","Male")
timein_period_sex_plot <- ggplot(df_out,aes(x=left_period_e,y=ID,color=Sex))+
    geom_errorbarh(aes(xmin=left_period_e,xmax=right_period_r),size=1)+
    scale_color_manual(values=met.brewer("Troy",2))+
    scale_x_continuous(breaks=c(0,52,52*2,52*3,52*4,52*5),labels=paste0("Jan ",2017:2022))+
    xlab("Study Period (Year)")+
    ggtitle("Survival Data Collared Individuals - Period - Sex")+
    theme_bw()+
    theme(axis.text.y=element_blank(),
        axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_period_sex_plot

ggsave("figures/timein_period_sex_plot.png",timein_period_sex_plot,height = 6, width = 9)


df_out$Status <- as.factor(df_out$censored)
levels(df_out$Status) <- c("Mortality","Censored")

timein_period_status_plot <- ggplot(df_out,aes(x=left_period_e,y=ID,color=Status))+
    geom_errorbarh(aes(xmin=left_period_e,xmax=right_period_r),size=1)+
    scale_color_manual(values=met.brewer("Johnson",2))+
    scale_x_continuous(breaks=c(0,52,52*2,52*3,52*4,52*5),labels=paste0("Jan ",2017:2022))+
    xlab("Study Period (Year)")+
    ggtitle("Survival Data Collared Individuals - Period - Mortality Status")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_period_status_plot

ggsave("figures/timein_period_status_plot.png",timein_period_status_plot,height = 6, width = 9)


df_out <- d_surv
df_out <- df_out %>% arrange(left_period_e)
df_out$Sex <- as.factor(df_out$sex)
levels(df_out$Sex) <- c("Female","Male")
df_out$Status <- as.factor(df_out$censored)
levels(df_out$Status) <- c("Mortality","Censored")
df_out$ID=1:nrow(df_out)
df_out$cwd_mort[is.na(df_out$cwd_mort)] <- "Unknown"
df_out$cwd_cap <- as.factor(df_out$cwd_cap)
df_out$cwd_mort <- as.factor(df_out$cwd_mort)
levels(df_out$cwd_cap) <- c("Negative/Unknown","Positive")
levels(df_out$cwd_mort) <- c("Negative","Positive","Unknown")


timein_period_cwd_cap_plot <- ggplot(df_out,aes(x=left_period_e,y=ID,color=cwd_cap))+
    geom_errorbarh(aes(xmin=left_period_e,xmax=right_period_r),size=1)+
    scale_color_manual("CWD Status\nat Capture",values=met.brewer("Java",2))+
    scale_x_continuous(breaks=c(0,52,52*2,52*3,52*4,52*5),labels=paste0("Jan ",2017:2022))+
    xlab("Study Period (Year)")+
    ggtitle("Survival Data Collared Individuals - Period - CWD Status at Capture")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_period_cwd_cap_plot
 
ggsave("figures/timein_period_cwd_cap_plot.png",timein_period_cwd_cap_plot,height = 6, width = 9)


timein_period_cwd_mort_plot <- ggplot(df_out,aes(x=left_period_e,y=ID,color=cwd_mort))+
    geom_errorbarh(aes(xmin=left_period_e,xmax=right_period_r),size=1)+
    scale_color_manual("CWD Status\nat Mortlity",values=met.brewer("Java",3))+
    scale_x_continuous(breaks=c(0,52,52*2,52*3,52*4,52*5),labels=paste0("Jan ",2017:2022))+
    xlab("Study Period (Year)")+
    ggtitle("Survival Data Collared Individuals - Period - CWD Status at Mortality")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_period_cwd_mort_plot
 
ggsave("figures/timein_period_cwd_mort_plot.png",timein_period_cwd_mort_plot,height = 6, width = 9)


#######################################
###
### Age plots
###
#######################################

df_out <- d_surv
df_out <- df_out %>% arrange(left_age_e)
df_out$Sex <- as.factor(df_out$sex)
levels(df_out$Sex) <- c("Female","Male")
df_out$Status <- as.factor(df_out$censored)
levels(df_out$Status) <- c("Mortality","Censored")
df_out$ID=1:nrow(df_out)

timein_age_status_plot <- ggplot(df_out,aes(x=left_age_e,y=ID,color=Status))+
    geom_errorbarh(aes(xmin=left_age_e,xmax=right_age_r),size=1)+
    scale_color_manual(values=met.brewer("Johnson",2))+
    scale_x_continuous(breaks=c(0,52*(1:20)),labels=0:20)+
    xlab("Age (Year)")+
    ggtitle("Survival Data Collared Individuals - Age - Mortality Status")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_age_status_plot
 
ggsave("figures/timein_age_status_plot.png",timein_age_status_plot,height = 6, width = 9)


df_out <- d_surv
df_out <- df_out %>% arrange(left_age_e)
df_out$Sex <- as.factor(df_out$sex)
levels(df_out$Sex) <- c("Female","Male")
df_out$Status <- as.factor(df_out$censored)
levels(df_out$Status) <- c("Mortality","Censored")
df_out$ID=1:nrow(df_out)
df_out$cwd_mort[is.na(df_out$cwd_mort)] <- "Unknown"
df_out$cwd_cap <- as.factor(df_out$cwd_cap)
df_out$cwd_mort <- as.factor(df_out$cwd_mort)
levels(df_out$cwd_cap) <- c("Negative/Unknown","Positive")
levels(df_out$cwd_mort) <- c("Negative","Positive","Unknown")

timein_age_cwd_cap_plot <- ggplot(df_out,aes(x=left_age_e,y=ID,color=cwd_cap))+
    geom_errorbarh(aes(xmin=left_age_e,xmax=right_age_r),size=1)+
    scale_color_manual("CWD Status\nat Capture",values=met.brewer("Java",2))+
    scale_x_continuous(breaks=c(0,52*(1:20)),labels=0:20)+
    xlab("Age (Year)")+
    ggtitle("Survival Data Collared Individuals - Age - CWD Status at Capture")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_age_cwd_cap_plot
 
ggsave("figures/timein_age_cwd_cap_plot.png",timein_age_cwd_cap_plot,height = 6, width = 9)


timein_age_cwd_mort_plot <- ggplot(df_out,aes(x=left_age_e,y=ID,color=cwd_mort))+
    geom_errorbarh(aes(xmin=left_age_e,xmax=right_age_r),size=1)+
    scale_color_manual("CWD Status\nat Mortlity",values=met.brewer("Java",3))+
    scale_x_continuous(breaks=c(0,52*(1:20)),labels=0:20)+
    xlab("Age (Year)")+
    ggtitle("Survival Data Collared Individuals - Age - CWD Status at Mortality")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_age_cwd_mort_plot
 
ggsave("figures/timein_age_cwd_mort_plot.png",timein_age_cwd_mort_plot,height = 6, width = 9)





timein_age_sex_plot <- ggplot(df_out,aes(x=left_age_e,y=ID,color=Sex))+
    geom_errorbarh(aes(xmin=left_age_e,xmax=right_age_r),size=1)+
    scale_color_manual(values=met.brewer("Troy",2))+
    scale_x_continuous(breaks=c(0,52*(1:20)),labels=0:20)+
    xlab("Age (Year)")+
    ggtitle("Survival Data Collared Individuals - Age - Sex")+
    theme_bw()+
    ylab("ID(n=1046)")+
    theme(axis.text.y=element_blank(),
           axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
timein_age_sex_plot
 
ggsave("figures/timein_age_status_plot.png",timein_age_sex_plot,height = 6, width = 9)



#############################################################
###
### Lexis Plane Plot
###
#############################################################

# devtools::install_github("ottlngr/LexisPlotR")
library(LexisPlotR)
library(gapminder)
df <- gapminder %>%
  filter(year %in% c(1952,2007)) %>%
  filter(continent %in% c("Asia")) %>%
  select(country,year,lifeExp, gdpPercap)%>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(year))
df

# df_out2 <- pivot_longer(df_out,c(ID))
# names(df_out2)

df_out <- d_surv
df_out <- df_out %>% arrange(left_age_e)
df_out$Sex <- as.factor(df_out$sex)
levels(df_out$Sex) <- c("Male","Female")
df_out$Status <- as.factor(df_out$censored)
levels(df_out$Status) <- c("Mortality","Censored")
df_out$ID=1:nrow(df_out)
df_out$cwd_mort[is.na(df_out$cwd_mort)] <- "Unknown"
df_out$cwd_cap <- as.factor(df_out$cwd_cap)
df_out$cwd_mort <- as.factor(df_out$cwd_mort)
levels(df_out$cwd_cap) <- c("Negative/Unknown","Positive")
levels(df_out$cwd_mort) <- c("Negative","Positive","Unknown")
lex_plot <- ggplot(df_out)+
    geom_segment(aes(x=left_period_e,y=left_age_e,xend=right_period_r,yend=right_age_r,color=Sex))+
    scale_y_continuous(breaks=c(0,52*(1:20)),labels=0:20)+
    ylab("Age (Year)")+
    scale_x_continuous(breaks=c(0,52,52*2,52*3,52*4,52*5),labels=paste0("Jan ",2017:2022))+
    xlab("Study Period (Year)")+
    ggtitle("Survival Data on Lexis Plane")+
    theme_bw()+
    theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            title = element_text(size=16))
lex_plot
ggsave("figures/lex_plot.png",lex_plot,height = 6, width = 9)
