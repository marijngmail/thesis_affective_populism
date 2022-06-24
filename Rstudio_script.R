#code from Rstudio used to performs regression, distribution, and clean data
#creating new dataframe
df<- dfs_sentiment_compound[ , c('UserScreenName', 'Embedded_text', 'Comments', 'Likes', 'Retweets', 'forVader', 'compound', 'neg', 'neu', 'pos')]

#creating numeric values
df$Retweets <- as.numeric(gsub(",","",df$Retweets))
df$Comments <- as.numeric(gsub(",","",df$Comments))
df$Likes <- as.numeric(gsub(",","",df$Likes))

#create sentiment variables
df$sentiment <- df$neg + df$pos

#Pearson correlation
cor(df[, c('Comments', 'Likes', 'Retweets', 'compound')], use = "complete.obs")

#get subsets individual politicians
Azarkan <- subset(df, UserScreenName=="Farid Azarkan")

#deleting outliers Retweets per politician using intequertile method
Q <- quantile(Haan$Retweets, probs=c(.25, .75), na.rm = TRUE) #change per politician to delete outliers per politician

iqr <- IQR(Haan$Retweets, na.rm= TRUE)

elim_Haan<- subset(Haan, #change per politician to delete outliers per politician
                   Haan$Retweets > (Q[1] - 1.5*iqr) 
                   & Haan$Retweets < (Q[2]+1.5*iqr))

#standardising the values
df_Wilders <- elim_Wilders %>% mutate_each_(list(~scale(.) %>% as.vector)
                                            ,vars = c("Comments","Likes", 
                                                      "Retweets"))

#summary statistics
describe() #fill in name of data set to get the data on the outliers

#regression per pol (change data set to get new regression)
df_Wilders.totsent.lm <- lm(Retweets ~ totsentiment, 
                            data = df_Wilders)
summary(df_Wilders.totsent.lm)

#regression whole (change sentiment to neg for H2)
df.resamp.retweets.lm <- lm(Retweets ~ sentiment, 
                            data = resampled_df)
summary(df.resamp.retweets.lm)

#visualisations used
g1 <-ggplot(resampled_df, 
            aes(x = jitter(neg),    
                y = Retweets)) +  
  geom_point(alpha = .5) +       
  geom_smooth(method = 'lm') +   
  theme_pubclean() +
  labs(x = "Negativity",
       y = "Retweets")

g2 <-ggplot(df_Wilders, 
            aes(x = jitter(neg),    
                y = Retweets)) +  
  geom_point(alpha = .5) +       
  geom_smooth(method = 'lm') +   
  theme_pubclean() +
  labs(x = "Negativity",
       y = "Retweets in standard deviations")

g3 <- ggplot(df_Baudet, 
             aes(x = jitter(neg),    
                 y = Retweets)) +  
  geom_point(alpha = .5) +       
  geom_smooth(method = 'lm') +   
  theme_pubclean() +
  labs(x = "Negativity",
       y = "Retweets in standard deviations")

d1 <- ggplot(data = dfs_elim, 
             mapping = aes(x = vader_label, 
                           fill=vader_label,))+ 
  geom_bar()+
  theme_pubclean() +
  labs(y = "Tweets",
       x= NULL,
       fill= "Sentiment label")+
  theme(legend.position="none")+
  scale_fill_manual(values=c('royalblue4', 'royalblue', 'royalblue1'))

d2 <- ggplot(dfs_elim, 
             aes(x=UserScreenName, 
                 y=compound)) + 
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", color="blue")+
  theme_pubclean() +
  labs(y = "Sentiment compound",
       x= NULL)+
  theme(legend.position="none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_fill_brewer(palette="Dark2")

d3 <- ggplot(dfs_elim, 
             aes(x=UserScreenName, 
                 y=Retweets)) + 
  geom_boxplot(coef = Inf,color = 'blue') +
  theme_pubclean() +
  labs(y = "Retweets",
       x= NULL)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))