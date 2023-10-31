library('dplyr')
library('REdaS')
library('FSA')
library('ggplot2')
library('rstatix')
library('ggpubr')
library('car')

df <- read.csv('intData_hri.csv')


df$Condition <- as.factor(df$Condition)
df$Condition = factor(df$Condition, labels = c('Full Control', 'Partial Control', 'No Control'))

class(df$Condition)

new <- data.frame(Condition = c('Full Control','Full Control','Full Control',
                                'Partial Control','Partial Control','Partial Control',
                                'No Control','No Control','No Control'),
                  Participant = c(1,3,9,
                                  2,5,8,
                                  4,6,7),
                  NumExhibits = c(8,10,5,
                                  8,10,10,
                                  8,7,7))

temp <- data.frame(Condition = c('Full Control','Full Control','Full Control',
                                'Partial Control','Partial Control','Partial Control',
                                'No Control','No Control','No Control'),
                  Participant = c(1,3,9,
                                  2,5,8,
                                  4,6,7),
                  VR = c(1,2,4,
                         1,2,4,
                         1,3,4))
group_by(temp, Condition) %>%
  summarise(
    Count = n(),
    Mean = mean(VR), SD = sd(VR),
    Median = median(VR, na.rm = TRUE),
    InterquartileRange = IQR(VR, na.rm = TRUE)
  )
 

new$Condition <- as.factor(new$Condition)
new$Condition = factor(new$Condition, labels = c('Full Control', 'No Control', 'Partial Control'))
class(new$Condition)

Full <- subset(df, Condition == 'Full Control')
Partial <- subset(df, Condition == 'Partial Control')
No <- subset(df, Condition == 'No Control')

qqnorm(Full$ListenedTime)
qqline(Full$ListenedTime)

qqnorm(Partial$ListenedTime)
qqline(Partial$ListenedTime)

qqnorm(No$ListenedTime)
qqline(No$ListenedTime)

#df$lp <- abs(median(df$ListenedPercentage)-df$ListenedPercentage)

bartlett.test(data=df, ListenedTime ~ Condition)
bartlett.test(data=df, ListenedPercentage ~ Condition)
bartlett.test(data=new, NumExhibits ~ Condition)

fligner.test(data=df, ListenedTime ~ Condition)
fligner.test(data=df, ListenedPercentage ~ Condition)
fligner.test(data=new, NumExhibits ~ Condition)

kruskal_lt <- kruskal_test(data=df, ListenedTime ~ Condition)
kruskal_lp <- kruskal_test(data=df, ListenedPercentage ~ Condition)
#kruskal_ne <- 
  kruskal_test(data=new, NumExhibits ~ Condition)


dunn_lt <- dunnTest(data=df, ListenedTime ~ Condition, method='bonferroni')
dunn_lp <- dunnTest(data=df, ListenedPercentage ~ Condition, method='bonferroni')
dunn_ne <- dunnTest(data=new, NumExhibits ~ Condition, method='bonferroni')


kruskal_lt

par(bg = 'white')
#boxplot_lt <-
  ggplot(df, aes(x=Condition, y=ListenedTime)) +
    geom_boxplot(fill=c('#a22c29','#3066be','#e18335'), colour='black') +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,70) , n = 7) ) +
    theme_bw() +
    #labs(subtitle=get_test_label(kruskal_lt, detailed = TRUE)) +
    xlab('Conditions') + ylab('Listened Time (s)') 

#boxplot_lp <-
  ggplot(df, aes(x=Condition, y=ListenedPercentage)) +
    geom_boxplot(fill=c('#a22c29','#3066be','#e18335'), colour='black') +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,100) , n = 10) ) +
    theme_bw() +
    #labs(subtitle=get_test_label(kruskal_lp, detailed = TRUE)) +
    xlab('Conditions') + ylab('Listened Percentage (%)')

#boxplot_ne <-
  ggplot(new, aes(x=Condition, y=NumExhibits)) +
    geom_boxplot(fill=c('#a22c29','#3066be','#e18335'), colour='black') +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,10) , n = 10) ) +
    theme_bw() + 
    labs(subtitle=get_test_label(kruskal_ne, detailed = TRUE)) +
    xlab('Conditions') + ylab('Number of Exhibits visited')




summary_lt <- group_by(df, Condition) %>%
  summarise(
    Count = n(),
    Mean = mean(ListenedTime), SD = sd(ListenedTime),
    Median = median(ListenedTime, na.rm = TRUE),
    InterquartileRange = IQR(ListenedTime, na.rm = TRUE)
  )

summary_lp <- group_by(df, Condition) %>%
  summarise(
    Count = n(),
    Mean = mean(ListenedPercentage), SD = sd(ListenedPercentage),
    Median = median(ListenedPercentage, na.rm = TRUE),
    InterquartileRange = IQR(ListenedPercentage, na.rm = TRUE)
  )

summary_ne <- group_by(new, Condition) %>%
  summarise(
    Count = n(),
    Mean = mean(NumExhibits), SD = sd(NumExhibits),
    Median = median(NumExhibits, na.rm = TRUE),
    InterquartileRange = IQR(NumExhibits, na.rm = TRUE)
  )

#barplot_lt <-
  ggplot(summary_lt, aes(x=Condition, y=Mean)) +
    geom_bar(fill=c('#a22c29','#3066be','#e18335'), stat = "identity") +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,70) , n = 7) ) + 
    theme_bw() +
    #labs(subtitle=get_test_label(kruskal_ne, detailed = TRUE)) +
    xlab('Conditions') + ylab('Listened Time (s)')

#barplot_lp <-  
  ggplot(summary_lp, aes(x=Condition, y=Mean)) +
    geom_bar(fill=c('#a22c29','#3066be','#e18335'), stat = "identity") +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,100) , n = 10) ) +
    theme_bw() +
    #labs(subtitle=get_test_label(kruskal_ne, detailed = TRUE)) +
    xlab('Conditions') + ylab('Listened Percentage (%)')
  
#barplot_ne <-
  ggplot(summary_ne, aes(x=Condition, y=Mean)) +
    geom_bar(fill=c('#a22c29','#3066be','#e18335'), stat = "identity") +
    scale_x_discrete() + scale_y_continuous(breaks = pretty( c(0,10) , n = 10) ) +
    theme_bw() +
    labs(subtitle=get_test_label(kruskal_ne, detailed = TRUE)) +
    xlab('Conditions') + ylab('Number of Exhibits visited')



dunn_ne
kruskal_ne
