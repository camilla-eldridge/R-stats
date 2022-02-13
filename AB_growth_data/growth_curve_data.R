library(ggplot2)


# 3 different ab concentrations (1 Independant variable with 3 levels) #

# Read in csv files and keep headers #
CSV_2<-read.csv("Manual GC MH R 2.csv", header = TRUE)
CSV_0<-read.csv("Manual GC MH R WT.csv", header = TRUE)
CSV_20<-read.csv("Manual GC MH R 20.csv", header = TRUE)
#names(CSV_FILE) # lets you check headers - i.e 4 check that columns are separated

# Get mean of each row #
WT<-rowMeans(CSV_0[,c(2,3,4,5,6)])
Two_mgL<-rowMeans(CSV_2[,c(2,3,4,5,6)])
twenty_mgL<-rowMeans(CSV_20[,c(2,3,4,5,6)])

# plot all initial data to compare row averages #

# First transform data FORMAT for ggplot 
# take one input file to get time column #
t<-CSV_0[1] ###column for time

# change into ggplot format #
A<-cbind(t, WT)
B<-cbind(t, Two_mgL)
C<-cbind(t, twenty_mgL)

wt<-rep("WT", length(WT))
wt_comb<-cbind(wt, A)
colnames(wt_comb) <- c("Strain", "Time(hours)", "N")

tw<-rep("2mgL", length(Two_mgL))  #len will be same as each colomn
tw_comb<-cbind(tw, B)
colnames(tw_comb) <- c("Strain", "Time(hours)", "N")

twnt<-rep("20mgL", length(twenty_mgL))
twnt_comb<-cbind(twnt, C)
colnames(twnt_comb) <- c("Strain", "Time(hours)", "N")

Averages<-data.frame(rbind(wt_comb, tw_comb, twnt_comb))

# plot averages #

T<-ggplot() +
  geom_line(data = Averages, aes(x = Averages$Time.hours. , y = Averages$N, group = Averages$Strain, colour=Averages$Strain)) + theme_bw() + ylab(label="Average Log10 CFU/mL") + xlab(label = "Time (hours)") +
        theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
        theme(panel.border= element_blank()) + labs(title="12 hour growth curve") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.key = element_blank()) + 
        theme(legend.title=element_blank()) + theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

T

#use tapply(response,factor,function-name)

#   SUMMARY STATISTICS #
# mean for each group #
tapply(Averages$N, Averages$Strain, mean)
# variance for each group #
tapply(Averages$N, Averages$Strain, var)
# length of data set for each group #
tapply(Averages$N, Averages$Strain, length)

#  Boxplot  #
plot(N~Strain, data = Averages)

#or in ggplot
p10 <- ggplot(Averages, aes(x = Averages$Strain, y = Averages$N, fill = Averages$Strain)) +
  geom_boxplot() + scale_x_discrete(name = "") + scale_y_continuous(name = "")  +  theme(legend.title=element_blank()) + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

p10 + scale_fill_manual(values=c("pink", "light blue", "yellow")) + theme(legend.position="none")

# ONE WAY ANOVA #
# aov(response  ~ factor, data=data_name)  
# Factor is the variable that separates the data into groups
one_way_anova<-aov(N~Strain, data = Averages)
summary(one_way_anova)

# Tukey test #
TukeyHSD(one_way_anova, conf.level = 0.95)

#The function pairwise.t.testcomputes the pair
#-wise comparisons between group means with corrections for multiple testing
# response here is a vector of observations, factor a list of factors
pairwise.t.test(Averages$N, Averages$Strain, p.adjust="bonferroni")

# notes #
#facet_wrap(~Strains)+ for individual plots for each strain
