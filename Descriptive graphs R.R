##############################################################################
##############                 INCENTIVES              #######################
##############            DESCRIPTIVE GRAPHS           #######################
##############################################################################
# Clear the memory
rm(list=ls()) 

library(gmodels)
library(foreign)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)
library(scales)
library(ggpubr)
library(tidyr)
library(reshape2)



setwd("/Users/cetre/Dropbox/Dossiers_partages/Incentives_shared/DataAnalysis/Data/GeneratDat/Session")
graph_path <-"/Users/cetre/Dropbox/Dossiers_partages/Incentives_shared/Writing"

############ 1. VISUALIZATION OF COMPREHENSION TESTS RESULTS ############
data <- read.dta("UseDat_Principals_v3_forR.dta")
attach(data)

# Create an overall comprehension test score 
# Number of questions Stakeholder: 3,3,6
# Number of questions Spectator: 2,2,4


# Stakeholder
stake <- subset(data,Treatment=="Stakeholder",select = c(subject_ID,Treatment,Num_False_PCT_1_1:Num_False_PCT_3_3) )
attach(stake)

score_trial1 <- (3-Num_False_PCT_1_1) + (3-Num_False_PCT_2_1) + (6-Num_False_PCT_3_1)
score_trial2 <- (3-Num_False_PCT_1_2) + (3-Num_False_PCT_2_2) + (6-Num_False_PCT_3_2)
score_trial3 <- (3-Num_False_PCT_1_3) + (3-Num_False_PCT_2_3) + (6-Num_False_PCT_3_3)
stake$score_trial1 <- score_trial1
stake$score_trial2<- score_trial2
stake$score_trial3 <- score_trial3

# Put data in long format : one line per trial and 3 variables (each CT)
stake_long <- gather(stake, trial, score, score_trial1:score_trial3, factor_key=TRUE)

# Rename variable "trial" for the legend
attach(stake_long)
trial2 <- NA
trial2[trial=="score_trial1"] <- "Score first trial"
trial2[trial=="score_trial2"] <- "Score second trial"
trial2[trial=="score_trial3"] <- "Score third trial"

stake_long$trial2 <- trial2

# Plot the distribution of scores over trials 
p_stake <- ggplot(stake_long, aes(x=score)) +
  geom_density(aes(group=factor(trial2), colour=factor(trial2), fill=factor(trial2)), alpha=0.3) +
  theme_gray() + theme(legend.title=element_blank()) + labs(x="Comprehension tests overall score") + ggtitle("Stakeholder") 

p_stake  

# Spectator
spect <- subset(data,Treatment=="Spectator",select = c(subject_ID,Treatment,Num_False_PCT_1_1:Num_False_PCT_3_3) )
attach(spect)

score_trial1 <- (2-Num_False_PCT_1_1) + (2-Num_False_PCT_2_1) + (4-Num_False_PCT_3_1)
score_trial2 <- (2-Num_False_PCT_1_2) + (2-Num_False_PCT_2_2) + (4-Num_False_PCT_3_2)
score_trial3 <- (2-Num_False_PCT_1_3) + (2-Num_False_PCT_2_3) + (4-Num_False_PCT_3_3)
spect$score_trial1 <- score_trial1
spect$score_trial2<- score_trial2
spect$score_trial3 <- score_trial3

# Put data in long format : one line per trial and 3 variables (each CT)
spect_long <- gather(spect, trial, score, score_trial1:score_trial3, factor_key=TRUE)

# Rename variable "trial" for the legend
attach(spect_long)
trial2 <- NA
trial2[trial=="score_trial1"] <- "Score first trial"
trial2[trial=="score_trial2"] <- "Score second trial"
trial2[trial=="score_trial3"] <- "Score third trial"

spect_long$trial2 <- trial2

# Plot the distribution of scores over trials 
p_spect <- ggplot(spect_long, aes(x=score)) +
  geom_density(aes(group=factor(trial2), colour=factor(trial2), fill=factor(trial2)), alpha=0.3) +
  theme_gray() + theme(legend.title=element_blank()) + labs(x="Comprehension tests overall score") + ggtitle("Spectator") 

p_spect

# Get the legend
legend <- get_legend(p_spect)


# Combine both plots
combined_nolegend <- plot_grid(p_stake + theme(legend.position="none"), p_spect + theme(legend.position="none"), ncol=1, align="v")
combined_with_legend<- plot_grid( combined_nolegend, legend, rel_widths = c(3,1))

# Save plot
ggsave(paste(graph_path, "Comp_test_princ.pdf", sep="/"),combined_with_legend, width=3, height=2, units="in", scale=2.5)


############ 2. VISUALIZATION OF CONTRACT TRADEOFF ############
data <- read.dta("UseDat_Principals_long_v6_forR.dta")
attach(data)

# nSanityChecks
data$dSanityCheck <- ifelse(dBel_high_Prod_A == 1,1,0) 
within(final, {data$nSanityCheck = ave(dSanityCheck,subject_ID,FUN=count)})
table(dSanityCheck)


# THEORY
# Show the variety of theory tradeoff
 
tradeoff_Th <- ggplot(data, aes(x=diffThProd, y=diffThIneq)) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point() + ggtitle("Contract tradeoffs assuming workers' best responses") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(graph_path, "contract_tradeoff_Th.pdf", sep="/"),tradeoff_Th, width=3, height=2, units="in", scale=2.5)

 
# BELIEF BASED 
# Show the variety of tradeoffs based on beliefs
ggplot(data, aes(x=diff_Prod_contr, y=diff_Ineq_contr)) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point() + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5))

# Showing the share of points without tradeoffs: 
# Output diff > 0 and ineq diff <0  ==> think efficient contract has low ineq
# OR Output diff < 0 and ineq diff >0 ==> think redistributive contract has high efficiency

no_tradeoff <- ifelse((diff_Prod_contr > 0 & diff_Ineq_contr <0) | 
                        (diff_Prod_contr < 0 & diff_Ineq_contr >0), "No tradeoff","Tradeoff between output and inequality")
table(no_tradeoff)
439/1369

tradeoff_bel <- ggplot(data, aes(x=diff_Prod_contr, y=diff_Ineq_contr,color=factor(no_tradeoff))) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point() + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank(),legend.position = c(0.8, 0.9))

ggsave(paste(graph_path, "contract_tradeoff_bel.pdf", sep="/"),tradeoff_bel, width=3, height=2, units="in", scale=2.5)

# This graph can be misleading because points overlay a lot
# Same graph but with dots reflecting the size of people

# For each tradeoff, compute the mean choice and graph this with a continuous color
# BALLOON PLOT
ggplot(data, aes(x=diff_Prod_contr, y=diff_Ineq_contr,color=factor(no_tradeoff))) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point(size = 0.5) + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum() + scale_size_continuous(range = c(1, 6))

# Plot additionnally the theoretical tradeoff dots
# Put data in long format so that two lines per subject: one theoretical and one belief tradeoff
long_temp <- melt(data, id.vars = c("subject_ID","Choice"),measure.vars=c("diff_Prod_contr", "diff_Ineq_contr", "diffThProd", "diffThIneq"),
                  variable.name="Diff_var",
                  value.name="value")

attach(long_temp)

# Create theory and belief variables to come back to a wide format based on this variable
theory_or_belief <- NA
theory_or_belief[Diff_var =="diff_Prod_contr"|Diff_var == "diff_Ineq_contr"] <- "Belief"
theory_or_belief[Diff_var =="diffThProd"|Diff_var == "diffThIneq"] <- "Theory"

long_temp$theory_or_belief <- theory_or_belief

# Create data for diff ineq or diff prod
type_of_diff <- NA
type_of_diff[Diff_var =="diff_Prod_contr"|Diff_var =="diffThProd"] <- "diffProd"
type_of_diff[Diff_var =="diff_Ineq_contr"|Diff_var == "diffThIneq"] <- "diffIneq"

long_temp$type_of_diff <- type_of_diff

long_temp <- long_temp[order(subject_ID,Choice,Diff_var),] 

# Reshape data to have two lines per subject: one theoretical and one belief tradeoff
final <- dcast(long_temp, subject_ID + Choice + theory_or_belief ~ type_of_diff, value.var="value")

attach(final)
# No tradeoff var
final$no_tradeoff <- ifelse(final$theory_or_belief=="Theory", "Tradeoff with best responses", 
                      ifelse((diffProd >0 & diffIneq <0 & final$theory_or_belief=="Belief") | (diffProd <0 & diffIneq >0 & final$theory_or_belief=="Belief"),"No tradeoff","Tradeoff between output and inequality"))

table(no_tradeoff,useNA="ifany")

# Plot data
belief <-subset(final,theory_or_belief=="Belief")
theory <-subset(final,theory_or_belief=="Theory")

# Belief plot
p <- ggplot(belief, aes(x=diffProd, y=diffIneq,color=factor(no_tradeoff))) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point(size = 0.5) + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum() + scale_size_continuous(range = c(1, 10))

# Add theoretical dots
tradeoff_graph <- p + geom_point(data=theory,  aes(x=diffProd, y=diffIneq, color=factor(no_tradeoff))) + 
  scale_color_manual(values=c("indianred2","seagreen3","black"))
  

ggsave(paste(graph_path, "contract_tradeoff.pdf", sep="/"),tradeoff_graph, width=4, height=2, units="in", scale=3)

##### SHOW THE DATA WITHOUT THE SANITY CHECKS AND TOO EXTREME BEHAVIOR ####

# Subset of data use in FMM
FMM <- subset(final,nSanityCheck<13 & abs(diffProd) <= 100)
belief <-subset(FMM,theory_or_belief=="Belief")
theory <-subset(FMM,theory_or_belief=="Theory")

table(FMM$diffProd)

# Plot on the subset of the data and keep the same scale as before
p <- ggplot(belief, aes(x=diffProd, y=diffIneq,color=factor(no_tradeoff))) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point(size = 0.5) + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum() + scale_size_continuous(range = c(1, 10)) + ylim(-40,120) + xlim(-250,230)

# Add theoretical dots
p + geom_point(data=theory,  aes(x=diffProd, y=diffIneq, color=factor(no_tradeoff))) + 
  scale_color_manual(values=c("indianred2","seagreen3","black"))






############ 3. STATED EFFORT AND EFFORT BELIEFS ##########

# PRINCIPALS ####
data <- read.dta("UseDat_Principals_effort_beliefs_forR.dta")
attach(data)

data$ability <- ifelse(grepl("p1",data$wage)==TRUE,"High ability","Low ability")

# names(data)[names(data) == 'wage'] <- 'wage_num'

W <- NA
W[grepl("w1",data$wage)==TRUE] <- 0.3
W[grepl("w2",data$wage)==TRUE]  <-0.35
W[grepl("w3",data$wage)==TRUE]  <-0.4 
W[grepl("w4",data$wage)==TRUE]  <-0.45
W[grepl("w5",data$wage)==TRUE]  <-0.5 
W[grepl("w6",data$wage)==TRUE]  <-0.55 
W[grepl("w7",data$wage)==TRUE]  <-0.6
W[grepl("w8",data$wage)==TRUE]  <-0.65
W[grepl("w9",data$wage)==TRUE]  <-0.7 
W[grepl("w10",data$wage)==TRUE]  <-0.75 

data$W <-W

table(W)

p <- ggplot(data, aes(x=W, y=belief, color=factor(ability), alpha=0.4)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Expected effort") +
  geom_point(size=0.5) + ggtitle("PRINCIPALS - Expected effort by ability types") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(aes(shape=ability))+ scale_size_continuous(range = c(1, 12)) 

# Fix the legend (remove alpha and give same legend for color and shape)
belief_effort <- p +  labs(color  = "", shape = "") +
  guides(alpha=FALSE) + scale_x_continuous(breaks = W)

ggsave(paste(graph_path, "belief_effort.pdf", sep="/"),belief_effort, width=4, height=2, units="in", scale=2)


# Separate the two graphs to see better
high_ability <-subset(data, ability=="High ability")
low_ability <-subset(data, ability=="Low ability")

low <- ggplot(low_ability, aes(x=W, y=belief)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Expected effort") +
  geom_point(size=0.5) + ggtitle("Low ability worker") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(color='darkslategray3') + scale_size_continuous(range = c(1, 12)) + scale_x_continuous(breaks = W)



high <- ggplot(high_ability, aes(x=W, y=belief)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Expected effort") +
  geom_point(size=0.5) + ggtitle("High ability worker") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(color='lightcoral')+ scale_size_continuous(range = c(1, 12)) + 
  scale_x_continuous(limits=c(0.3,0.75), breaks=W) + ylim(0,5)



combined <- plot_grid(high, low, ncol=1, align="v")

ggsave(paste(graph_path, "principal_effort_by_ability.pdf", sep="/"),combined, width=4, height=4, units="in", scale=2)


# Add the datapoints representing the best responses
BR_low  <- data.frame(W =c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.75), 
                      belief= c(1,1,1.5,1.5,2,2,2.5,2.5,3),
                      cond=c(replicate(9, "Best Response")))

BR_high  <- data.frame(W =c(0.4,0.5,0.55,0.6,0.65,0.70), 
                      belief= c(2.5,3,3.5,4,4.5,5),
                      cond=c(replicate(6, "Best Response")))


# Final graph
combined <- plot_grid(high + geom_point(data=BR_high, aes(colour =factor(cond))) +
                        scale_color_manual(values = c("Best Response" = "black")),
                      low + geom_point(data=BR_low, aes(colour =factor(cond))) +
                        scale_color_manual(values = c("Best Response" = "black")),
                      ncol=1, align="v") + ggtitle("Principals' beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=22))


ggsave(paste(graph_path, "principal_effort_by_ability_withBR.pdf", sep="/"),combined, width=4, height=4, units="in", scale=2)

# WORKERS #####
data <- read.dta("UseDat_Agents_forR.dta")
attach(data)
W <- NA
W[wage_num==1] <- 0.3
W[wage_num==2] <-0.35
W[wage_num==3] <-0.4 
W[wage_num==4] <-0.45
W[wage_num==5] <-0.5 
W[wage_num==6] <-0.55 
W[wage_num==7] <-0.6
W[wage_num==8] <-0.65
W[wage_num==9] <-0.7 
W[wage_num==10] <-0.75 
table(W,useNA="ifany")

data$W <-W

# Ability variable
data$ability <-ifelse(alpha==4,"Low ability","High ability")

p <- ggplot(data, aes(x=W, y=Effort_wage, color=factor(ability), alpha=0.4)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Stated effort") +
  geom_point(size=0.5) + ggtitle("WORKERS - Stated effort by ability types") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(aes(shape=ability))+ scale_size_continuous(range = c(1, 12)) 

# Fix the legend (remove alpha and give same legend for color and shape)
worker_effort <- p +  labs(color  = "", shape = "") +
   guides(alpha=FALSE) + scale_x_continuous(breaks = W)

ggsave(paste(graph_path, "worker_effort.pdf", sep="/"),worker_effort, width=4, height=2, units="in", scale=2)
 
# Graphs that separate high and low ability types to see better the differences
low_ability <-subset(data, alpha==4)
high_ability <-subset(data, alpha==6)

low_w <- ggplot(low_ability, aes(x=W, y=Effort_wage)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Stated effort") +
  geom_point(size=0.5) + ggtitle("Low ability worker") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(color='darkslategray3') + scale_size_continuous(range = c(1, 12))  + scale_x_continuous(breaks = W)

high_w <- ggplot(high_ability, aes(x=W, y=Effort_wage)) +theme_gray() + 
  labs(x="Piece-rate wage",
       y="Stated effort") +
  geom_point(size=0.5) + ggtitle("High ability worker") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  stat_sum(color='lightcoral')+ scale_size_continuous(range = c(1, 12))  + scale_x_continuous(breaks = W)


combined <- plot_grid(high_w, low_w, ncol=1, align="v")

ggsave(paste(graph_path, "worker_effort_by_ability.pdf", sep="/"),combined, width=4, height=4, units="in", scale=2)


# Adding best responses
BR_low  <- data.frame(W =seq(0.3,0.75,by=0.05), 
                      Effort_wage= c(1,1,1.5,1.5,2,2,2.5,2.5,3,3),
                      cond=c(replicate(10, "Best Response")))

# Wage 4 (0.45 case of two BR ==> 3 and 2.5)
BR_high  <- data.frame(W =c(seq(0.3,0.75,by=0.05),0.45), 
                       Effort_wage= c(1.5,2,2.5,3,3,3.5,4,4.5,5,5,2.5),
                       cond=c(replicate(11, "Best Response")))

# Final graph
combined_w <- plot_grid(high_w + geom_point(data=BR_high, aes(colour =factor(cond))) +
                        scale_color_manual(values = c("Best Response" = "black")),
                      low_w + geom_point(data=BR_low, aes(colour =factor(cond))) +
                        scale_color_manual(values = c("Best Response" = "black")),
                      ncol=1, align="v") + ggtitle("Workers effort choices") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=22))


ggsave(paste(graph_path, "worker_effort_by_ability_withBR.pdf", sep="/"),combined_w, width=4, height=4, units="in", scale=2)

# COMBINE WORKER AND PRINCIPAL GRAPHS ####

# Get the legend
legend <- get_legend(worker_effort)

# Combine both plots
combined_nolegend <- plot_grid(worker_effort + theme(legend.position="none"), belief_effort + theme(legend.position="none"), ncol=1, align="v")
combined_with_legend<- plot_grid( combined_nolegend, legend, rel_widths = c(3.5,1))

ggsave(paste(graph_path, "worker_effort_and_beliefs.pdf", sep="/"),combined_with_legend, width=4, height=4, units="in", scale=2)


# 2 by 2 graph
final <- plot_grid(combined_w,combined, ncol=2, align="v")

ggsave(paste(graph_path, "worker_effort_and_beliefs_2by2.pdf", sep="/"),final, width=5, height=4, units="in", scale=2.5)





########### 4. GRAPHICAL REPRESENTATION OF PRINCIPALS' CHOICES #######
data <- read.dta("UseDat_Principals_long_v6_forR.dta")
attach(data)

# BALOON PLOT WITH COLORS ####
# For each dot on the contract tradeoff graph, show the share
# of people choosing the egalitarian contract by tradeoff
means <- ddply(data, .(diff_Prod_contr, diff_Ineq_contr,Treatment), 
               summarise,mean=mean(yA), N=length(yA))

ggplot(means, aes(x=diff_Prod_contr, y=diff_Ineq_contr,color=mean, size = N)) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point() + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  scale_size_continuous(range = c(1, 10))

  
# Zooming over the reasonable tradeoff
# Removing tradeoffs below -100 and above 200 in terms of output beliefs
attach(means)
means$my_tradeoffs <- ifelse(diff_Prod_contr>-100 & diff_Prod_contr <100,1,0)
my_tradeoffs <- subset(means,my_tradeoffs==1 & Treatment == "Stakeholder")

ggplot(my_tradeoffs, aes(x=diff_Prod_contr, y=diff_Ineq_contr,color=mean, size = N)) +theme_gray() + 
  labs(x="Output contract B - Output contract A",
       y="Inequality contract B - Inequality contract A") +
  geom_point() + ggtitle("Contract tradeoffs based on Principal's beliefs") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) +
  scale_size_continuous(range = c(1, 10))

# REPRODUCTION STATA GRAPH WITH CI BY TREATMENT ####
# Collapse all tradeoff below -10 as being "tradeoff below - 10"
data <- read.dta("UseDat_Principals_long_v6_forR.dta")
attach(data)
tradeoffs <- data$diff_Prod_contr
tradeoffs[data$diff_Prod_contr < -10] <- "Below -10"
tradeoffs[data$diff_Prod_contr >60] <- "Above 60"

table(tradeoffs)
# Reorder the x axis
tradeoffs  <- factor(tradeoffs, levels=c("Below -10",seq(-10,60,by=10),"Above 60"))

data$tradeoffs <- tradeoffs

# But focus on the egalitarian contract (clearer because sometimes yB is not efficient)
means.sem <- ddply(data, .(tradeoffs,Treatment), summarise,
                   mean=mean(yB), sem=sd(yB)/sqrt(length(yB)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

choice_bel <-  ggplot(means.sem, aes(x=tradeoffs,y=mean,color=factor(Treatment),group=Treatment))+
  geom_point() + geom_line(aes(linetype=Treatment),size = 1)+ geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6)  +
  labs(y="Share of contract B chosen",x = "Difference in Output Contract B - Contract A", color="Treatment groups")  +
  scale_color_manual(name ="",values=c("deepskyblue1", "deepskyblue4")) + theme_gray() +
  labs(color  = "", linetype = "") + ggtitle("Belief-based Tradeoffs") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1)

ggsave(paste(graph_path, "choiceBEL_bytreat.pdf", sep="/"),choice_bel, width=2, height=1, units="in", scale=4)

# THEORETICAL TRADEOFFS
attach(data)
means.sem <- ddply(data, .(diffThProd,Treatment), summarise,
                   mean=mean(yB), sem=sd(yB)/sqrt(length(yB)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

choice_th <- ggplot(means.sem, aes(x=diffThProd,y=mean,color=factor(Treatment),group=Treatment))+
  geom_point() + geom_line(aes(linetype=Treatment),size = 1)+ geom_errorbar(aes(ymax =upper, ymin=lower), width=1, alpha=0.6)  +
  labs(y="Share of contract B chosen",x = "Difference in Output Contract B - Contract A", color="Treatment groups")  +
  scale_color_manual(name ="",values=c("deepskyblue1", "deepskyblue4")) + theme_gray() +
  labs(color  = "", linetype = "")  + ggtitle("Theoretical tradeoffs") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1)


ggsave(paste(graph_path, "choiceTH_bytreat.pdf", sep="/"),choice_th, width=2, height=1, units="in", scale=4)


# COMBINE BELIEFS AND THEORETICAL GRAPHS
# Get the legend
legend <- get_legend(choice_th)

# Combine both plots
combined_nolegend <- plot_grid(choice_th + theme(legend.position="none"), choice_bel + theme(legend.position="none"), ncol=1, align="v")
combined <- plot_grid( combined_nolegend, legend, rel_widths = c(7,1.5))

ggsave(paste(graph_path, "choiceTH_BEL_bytreat.pdf", sep="/"),combined, width=2, height=2, units="in", scale=4)

# REPRODUCTION STATA GRAPH WITH CI BY TREATMENT - EGALITARIAN ####
# Collapse all tradeoff below -10 as being "tradeoff below - 10"
data <- read.dta("UseDat_Principals_long_v6_forR.dta")
attach(data)
tradeoffs <- data$diff_Prod_contr
tradeoffs[data$diff_Prod_contr < -10] <- "Below -10"
tradeoffs[data$diff_Prod_contr >60] <- "Above 60"

table(tradeoffs)
# Reorder the x axis
tradeoffs  <- factor(tradeoffs, levels=c("Below -10",seq(-10,60,by=10),"Above 60"))

data$tradeoffs <- tradeoffs

# But focus on the egalitarian contract (clearer because sometimes yB is not efficient)
means.sem <- ddply(data, .(tradeoffs,Treatment), summarise,
                   mean=mean(yA), sem=sd(yA)/sqrt(length(yA)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

choice_bel <-  ggplot(means.sem, aes(x=tradeoffs,y=mean,color=factor(Treatment),group=Treatment))+
  geom_point() + geom_line(aes(linetype=Treatment),size = 1)+ geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6)  +
  labs(y="Share of contract A (egalitarian contract) chosen",x = "Difference in Output Contract B - Contract A", color="Treatment groups")  +
  scale_color_manual(name ="",values=c("deepskyblue1", "deepskyblue4")) + theme_gray() +
  labs(color  = "", linetype = "") + ggtitle("Belief-based Tradeoffs") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1)


ggsave(paste(graph_path, "choiceBEL_bytreat_egal.pdf", sep="/"),choice_bel, width=2, height=1, units="in", scale=4)

# THEORETICAL TRADEOFFS
attach(data)
means.sem <- ddply(data, .(diffThProd,Treatment), summarise,
                   mean=mean(yA), sem=sd(yA)/sqrt(length(yA)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

choice_th <- ggplot(means.sem, aes(x=diffThProd,y=mean,color=factor(Treatment),group=Treatment))+
  geom_point() + geom_line(aes(linetype=Treatment),size = 1)+ geom_errorbar(aes(ymax =upper, ymin=lower), width=1, alpha=0.6)  +
  labs(y="Share of contract A (egalitarian contract) chosen",x = "Difference in Output Contract B - Contract A", color="Treatment groups")  +
  scale_color_manual(name ="",values=c("deepskyblue1", "deepskyblue4")) + theme_gray() +
  labs(color  = "", linetype = "")  + ggtitle("Theoretical tradeoffs") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1)


ggsave(paste(graph_path, "choiceTH_bytreat_egal.pdf", sep="/"),choice_th, width=2, height=1, units="in", scale=4)


# COMBINE BELIEFS AND THEORETICAL GRAPHS
# Get the legend
legend <- get_legend(choice_th)

# Combine both plots
combined_nolegend <- plot_grid(choice_th + theme(legend.position="none"), choice_bel + theme(legend.position="none"), ncol=1, align="v")
combined <- plot_grid( combined_nolegend, legend, rel_widths = c(7,1.5))

ggsave(paste(graph_path, "choiceTH_BEL_bytreat_egal.pdf", sep="/"),combined, width=2, height=2, units="in", scale=4)


