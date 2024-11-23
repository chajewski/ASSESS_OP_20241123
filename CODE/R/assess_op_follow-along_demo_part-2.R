# --------------------------------------------------------------------- #
#                                                                       #
# TITLE:       ASSESSMENT OPERATIONALIZATION WORKSHOP                   #
# TASK:        Follow-along demonstrations in R                         #
#                                                                       #
# SECTION:     Part 2: Scaling and field-test calibration               #
#                                                                       #
# --------------------------------------------------------------------- #
#                                                                       #
# AUTHOR:      Michael Chajewski                                        #
# CREATED:     November 23, 2024                                        #
#                                                                       #
# DESCRIPTION: The workshop introduces attendees to the sequential      #
#              decisions required to develop an assessment and provides #
#              an opportunity to explore a selection of standard        #
#              activities that would be supported by operational        #
#              psychometricians. Using assessment data, attendees will  #
#              process and analyze various solutions in R, learning how #
#              psychometric deliverables are negotiated, and how        #
#              assessment measurement expertise supports client and     #
#              policy decisions.                                        #
#                                                                       #
# --------------------------------------------------------------------- #
#                                                                       #
# DISCLAIMER:  No proprietary operational student, performance or item  #
#              data has been utilized in the data and analyses prepared #
#              for this workshop. Further, all Texas Education Agency   #
#              (TEA) materials provided as examples are publicly        #
#              available on TEA's website at https://tea.texas.gov/     #
#                                                                       #
# --------------------------------------------------------------------- #

# ---------------
# Setup workspace
# ---------------

setwd("C:/.../")

# ----------- #
#   SCALING   #
# ----------- #

# -- Creating the Incomplete Data Matrix (IDM)--

# Import data
load("DATA/assess_op_demo_R_objects")

# Item scores & distributions
names(item_scores)
names(item_scores_stats)
item_scores_stats$Variable

# Student level performance data & distributions
names(student_data)
names(student_data_stats)
student_data_stats$Variable

# Review:
# https://www.winsteps.com/winman/index.htm
# CODE -> WINSTEPS CALIBRATIONS -> SCALING
# ANALYSES -> PART 2 -> SCALING

# -- Creating the RST --

# Review:
# CODE -> WINSTEPS CALIBRATIONS -> RST
# ANALYSES -> PART 2 -> RST

# Import RST
rst <- read.csv(paste("ANALYSES/PART 2/RST/rasch_demo_rst.csv",sep=""), skip=2, header = TRUE)

# Correct raw score
rst$SCORE <- rst$SCORE+2

# Quick (marginal) reliability check
(var(rep(rst$MEASURE,rst$FREQ))-mean(rep(rst$S.E.,rst$FREQ)^2))/var(rep(rst$MEASURE,rst$FREQ))

# -- Creating scaling constants --

# Review:
# TEA_STAAR_Grade-3-8-Standard-Setting-Report_2023.pdf

# Raw score cuts from standard setting
appcut <- 12
procut <- 20
adpcut <- 32

# Pre-defined scale score cuts
appss <- 80
pross <- 100
adpss <- c()

# Establish scaling constants via linear interpolation
usea <- round((pross-appss)/(rst$MEASURE[which(rst$SCORE==procut)]-rst$MEASURE[which(rst$SCORE==appcut)]),4)
useb <- round(pross-(rst$MEASURE[which(rst$SCORE==procut)]*usea),4)

# Interpolate advanced proficient SS
adpss <- round((usea*rst$MEASURE[which(rst$SCORE==adpcut)])+useb,0)

# -- Create RSSS --

# Compute (rounded and adjusted) scale scores
ss <- round((rst$MEASURE*usea)+useb,4)

# Replace observed advanced proficient SS
# with extrapolated cut
ss[which(rst$SCORE==adpcut)] <- adpss

# Rounded SS
rss <- round(ss,0)

# Create scoring table
RSSS <- data.frame("Subject"="Social Studies",
                      "Grade"='8',
                      "RawScore"=rst$SCORE,
                      "Count"=rst$FREQ,
                      "ThetaScore"=round(rst$MEASURE,4),
                      "ThetaScoreSE"=round(rst$S.E.,4),
                      "UnroundedScaleScore"=ss,
                      "UnroundedScaleScoreSE"=round(rst$S.E.*usea,4),
                      "ScaleScore"=rss,
                      "ScaleScoreSE"=round(rst$S.E.*usea,0),
                      "PerformanceLevel"=ifelse(rss<appss,1,ifelse(rss>=appss & rss<pross,2,ifelse(rss>=pross & rss<adpss,3,ifelse(rss>=adpss,4,-99)))),
                      "PerformanceLabel"=factor(ifelse(rss<appss,"Below Proficient",ifelse(rss>=appss & rss<pross,"Approaching Proficient",ifelse(rss>=pross & rss<adpss,"Proficient",ifelse(rss>=adpss,"Advanced Proficient",-99)))),levels=c("Below Proficient","Approaching Proficient","Proficient","Advanced Proficient")),
                      "Percentile"=round(rst$CUM.FREQ/max(rst$CUM.FREQ)*100,0))

# Impact data
by(RSSS, RSSS$PerformanceLabel, function(x){round(sum(x$Count)/sum(RSSS$Count)*100,2)})

# Explore the score relations--including the
# end-points to determine LOSS/HOSS.

# ----------------------- #
# FIELD-TEST CALIBRATIONS #
# ----------------------- #

# Review: 
# CODE -> WINSTEPS CALIBRATIONS -> FT
# ANALYSES -> PART 2 -> FT

# ---------------------- #
# TC STATISTICAL TARGETS #
# ---------------------- #

# Import TCC
tcc <- read.csv(paste("ANALYSES/PART 2/RST/rasch_demo_rst_tcc.csv",sep=""), skip=1, header = TRUE)

# Subset items to only include OP
OP_items <- item_metadata[which(item_metadata$Status=="OP"),]

# Number of operational items
nOP <- dim(OP_items)[1]

# Review item difficulties
rids <- OP_items$RID_op
summary(rids)
by(rids,OP_items$Type,summary)

# Determine empirical average form difficulty
# interval size to construct target ranges
# based on +/-1 rs pt form alignment to measurement scale

int_avgrid_1pt <- (RSSS$ThetaScore[which(abs(mean(rids)-RSSS$ThetaScore)==min(abs(mean(rids)-RSSS$ThetaScore)))+1]-
                     RSSS$ThetaScore[which(abs(mean(rids)-RSSS$ThetaScore)==min(abs(mean(rids)-RSSS$ThetaScore)))-1])/2

int_avg_cut1pt <-  (mean(RSSS$ThetaScore[match(c(appss,pross,adpss),RSSS$ScaleScore)+1]-
                       RSSS$ThetaScore[match(c(appss,pross,adpss),RSSS$ScaleScore)-1]))/2

# Overall form target
target_low <- round(mean(rids)-int_avgrid_1pt,4)
target_high <- round(mean(rids)+int_avgrid_1pt,4)

# To maintain similar measurement alignments in future forms
# targets with respect to Fisherian information at or near
# the cut scores would assure a stable raw score cut
# year-over-year.









