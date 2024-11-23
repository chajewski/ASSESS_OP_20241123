# --------------------------------------------------------------------- #
#                                                                       #
# TITLE:       ASSESSMENT OPERATIONALIZATION WORKSHOP                   #
# TASK:        Follow-along demonstrations in R                         #
#                                                                       #
# SECTION:     Part 1: Blueprint, assessment, and field-test plan       #
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

# --------------------- #
# BLUEPRINT DEVELOPMENT #
# --------------------- #

# -- Item types & scoring --

# Learn about asset development standards
# such as QTI at https://www.imsglobal.org/home

# Based on process and cognitive load certain
# item types are likely to associate with specific
# points. The client and Content development (CD)
# will provide the minimum expected mix of types.

# For the demo test CD suggested that no more than
# 10% of the items should be non-MCQ, and that the
# assessment will have to include one short constructed
# response item (SCR).

# Constructed response (CR) often have item scoring
# rubrics developed against the construct performance
# level (PL) descriptors (PLDs).

# Review:
# TEA_STAAR-Grade-8-Social-Studies-PLDs.pdf

# For the demo test the client has adopted 4
# PLs: (1) Below Proficient
#      (2) Approaching Proficient
#      (3) Proficient
#      (4) Advanced Proficient

# The ECR item type was concluded to be
# scored holistically 1-4 (aligned with the PLDs),
# by two raters for a (summed) item score 
# ranging from 2 to 8.

# All MCQ will garner 1 point, while Technology
# Enhanced Items (TEIs) will garner a maximum
# of 2 points.


# -- Number of items --

# Review:
# TX-state-and-federal-required-tests-march-2022.pdf
# TEA_STAAR-8-Social-Studies-Assessed-Curriculum.pdf

# CD would provide information regarding
# how standards relate to the construct.

# For the demo test CD suggested that RC1 and RC2
# cover the construct about equally, while RC3 and
# RC4 standards account for 23% and 15% respectively.

rcpct <- data.frame(RC=paste0("RC",1:4),
                    Pct=c((100-23-15)/2,(100-23-15)/2,23,15))
sum(rcpct$Pct)

# Additionally the RCs have a student expectation (SE)
# type breakdown as follows:

rcpct <- data.frame(rcpct,
                    "Readiness"=c(8,8,6,1),
                    "Supporting"=c(15,16,12,0))
rcpct <- data.frame(rcpct,"SEs"=rcpct$Readiness+rcpct$Supporting)

# Because of the nature of RC4, the SCR item
# will be used to measure the corresponding
# readiness SE. The SCR item counts as a non-MCQ.

# Ideally an assessment provides an interaction
# opportunity for each readiness standard.

readi_cnt <- sum(rcpct$Readiness)

# Where any one form of the assessment should
# cover about 50% of all SEs.

target_items <- sum(rcpct$SEs*.50)

# We would therefore allow a range of items
# in each RC (with supporting SEs) so as to
# allow additional standards to be evaluated.
# The additional items are typically distributed
# as a function of the RC construct representation.

# Additional supporting SE items on the form
# to be distributed between RC1-RC3 (RC4 has
# no supporting standards)

needed_items <- target_items-readi_cnt

# RC4 aside, typically only one RC on a form 
# would be allowed to only assess readiness SEs.
# Thus the remaining items would need to be
# distributed among other RCs. In such a case
# the remaining required items could be split
# evenly.

rcpct <- data.frame(rcpct,
                    Item_min=rcpct$Readiness,
                    Item_max=c(rcpct$Readiness[-4]+needed_items/2,1))

# Other methods use construct representation
# permissible variability to inform RC item ranges.


# -- Score Points --

# Since no more than 10% of items are to be non-MCQ,
# and the SCR item already counts toward this total,
# only 2 TEIs will be permissible on a form. Therefore,
# the test total raw score range is given by the sum
# across possible items poionts.

# (MCQs * min points for MCQ) + (TEIs * min points for TEI) + (SCRs * min points SCR)
min_rs <- (target_items-2-1)*0+2*0+1*2

# (MCQs * max points for MCQ) + (TEIs * max points for TEI) + (SCRs * max points SCR)
max_rs <- (target_items-2-1)*1+2*2+1*8

# With max_rs-min_rs+1 possible scores...
length(min_rs:max_rs)

# ...and a healthy item to possible scores ratio.
# Ratios of > 1.5 should be avoided, as well as
# items (interactions) that generate more than
# 10% of the possible raw scores.

(max_rs-min_rs)/target_items # Overall

(8-2)/(max_rs-min_rs) # SCR
2/(max_rs-min_rs)     # TEIs


# -- Reliability -- #

# Without performance data, pre-administration
# reliability estimates are largely speculative.
# Historic or comparable assessment analyses can
# be used to provide directional evidence.

# Review:
# TEA_STAAR-Grade-8-Social-Studies-Blueprint_2022-2023.pdf
# TEA_2022-2023-Technical-Digest.pdf (see p. 246)

# Spearman-Brown is often used with historical or
# comparable assessment data to confirm proposed blueprint.

sb_r2 <- function(old_n, new_n, old_rel){(new_n/old_n * old_rel)/(1+((new_n/old_n)-1) * old_rel)}

sb_r2(49, max_rs-min_rs, .89) # Based on points
sb_r2(40, target_items, .89)  # Based on items

# Item reduction exploration
cbind(readi_cnt:target_items,sb_r2(40, readi_cnt:target_items, .89))

# Various IRT reliability estimates are also
# used to anticipate/explore target measurement
# scale properties.

# I.e. marginal reliability; see Kim(2011)
mar_rel <- function(var_th, avg_csem2){(var_th-avg_csem2)/var_th}

# Fixing variance of latent trait 
mar_rel(11,1)
mar_rel(5,.40)

# --------------- #
# ASSESSMENT PLAN #
# --------------- #

# -- Testing time --

# If possible review historic student item interactions
# within subject x grade by type (or other meaningful
# classification granularity).

# Sadly much of timing data is not published.
# Content and curriculum specialists will likely know
# a rough approximation for well established item types.

# Review:
# https://www.peardeck.com/blog/how-to-determine-the-best-length-for-your-assessment#:~:text=Kansas%20Curriculum%20Center%E2%80%99s%20David%20Clay%20suggests%3A%201%2030,5%20to%2010%20minutes%20to%20review%20the%20work
# TEA_STAAR_2025-Test-Administrator-Manual.pdf

# "Typical" test duration approximations are based
# on point estimates from average item type response
# time, where MCQ ~ 1 min, some TEIs ~ 1.5 min, and
# SCR ~ 10 min.

exp_test_time <- 30*1+2*1.5+1*10

# Typical 8th grade class periods range 45-55 minutes.
# Usually x2 or x3 the expected testing time should be
# reserved for students of all ability levels to complete
# the assessment.

(exp_test_time*2)/60
(exp_test_time*3)/60

# Item type response-time point estimates may poorly account
# for the typical skewness of time data. It is more
# informative to model the likely testing time as a 
# range, based on item type lowest 2.5pct and top 97.5pct
# response-times. This requires data, and the derivation of
# distributions can be difficult.

# An example ...
# Distribution response-time in given in seconds and then
# converted back to minutes at the test level.

exp_short_test_time <- (30*30+2*40+1*300)/60
round(((exp_short_test_time/exp_test_time)-1)*100,2)

exp_long_test_time <- (30*90+2*225+1*2100)/60
round(((exp_long_test_time/exp_test_time)-1)*100,2)

# -- Field-test (FT) items --

# Usually testing duration is reviewed for
# approval without field-testing considerations; a
# practice that often ends in district dissatisfaction.

# When considering the assessment plan, the additional
# item response-time needed to respond to field-test items
# can inform their number.

# -- Confirm scaling form --

# Import data
load("DATA/assess_op_demo_R_objects")

# Item metadata
names(item_metadata)

# Item types
by(item_metadata, item_metadata$RC, function(x){table(x$Status,x$Type)})

# --------------- #
# FIELD-TEST PLAN #
# --------------- #

# -- Item development --

# It takes 33 items: 30 MCQ + 2 TEI + 1 SCR
# to build an OP base form. When constructing
# a form, CD needs flexibility in selecting
# items from an item bank in order to meet all
# test construction specification requirements.

# -- Field-test forms --

# Keep in mind certain item types may require
# greater numbers of examinees to calibrate.
