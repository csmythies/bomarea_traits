library(RevGadgets)
library(ggplot2)
library(ape)
setwd("~/Desktop/bomarea_traits/")
branchiness <- processAncStates("output/branchiness.tree", 
                                state_labels = c("0" = "no branching",
                                                 "1" = "1",
                                                 "2" = "2",
                                                 "3" = "3",
                                                 "4" = "4",
                                                 "5" = "5",
                                                 "7" = "7"))

plotAncStatesPie(branchiness, tip_labels = TRUE)
ggsave("branchiness_tree_ordered.png", width = 10, height = 10, dpi = 200)

#rates <- readTrace(c("output/infl_type_ard_run_1.log",
#                     "output/infl_type_ard_run_2.log"))

#rates <- combineTraces(rates)

#plotTrace(rates, match = "rate")