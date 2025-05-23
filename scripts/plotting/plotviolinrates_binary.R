library(RevGadgets)
library(tidyverse)
library(ggthemes)
setwd("~/Desktop/bomarea_traits/")

# process data
rates <- readTrace(c("output/binary_type_ard_run_1.log",
                     "output/binary_type_ard_run_2.log"))

rates <- combineTraces(rates)

df <- rates[[1]]
cols <- c("rate[1]", "rate[2]")


# change column names to more informative parameters 
#    A  B  C
# A  X  1  2
# B  3  X  4
# C  5  6  X
# where A = plain, B = bracteole, C = compound

df_cols <- df[,cols]
colnames(df_cols) <- c("Plain to compound",
                       "Compound to plain")


df_cols %>%
  tidyr::gather(key = "grp", 
                value = "val",
                factor_key = TRUE) -> df_rates


# set up colors
colors <- RevGadgets::colFun(6)

names(colors) <- levels(df_rates$grp)

g <- ggplot(df_rates) +
  geom_violin(data = df_rates,
              aes(x = grp, 
                  y = val, 
                  group = grp, 
                  fill = grp),
              color = "black",
              lwd = 1,
              scale = "width",
              show.legend = F) +
  stat_summary(fun=mean, aes(x = grp, y = val), geom="point", size=2, color="black") +
  #geom_hline(yintercept = 0.0, color = "grey") + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(name = "Transition rate") +
  ylab("Posterior density") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, 
                                   #angle=45,
                                   hjust = .5),
        axis.title.y = element_text(face = "bold", 
                                    size = 20),
        axis.title.x = element_text(face = "bold", 
                                    size = 20))

summarizeTrace(rates, vars = c("rate[1]", "rate[2]"))

print(g)
ggsave("binary_type_ase_ard_violinPlot.png", width = 7, height = 10, dpi = 200)
