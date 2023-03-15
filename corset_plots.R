# Code for Corset Plots
# By Elena Leib


# This is code that I wrote to make faceted corset plots (see Leib et al., accepted, Dev Psych)

# The original data set looked at performance on two different trial types
# (congruent vs incongruent) for participants in different grades (Grade 3, 5,
# and 7). However, this code could be used anytime a participant has two data
# points in a sample (e.g., pre and post-test) and you want to plot the change
# between that for individual participants, and at a group level.

# Note that the plot is faceted, but that part of it could be removed if you
# don't have a grouping variable to facet by.


#### Prerequisites to run this code ####
# Make sure that you have the libraries tidyverse and see installed


#### Load in libraries ####
library(tidyverse)
library(see) # geom_violinhalf function that we need is in this package


#### Make fake data to plot ####

# I made fake data set of 100 participants to show how the code works in simplest terms.

# Imagine the study was a math learning intervention.

# participant ID
pid <- rep(1:100, times = 2)

# timepoint: pre or post
timepoint <- factor(rep(c("pre", "post"), each = 100), levels = c("pre", "post"))

# grade: either 1 or 2 (50 in grade 1, 50 in grade 2)
grade <- rep(c(1, 2), each = 50, times = 2)

# condition: either cond1 or cond2 (half of each grade is in cond1)
condition <- factor(rep(c("cond1", "cond2"), each = 25, times = 4))

# set seed for generating random numbers for the scores
set.seed(123)

# make pre-test data
score_pre <- rnorm(100, 2, 4)
score_pre[score_pre < 0] <- 0

# make post-test data to find an effect of grade and condition
score_post_c1g1 <- rnorm(25, 6, 3)
score_post_c2g1 <- rnorm(25, 10, 3)

score_post_c1g2 <- rnorm(25, 10, 2)
score_post_c2g2 <- rnorm(25, 15, 2)

# imagine this is a score on a math understanding task that we gave before and
# after the math intervention
score <- c(score_pre, score_post_c1g1, score_post_c2g1, score_post_c1g2, score_post_c2g2)


# Make the data frame
df <- data.frame(pid, grade, condition, timepoint, score)

# Note that these data are in "long" format, with one row for pre-test
# score and one row for post-test score per participant. If your data are not in
# this format, you will need to reshape the dataframe into this format in order
# to use this plotting function.


#### Write function to create corset plots ####
makeCorsetPlot <- function(df, individual, x_axis, y_axis, group_var, n_group, 
                           x_lab, y_lab, group_labs, x_tick_labs,
                           y_min, y_max) {
  # individual = the name of the variable that links data between the two rows (e.g., participant ID)
  # x_axis = the name of the variable that will be plotted on the x-axis as a string
  # y_axis = the name of the variable that will be plotted on the y-axis as a string
  # group_var = the name of the grouping variable as a string
  # n_group = the number of groups there are (e.g., how many grades are you plotting)
  # x_lab = the label for the x-axis
  # y_lab = the label for the y-axis
  # group_labs = vector of labels for the groups (in order of the factors, be careful of this!)
  # x_tick_labs = vector of two labels for the x-tick marks on the x-axis
  # y_min = the minimum value for the y-axis
  # y_max = the maximum value for the y-axis
  
  
  # Set "flip" vector for geom_halfviolin
  flip_vec = seq(1, n_group * 2, by=2)
  
  # using mean_cl_boot (bootstrapping) to get the bootstrapped 95% confidence
  # interval for the errorbars. Set seed so that we can reproduce our graph
  # exactly every time.
  set.seed(1234)
  
  g <- df %>% 
    # set up plot
    ggplot(aes(x=get(x_axis), y=get(y_axis))) + 
    
    # add half violin plot
    ### the width argument controls how "fat" the violins are
    geom_violinhalf(aes(color = get(group_var)), flip = flip_vec, width = 1) + 
    
    # add line for each participant connecting their performance on each trial type 
    geom_line(aes(group = get(individual), color = get(group_var)), size = .5, alpha = .1) +
    
    # add error bar around mean (using mean_cl_boot) -> gives you bootstrapped
    # 95% confidence interval for the errorbars
    ### confidence interval by default is set to .95. to change it, add conf.int
    ### argument, such as conf.int = .99 for 99% CI
    stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = .15) +
    
    # add point for mean
    stat_summary(geom = "point", fun = "mean", shape = 15) +
    
    # add black line connecting means 
    stat_summary(aes(group = get(group_var)), geom = "line", fun = "mean", color = "black") +
    
    # facet by grouping variable
    facet_wrap(~get(group_var), labeller = as_labeller(group_labs)) +
    
    # set x and y axis labels
    xlab(x_lab) + 
    ylab(y_lab) +
    
    
    
    # set up axes
    ### Can change x-axis tick mark labels by adding labels = c() argument to scale_x_discrete
    ### the expansion(mult = 1.5 "squeezes the waist" of the corset so the plots
    ### aren't too far apart) make the number bigger to make the "waist"
    ### smaller. Could also make this an argument in the function
    scale_x_discrete(expand = expansion(mult = 2), labels = x_tick_labs) +
    scale_y_continuous(limits = c(y_min, y_max + .01)) +
    
    # set theme
    theme_classic() + 
    
    # remove legend and set margins for axis labels
    theme(legend.position = "none", 
          strip.background = element_blank(), 
          text = element_text(size = 24),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          # Make more space between the axis label and the axis
          axis.title.x = element_text(margin = 
                                        margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = 
                                        margin(t = 0, r = 15, b = 0, l = 0)),
          
          strip.text.x = element_text(size = 16)) 
  
  
  # Can also set manuel colors for the different groups manually or using color brewer
  # scale_color_manual(values = c())
  
  return(g)
}


#### Time to plot ####

# Let's imagine we want to see how participants scores change from pre- to post-test.
# x-axis: timepoint
# y-axis: score
# group_var = condition

# Note that my data frame has two rows per participant, one for pre-test score and one for post-test score.
# If your data frame is not in this "longer" format (i.e., two rows per
# participant; in other words: one row each for what you want to the lines for
# each participant to connect between, e.g., before/after or between two
# conditions), then you need to get your data in this structure before plotting.

makeCorsetPlot(df, 
               individual = "pid", 
               x_axis = "timepoint", 
               y_axis = "score", 
               group_var = "condition", 
               n_group = 2, 
               x_lab = "Timepoint", 
               y_lab = "Total Score", 
               group_labs = c("cond1" = "Condition 1", "cond2" = "Condition 2"), 
               x_tick_labs = c("Pre", "Post"),
               y_min = 0, 
               y_max = (max(df$score) + 5))


# There a lot of things that you can change around in the code above. For
# example, you may want to add a title. Alternatively, you might not want to
# name the x-tick marks. Or maybe you don't need to facet, you can remove that,
# too.

