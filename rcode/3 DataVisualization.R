rm(list=ls())
library(tidyverse)    # loads ggplot2
library(reshape2)


###############################################################
# SESSION 3a Data Visualization
###############################################################
transport <- read_csv("data/transport_clean.csv") %>%
  filter(!is.na(height))

# 3.1 scatterplot
ggplot(transport,
       aes(x = weight, y = height,
           color = method, shape = gender)) +
  geom_point()

# or
ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = method, shape = gender),
             size = 4)

# 3.2 fit a loess
ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = method, shape = gender),
             size = 4) +
  geom_smooth()

# fit a straight line and add labels to the points
ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = method, shape = gender),
             size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name)) +
  labs(x = "Weight (kg)",
       y = "Height (cm)",
       shape = "Gender",
       colour = "Method")


# 3.3 spread out the labels, use ggrepel
library(ggrepel)
ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = method, shape = gender),
             size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(aes(label = name)) +
  labs(x = "Weight (kg)",
       y = "Height (cm)",
       shape = "Gender",
       colour = "Method")

# 3.4 split into several plots by gender
ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = method, shape = gender),
             size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text_repel(aes(label = name)) +
  labs(x = "Weight (kg)",
       y = "Height (cm)",
       shape = "Gender",
       colour = "Method") +
  facet_wrap(~ gender, scales = "free")   # different axes scales


ggplot(transport,
       aes(x = weight, y = height)) +
  geom_point(aes(color = gender),
             size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text_repel(aes(label = name)) +
  labs(x = "Weight (kg)",
       y = "Height (cm)",
       shape = "Gender",
       colour = "Method") +
  facet_wrap(~ method)


# 3.5 other graphs
ggplot(transport,
       aes(x = height, fill = gender)) +
  geom_density(alpha = 0.4)

ggplot(transport,
       aes(x = weight, y = height, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~ method)

# 3.6 plot summary statistics
transport %>%
  group_by(method, gender) %>%
  summarise(avgHeight = mean(height),
           sdHeight = sd(height)) %>%
  ggplot(aes(x = method, y = avgHeight, fill = method)) +
    geom_bar(stat = "identity") +  # plot using the values in the data
    geom_errorbar(aes(ymin = avgHeight - sdHeight,
                      ymax = avgHeight + sdHeight),
                  width = 0.5) +
    facet_wrap(~ gender) +
    guides(fill = FALSE)  # remove the legend



# 3.7 get histograms for both height and weight using facets
transport %>%
  melt(id.vars = c("gender", "name", "method")) %>%
  ggplot(aes(x = value, fill = variable)) +
    geom_histogram(bins = 10, colour = "black") +
    facet_wrap(~ variable, scales = "free_x") +
    guides(fill = FALSE)
    



###############################################################
# SESSION 3b Professional plotting
###############################################################

ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot()

# remove gray background
ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot() +
  theme_bw()

ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot() +
  theme_classic()


ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot() +
  theme_void()

library(ggthemes)
ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot() +
  theme_economist()

ggplot(transport,
       aes(x = weight, y = height,
           fill = gender)) +
  geom_boxplot() +
  theme_excel()



# set a default theme in your workspace
theme_set(theme_bw())


###############################################################
# change text elements
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Courier", 
                                   size = 15, angle = 90))

# change backgrounds
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.background = element_rect(fill = "yellow", 
                                         colour = "black"))

# remove all elements
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid = element_blank())

# move the legend
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))

###############################################################
# set the scale
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_log10(limits = c(100, 200), 
                breaks = c(100, 125, 150, 175)) 


# The first a = 0 multiplies the scale by 1 + a
# The second b = 0 adds ±b to the axis extrema
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) 


###############################################################
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("green", "blue"))


###############################################################
# saving plots
# jpeg, tiff, png, svg, pdf
ggsave(file = "plot/boxplot.svg")


# write directly to a file
png("plot/HeightByGender.png", width = 18, height = 18, units = "cm", 
    res = 300)
ggplot(transport, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  scale_fill_manual(values = c("green", "blue"))
dev.off()


