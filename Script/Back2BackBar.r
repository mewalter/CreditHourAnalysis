rm(list = ls())

# load packages
library(dplyr)
library(ggplot2)


# create a fake data set
## some preparation
set.seed(123)
ten_positive_rand_numbers <- abs(rnorm(10)) + 0.1
the_prob <- ten_positive_rand_numbers / sum(ten_positive_rand_numbers)

fk_data <- data.frame(job_type = sample(LETTERS[1:10], 1000, 
                                        replace = TRUE, prob = the_prob),
                      gender = sample(c("Male", "Female"), 1000, 
                                      replace = TRUE))

# prepare data for plotting
plotting2_df <-
  fk_data %>% 
  group_by(job_type, gender) %>% 
  summarise(Freq = n()) %>% 
  # a trick!
  mutate(Freq = if_else(gender == "Male", -Freq, Freq))
## find the order
temp_df <-
  plotting_df %>% 
  filter(gender == "Female") %>% 
  arrange(Freq)
the_order <- temp_df$job_type


# plot
p <- 
  plotting_df %>% 
  ggplot(aes(x = job_type, y = Freq, group = gender, fill = gender)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  # another trick!
  scale_y_continuous(breaks = seq(-150, 150, 50), 
                     labels = abs(seq(-150, 150, 50))) +
  labs(x = "Job type", y = "Count", title = "Back-to-back bar chart") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  # reverse the order of items in legend
  # guides(fill = guide_legend(reverse = TRUE)) +
  # change the default colors of bars
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Male", "Female"),
                    labels=c("Male", "Female")) 

print(p)