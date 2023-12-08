rm(list = ls())


fruits <- c("apple", "orange", "banana", "pear", "plum", 
            "kiwi fruit", "peach", "mango", "lemon")
origin <- c("local", "imported")


a_df <- data.frame(the_fruits = sample(fruits, 1000, replace = TRUE),
                   the_origin = sample(origin, 1000, replace = TRUE)) 


plotting_df <-
  a_df %>%  
  group_by(the_origin, the_fruits) %>% 
  summarise(Freq = n())

temp_df_1 <-
  plotting_df %>% 
  filter(the_origin == "local") %>% 
  arrange(Freq)
the_order <- temp_df_1$the_fruits

p1 <- 
  plotting_df %>% 
  ggplot(aes(x = the_fruits, weight = Freq, fill = the_origin)) +
  geom_bar(position = "dodge", width = 0.75) +
  # NB: use the above to plot bars in the certain order
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  labs(x = "", y = "Number of fruits in the 'basket'") +
  scale_fill_brewer(breaks=c("local", "imported"), palette = "Set1") +
  # NB: use the above to change the default order and color of legend
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=14),
        legend.text = element_text(size=9),
        panel.background = element_rect(fill =  "grey90"))

print(p1)