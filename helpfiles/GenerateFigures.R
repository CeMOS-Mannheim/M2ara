library(ggplot)
library(dplyr)
library(ggpubr)
df <- tibble(x = 1:25 + rnorm(25, sd = 0.5),
             y = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.5, 9.75, 10, 9.75, 9.5,  9, 8, 7, 6, 5, 4, 3, 2, 1, 0) + rnorm(25, sd = 0.5))

underfit <- df %>%
  ggplot(aes(x = x , y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(title = "Underfit",
       subtitle = "High bias")


overfit <- df %>%
  ggplot(aes(x = x , y = y)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 0.2) +
  theme_minimal(base_size = 14)+
  labs(title = "Overfit",
       subtitle = "High variance")

goodfit <- df %>%
  ggplot(aes(x = x , y = y)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(title = "Good balance",
       subtitle = "Low bias, low variance")

p <- ggarrange(underfit, overfit, goodfit, labels = "AUTO", nrow = 1)
ggsave(p, width = 450, height = 150, dpi = 96, units = "px", filename = "helpfiles/overUnderFit.png", scale = 1.75)
