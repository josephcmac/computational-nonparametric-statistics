library(tidyverse)

Wilcoxon <- function(m, n, N) {
  as.numeric((1:N) %*% sample(c(rep(0, m), rep(1, n))))
}

m <- 2
n <- 3
N <- m + n

n_sample <- 10000

W <- data.frame(value = sapply(1:n_sample, function(i) Wilcoxon(m, n, N)))

# Calculate probabilities by dividing the count by the total number of samples
ggplot(W, aes(x = value)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, color = "black", fill = "skyblue") +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y axis to percentage format if needed
  labs(y = "Probability", x = "Value") +
  theme_classic()

W %>% group_by(value) %>% 
  summarise(count = n()) %>%
  mutate(prob = count/sum(count)) %>%
  select(-count)



