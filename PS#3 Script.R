### PART 2 ###
fuel <- seq(-40, 200, length.out = 1000)
likelihood <- dnorm(fuel, mean = 34, sd = 20)

plot(fuel, likelihood, type = "l", lwd = 2, col = "#0D2D40",
     xlab = "Fuel (L)", ylab = "Density", 
     main = "Naive Probability Density Function for Usable Fuel")
abline(v = 0, col = "#9AD5F8", lty = 2)
text(0, max(likelihood)*0.7, "0 L", pos = 4, col = "#9AD5F8")

### PART 4 ###
prior <- ifelse(fuel >= 0 & fuel <= 182, 1/182, 0)
unnorm_post <- likelihood * prior
normalization <- function(x, y) {
  sum((diff(x) * (head(y, -1) + tail(y, -1))) / 2)
}
posterior <- unnorm_post / normalization(fuel, unnorm_post)

plot(fuel, likelihood, type = "l", lwd = 2, col = "#5E74DD",
     xlab = "Fuel (L)", ylab = "Density", ylim = c(0, max(likelihood)*1.2),
     main = "Grid-Based Bayesian Update")
lines(fuel, prior, col = "#9AD5F8", lwd = 2, lty = 2)
lines(fuel, posterior, col = "#0D2D40", lwd = 2)
legend("topright", legend = c("Likelihood", "Prior", "Posterior"),
       col = c("#5E74DD", "#9AD5F8", "#0D2D40"), lty = c(1,2,1), lwd = 2)

p_neg <- sum(posterior[fuel < 0]) * (fuel[2] - fuel[1])
p_neg

### PART 5 ###
library(ggplot2)
n <- 100000 
seeds <- c(123, 456, 789, 111, 222, 333, 555, 777, 888, 999)

density_df <- data.frame()
for (s in seeds) {
  set.seed(s)
  prior_sample <- runif(n, min = 0, max = 182)
  
  w <- dnorm(prior_sample, mean = 34, sd = 20)
  w <- w / sum(w) 
  posterior_sample <- sample(prior_sample, size = n, replace = TRUE, prob = w)
  
  d <- density(posterior_sample, from = 0, to = 182)
  temp <- data.frame(fuel = d$x, density = d$y, seed = factor(s))
  density_df <- rbind(density_df, temp)
}

ggplot(density_df, aes(x = fuel, y = density, color = seed)) +
  geom_line(size = 1) +
  labs(title = "Monte Carlo Method Bayesian Update",
       x = "Fuel (L)",
       y = "Density",
       color = "Seed") +
  scale_color_viridis_d(option = "mako") +
  theme_minimal()

### PART 7 ###
results <- data.frame(seed = seeds, p_yes = NA, p_no = NA)
dens_list <- list()
i <- 1
for (s in seeds) {
  set.seed(s)
  
  fuel_MC <- sample(posterior_sample, size = n, replace = TRUE)
  consumption <- rnorm(n, mean = 18, sd = 2)
  consumption[consumption <= 0] <- 0
  
  time <- fuel_MC / consumption
  dens_list[[i]] <- data.frame(time = density(time)$x, density = density(time)$y, 
                               seed = as.factor(s))
  
  p_yes <- mean(time >= (100/60) + 0.5)
  p_no <- mean(time < (100/60))
  results$p_yes[[i]] <- p_yes
  results$p_no[[i]] <- p_no
  i <- i+1
}

densities <- do.call(rbind, dens_list)
ggplot(densities, aes(x = time, y = density, color = seed)) +
  geom_line(size = 1) +
  labs(title = "Estimated Available Flight Time",
       x = "Flight Time (hr)",
       y = "Density",
       color = "Seed") +
  scale_color_viridis_d(option = "mako") +
  theme_minimal()

results_summary <- rbind(results, data.frame(seed = "Mean", 
                                             p_yes = mean(results$p_yes), 
                                             p_no = mean(results$p_no)))
colnames(results_summary) <- c("Seed", "Probability_of_Making_It", 
                               "Probability_of_Runing_Out_Fuel")
kable(results_summary, digits = 6)