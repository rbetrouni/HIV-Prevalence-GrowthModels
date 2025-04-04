library(deSolve)
library(ggplot2)

# Defining our SIRV model
sirv_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N - v * S
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    dV <- v * S
    list(c(dS, dI, dR, dV))
  })
}

# Population and Initial Conditions
N <- 50000
state <- c(S = N - 10, I = 10, R = 0, V = 0)
times <- seq(0, 180, by = 1)

# Parameters
params_no_vaccine <- c(beta = 0.3, gamma = 0.13, v = 0)
params_with_vaccine <- c(beta = 0.3, gamma = 0.13, v = 0.01)

# Solve differential equations
out_no_vaccine <- as.data.frame(ode(y = state, times = times, func = sirv_model, parms = params_no_vaccine))
out_no_vaccine$Scenario <- "No Vaccine"

out_with_vaccine <- as.data.frame(ode(y = state, times = times, func = sirv_model, parms = params_with_vaccine))
out_with_vaccine$Scenario <- "With Vaccine"

# Combine datasets
out_combined <- rbind(out_no_vaccine, out_with_vaccine)

# Calculate cumulative infected (I + R)
out_combined$cumulative_infected <- out_combined$I + out_combined$R

out_with_vaccine <- subset.data.frame(out_combined,Scenario=="With Vaccine")

# Save CSV file for the scenario with vaccination (for LTVLM model)
write.csv(out_with_vaccine[, c("time", "cumulative_infected")], "sirv_cumulative_infected.csv", row.names = FALSE)

# Enhanced Plot
ggplot(out_combined, aes(x = time, y = cumulative_infected, color = Scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Impact of Vaccination on COVID-19 Cumulative Infections",
       x = "Time (days)",
       y = "Cumulative Infected Individuals") +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("sirv_vaccination_impact.pdf")
ggsave("sirv_vaccination_impact.png")
