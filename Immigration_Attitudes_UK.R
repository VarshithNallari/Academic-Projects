install.packages(c("tidyverse", "haven", "MASS", "psych", "sjPlot", "pacman"))
pacman::p_load(tidyverse, haven, MASS, psych, sjPlot)

ess_data <- read_dta("ESS11-subset.dta")

# Load and prepare data
analysis_data <- ess_data %>%
  dplyr::select(imdfetn, hinctnta, ipeqopta, lrscale, agea, eduyrs) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~na_if(., 77)),
         across(everything(), ~na_if(., 88)),
         across(everything(), ~na_if(., 99))) %>%
  mutate(imdfetn = 5 - imdfetn,                   
         ipeqopta_rev = 7 - ipeqopta,             
         imdfetn = factor(imdfetn,                
                          levels = 1:4, 
                          ordered = TRUE)) %>%
drop_na()


continuous_table <- analysis_data %>%
  select(imdfetn, hinctnta, ipeqopta_rev, lrscale, agea, eduyrs) %>%
  describe() %>%
  as_tibble(rownames = "Variable") %>%
  select(Variable, min, max, mean, median, sd) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

view_df(continuous_table)

ggplot(analysis_data, aes(x = hinctnta, y = as.numeric(imdfetn))) +
  geom_point(position = position_jitter(width = 0.8, height = 0.6), alpha = 0.8, size = 1.8, color = "#000000") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(title = "Graph 1. Income and Support for Immigration", x = "Household's Net Income", y = "Immigration Support")


analysis_data %>%
  mutate(income_level = cut(hinctnta, breaks = 3, labels = c("Low", "Medium", "High"))) %>%
  group_by(lrscale, income_level) %>%
  summarise(mean_support = mean(imdfetn), .groups = "drop") %>%
  ggplot(aes(x = factor(lrscale), y = mean_support, group = income_level, color = income_level)) +
  geom_line() +
  geom_point() +
  labs(x = "Political Orientation", y = "Mean Support for Immigration", color = "Income Level") +
  theme_minimal()


# Run ordinal logistic regression
regression_model <- polr(imdfetn ~ hinctnta + ipeqopta_rev + lrscale + agea + eduyrs, 
                  data = analysis_data, Hess = TRUE)

summary(regression_model)


tab_model(regression_model,
  digits = 3,
  show.std = TRUE,     
  show.ci = FALSE,     
  show.p = TRUE,       
  pred.labels = c(
    "Household Income (hinctnta)",
    "Belief in Equality (ipeqopta_rev)",
    "Political Ideology (lrscale)",
    "Age (agea)",
    "Education (eduyrs)"),
  dv.labels = "Support for Immigration (imdfetn)",
  title = "Ordinal Logistic Regression Results")









