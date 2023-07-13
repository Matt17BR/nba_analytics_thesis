theme_set( # set a default ggplot theme
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(vjust = 1.5),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(face = "bold", size = 12, color = "white")))

shots %>%
  unite(SHOT_INFO, c(SHOT_TYPE,SHOT_MADE)) %>% 
  group_by(SEASON_2, SHOT_INFO) %>%
  summarize(COUNT = n()) %>%
  mutate(RATIO_3 = COUNT / sum(COUNT)) %>%
  filter(grepl("3",SHOT_INFO)) %>%
  mutate(SHOT_INFO=if_else(grepl("TRUE",SHOT_INFO),"Made","Missed")) %>%
  ggplot(aes(SEASON_2, RATIO_3,
         fill = SHOT_INFO, label = label_percent(0.1)(RATIO_3))) +
  geom_col(position = "stack") +
  geom_text(position = "stack", color = "white", vjust = 2) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x="Season",y="3PT Field Goals to Total FGs",fill="Shot Result",
       title="3-point field goals in the NBA over the past 20 seasons",
       subtitle="3-point field goals over total field goals between 2003 and 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

analytical_teams %>%
  group_by(SEASON_2) %>%
  summarize(EV_AVG = mean(EV_AVG)) %>%
  ggplot(aes(SEASON_2, EV_AVG, group = 1)) +
  geom_line(lwd = 1, color = "steelblue") +
  geom_point(size = 3, shape = 21, fill = "white") +
  labs(x = "Season", y = "Expected points per shot", 
       title = "EV Average per Season",
       subtitle = "League-wide expected shooting efficiency between 2003 and 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

C_half + shots %>%
  mutate(SHOT_MADE=if_else(grepl("TRUE",SHOT_MADE),"Made","Missed")) %>%
  filter(LOC_Y <= 47 & BASIC_ZONE != "Backcourt" & TEAM_ID == 44 &
         (SEASON_2 == "2003-04" | SEASON_2 == "2015-16")) %>%
  geom_point(aes(LOC_X,LOC_Y,color=SHOT_MADE),.,alpha=0.3) +
  scale_color_manual(values = c("#003da5","#ffc72c")) +
  facet_grid(~SEASON_2) +
  labs(title = "Golden State Warriors shooting",
       subtitle = "Shot charts in 2003-04 vs. 2015-16",
       color = "Shot result")

C_half + shots %>%
  filter(LOC_Y <= 47 & BASIC_ZONE != "Backcourt" & TEAM_ID == 45 &
         (SEASON_2 == "2003-04" | SEASON_2 == "2017-18")) %>%
  left_join(EV_info) %>%
  geom_point(aes(LOC_X,LOC_Y,color=EV,shape=BASIC_ZONE),.) +
  scale_color_gradient(low="#a0a0a0", high="#ce1141") +
  facet_grid(~SEASON_2) +
  labs(title = "Houston Rockets shooting",
       subtitle = "Shot charts in 2003-04 vs. 2017-18",
       color = "Shot expected value",
       shape = "Shot area")

games_details %>%
  group_by(GAME_ID,TEAM_ABBREVIATION) %>%
  summarize(FGA=sum(FGA), FGM=sum(FGM),
            PTS=sum(PTS)) %>%
  mutate(WIN=if_else(PTS==max(PTS),"Win","Loss")) %>%
  filter(FGA!=0) %>%
  ggplot(aes(FGA,FGM/FGA,color=WIN)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  labs(x="Field Goals Attempts",y="Shot Percentage",color="Game Result",
       title="Shot precision reliably predicts wins",
       subtitle="Relationship between shot precision and game results")

games_details %>%
  group_by(GAME_ID,TEAM_ABBREVIATION) %>%
  summarize(FG3A=sum(FG3A), FG3M=sum(FG3M),
            PTS=sum(PTS)) %>%
  mutate(WIN=if_else(PTS==max(PTS),"Win","Loss")) %>%
  filter(FG3A!=0) %>%
  ggplot(aes(FG3A,FG3M/FG3A,color=WIN)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  labs(x="3PT Field Goals Attempts",y="3PT Percentage",color="Game Result",
       title="3PT shooting precision reliably predicts wins",
       subtitle="Relationship between 3-point shot precision and game results")

# Plot variable importance
# extract_fit_engine(fit_rf)$variable.importance %>% 
#   tibble(Variable = names(.), Importance = .) %>%
#   ggplot(aes(reorder(Variable, Importance), Importance, fill = Variable)) +
#   geom_col(alpha = 0.8) +
#   coord_flip() +
#   labs(x = "Feature",
#        y = "Importance",
#        title = "Variable Importance") +
#   guides(fill = "none")

fit_dt %>%
  extract_fit_engine() %>%
  rpart.plot(main="Decision tree plot for full model",roundint = FALSE)

extract_fit_engine(fit_dt)$variable.importance %>% 
  tibble(Variable = names(.), Importance = .) %>%
  ggplot(aes(reorder(Variable, Importance), Importance, fill = Variable)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature", y = "Importance",
       title = "Variable Importance",
       subtitle = "Full decision tree model fit") +
  guides(fill = "none")

fit_dt_nofg %>%
  extract_fit_engine() %>%
  rpart.plot(main="Decision tree plot for model with no FGs",roundint = FALSE)

extract_fit_engine(fit_dt_nofg)$variable.importance %>% 
  tibble(Variable = names(.), Importance = .) %>%
  ggplot(aes(reorder(Variable, Importance), Importance, fill = Variable)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature", y = "Importance",
       title = "Variable Importance",
       subtitle = "Decision tree model fit with no FGs") +
  guides(fill = "none")

a$Logistic_Model_Coefficients %>%
  filter(term != "(Intercept)") %>%
  select(id, term, estimate, std.error) %>%
  group_by(term) %>%
  mutate(avg_estimate = mean(estimate)) %>%
  ggplot(aes(id, estimate, fill = term, color = term)) +
  geom_hline(aes(yintercept = avg_estimate), linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.1, lwd = 1.2) +
  facet_wrap(~term, scales = "free_y") +
  labs(x = "CV Folds", y = "Estimate ± 95% CI",
       title = "Regression Coefficients ± 95% CI for 10-fold CV",
       subtitle = "Dashed line = average coefficient estimate over 10 CV folds per predictor") +
  guides(fill = "none", color = "none") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.1),
        axis.text.y = element_text(face = "bold", size = 12))

## Violin plot version of the plot above
# a$Logistic_Model_Coefficients %>%
#   filter(term != "(Intercept)") %>%
#   select(term, estimate, std.error) %>%
#   ggplot(aes("", estimate, fill = term)) +
#   geom_violin(trim = FALSE) +
#   facet_wrap(~ term, scales = "free_y") +
#   labs(x = "",
#        y = "Estimate",
#        title = "Violin plot of estimates for each term",
#        fill = "Term") +
#   guides(fill = "none") +
#   theme(axis.ticks = element_blank(),
#         axis.text.x = element_blank())

bind_rows(a$Fits_Metrics$Logistic_Regression %>% mutate(Model = "Full Logistic Regression"),
          a$Fits_Metrics$Decision_Tree       %>% mutate(Model = "Full Decision Tree"),
          b$Fits_Metrics$Logistic_Regression %>% mutate(Model = "No FG Logistic Regression"),
          b$Fits_Metrics$Decision_Tree       %>% mutate(Model = "No FG Decision Tree")) %>%
  mutate(.metric = case_when(
    .metric == "f_meas"  ~ "F-measure",
    .metric == "kap"     ~ "Cohen's Kappa",
    .metric == "roc_auc" ~ "ROC AUC")) %>%
  ggplot(aes(.metric, mean, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Metric", y = "Mean Value", fill = "Model",
       title = "Model Metrics Comparison",
       subtitle = "Comparison of model metrics for our 4 different models") +
  theme(axis.text.x = element_text(size = 10, vjust = 2.5))

bind_rows(roc_curve(a$Fits_Predictions$Logistic_Regression,
                    WIN_home, .pred_Yes, event_level = "second") %>%
           mutate(Model = "Full Logistic Regression"),
          roc_curve(a$Fits_Predictions$Decision_Tree,
                    WIN_home, .pred_Yes, event_level = "second") %>%
           mutate(Model = "Full Decision Tree"),
          roc_curve(b$Fits_Predictions$Logistic_Regression,
                    WIN_home, .pred_Yes, event_level = "second") %>%
           mutate(Model = "No FG Logistic Regression"),
          roc_curve(b$Fits_Predictions$Decision_Tree,
                    WIN_home, .pred_Yes, event_level = "second") %>%
           mutate(Model = "No FG Decision Tree")) %>%
  ggplot(aes(1 - specificity, sensitivity, color = Model)) +
  geom_line(lwd = 1.5) +
  geom_segment(aes(0,0,xend=1,yend=1),linetype="dashed",col="black") +
  coord_equal() +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       title = "ROC Curves", 
       subtitle = "Comparison of ROC curves for our 4 different models")
