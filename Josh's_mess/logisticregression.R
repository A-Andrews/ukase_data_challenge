df <- read.csv('cleaned_data copy/full_data_no_corrs_binarised_label.csv')

model <- glm(label ~ TT_p2_Bias_HA, data = df, family = "poisson")

summary(model)