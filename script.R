library(readxl)
library(gvlma)
library(lmtest)
library(car)
library(ggplot2)
library(sandwich)
library(dplyr)


#####################
### FUNCTIONS #######
#####################
clean_data = function(df){
  # Remove ships, foreign stations, and other "abnormal" committees
  df = df[df$`Typ obwodu` == "stały", ]
  df = df[df$`Liczba kart do głosowania otrzymanych przez obwodową komisję wyborczą, ustalona po ich przeliczeniu przed rozpoczęciem głosowania z uwzględnieniem ewentualnych kart otrzymanych z rezerwy`
          >= 100, ]
}

mad_analysis = function(title, df, ko_col, pis_col){
  ko  = df[[ko_col]]
  pis = df[[pis_col]]
  
  med_ko = median(ko)
  med_pis = median(pis)
  
  mad_ko = mad(ko)
  mad_pis = mad(pis)
  
  k_needed_ko = (ko - med_ko)/ mad_ko
  k_needed_pis= (pis - med_pis)/ mad_pis
  
  outliers_ko = sum(abs(k_needed_ko) > 3)
  outliers_pis = sum(abs(k_needed_pis) > 3)
  
  cat("MAD analysis: "); print(title)
  cat("\nAverage k-needed for KO:\n"); print(mean(k_needed_ko))
  cat(outliers_ko, "outliers found for KO —", 
      round(outliers_ko / nrow(df) * 100, 2), "% of stations.\n\n")
  
  cat("Average k-needed for PiS:\n"); print(mean(k_needed_pis))
  cat(outliers_pis, "outliers found for PiS —", 
      round(outliers_pis / nrow(df) * 100, 2), "% of stations.\n\n")
  
  cat("Total outliers found:\n"); print(outliers_ko + outliers_pis)
  cat((outliers_ko + outliers_pis)/nrow(df) * 100, "% of stations. \n")
  
  return(list(
    outliers_ko = outliers_ko,
    outliers_pis = outliers_pis,
    z_ko = k_needed_ko,
    z_pis = k_needed_pis,
    outliers_ko_percent = outliers_ko / nrow(df),
    outliers_pis_percent = outliers_pis / nrow(df)
  ))
}

test_assumptions <- function(mod) {
  summary_stats <- summary(mod)
  
  cat("Call:\n")
  print(mod$call)
  
  cat("\n\nAssumptions testing:\n")
  cat("\n\nMULTICOLLINEARITY:\n")
  print(vif(mod))
  cat("\n\nHETEROSCEDASTICITY:\n")
  print(gvlma(mod))
  cat("\n\nINDEPENDENCE:\n")
  print(dwtest(mod))
}

compare_with_benford = function(df, colname){
  base = df
  base = base %>%
    mutate(first_digit = as.numeric(substr(as.character(abs({{ colname }})), 1, 1)))
  
  observed = base %>%
    count(first_digit) %>%
    filter(first_digit >= 1 & first_digit <= 9)
  
  benford <- data.frame(
    first_digit = 1:9,
    expected_prop = log10(1 + 1 / (1:9))
  )
  
  plot_data = observed %>%
    left_join(benford, by = "first_digit") %>%
    mutate(expected_count = expected_prop * sum(n))
  
  return(plot_data)
  
}

#####################
### 2025 ANALYSIS ###
#####################

# Load the data
data_round1 = read_excel("protokoly_tura1.xlsx")
data_round2 = read_excel("protokoly_tura2.xlsx")

# Remove ships, foreign stations, and other "abnormal" committees
data_round1 = clean_data(data_round1)
data_round2 = clean_data(data_round2)

# Preserve and rename the necessary columns
round1_col_indexes = c(1:7, 24, 11, 33:45)
round2_col_indexes = c(1:7, 24, 11, 32, 33)
round1_col_names = c("id", "gmina", "gmina_area", "powiat", "powiat_area", 
                       "voivod", "hq", "no_voted1", "eligible1", "bartoszewicz", 
                       "biejat", "braun", "hołownia", "jakubiak", "maciak", "mentzen", 
                       "nawrocki1", "senyszyn", "stanowski", "trzaskowski1", "woch", "zandberg")

round2_col_names = c("id", "gmina", "gmina_area", "powiat", "powiat_area", 
                     "voivod", "hq", "no_voted2", "eligible2", "nawrocki2", "trzaskowski2")

data_round1 = data_round1[, round1_col_indexes]
data_round2 = data_round2[, round2_col_indexes]
colnames(data_round1) = round1_col_names
colnames(data_round2) = round2_col_names

# Join the two dataframes
df = merge(data_round1, data_round2, by = c("id", "gmina", "gmina_area", "powiat", "powiat_area", 
                                            "voivod", "hq"))

### FUNDACJA BATOREGO ANALYSIS ###
# Linear regression: round 1 ~ round 2
model1 = lm(nawrocki2 ~ trzaskowski1 + nawrocki1 + bartoszewicz + biejat + 
              braun + hołownia + jakubiak + maciak + mentzen + senyszyn + 
              stanowski + woch + zandberg + no_voted2, data = df)
model2 = lm(trzaskowski2 ~ trzaskowski1 + nawrocki1 + bartoszewicz + biejat + 
              braun + hołownia + jakubiak + maciak + mentzen + senyszyn + 
              stanowski + woch + zandberg + no_voted1, data = df)

test_assumptions(model1)
#Statistical assumptions test
#Non-multicollinearity
vif(model1)
vif(model2)

#Random residuals
resid1 = resid(model1)
resid2 = resid(model2)
#shapiro.test(resid1)
#shapiro.test(resid2)
#Heteroscedasticity
gvlma(model1)
gvlma(model2)
#Independence
dwtest(model1)
dwtest(model2)

# Visualize residuals vs fitted
df_plot = data.frame(fitted = fitted(model1), residuals = resid(model1))
ggplot(df_plot, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Apply Robust Standard Errors to improve the p-values?
model1_alt = lm(sqrt(nawrocki2) ~ trzaskowski1 + nawrocki1 + bartoszewicz + biejat + 
                  braun + hołownia + jakubiak + maciak + mentzen + senyszyn + 
                  stanowski + woch + zandberg, data = df)
summary(model1_alt)
test_assumptions(model1_alt)

### KRZYSZTOF KONTEK ANALYSIS ###
# General, for the whole country

mad_analysis_2025 = mad_analysis("2025 general", df, "trzaskowski2", "nawrocki2")

# Next best thing to postal codes - regional code
regionaldf = df
regionaldf = regionaldf[, !names(regionaldf) %in% c("id", "gmina", "powiat", "powiat_area", "voivod", "hq")]
regionaldf = regionaldf %>%
  group_by(gmina_area) %>%
  summarise(
    count = n(),
    across(where(is.numeric), sum, na.rm = TRUE)
  )

regional_mad_2025_analysis = mad_analysis("2025 regional", regionaldf, "trzaskowski2", "nawrocki2")


## KRAKÓW KRAKÓW KOCHAM KRAKÓW KRAKÓW MIESZKAM TU TU TU TU
krakowdf =  df %>%
  filter(grepl("m. Kraków", df$gmina, ignore.case = TRUE))

krakow_mad_2025_analysis = mad_analysis("2025 Kraków", krakowdf, "trzaskowski2", "nawrocki2")


# Benford's Law
benford2025_nawro = compare_with_benford(df, nawrocki2)
ggplot(benford2025_nawro, aes(x = factor(first_digit))) +
  geom_col(aes(y = n), fill = "skyblue", color = "black", width = 0.7) +
  geom_point(aes(y = expected_count), color = "red", size = 3) +
  geom_line(aes(y = expected_count, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benford's Law",
       x = "First Digit",
       y = "Count") +
  theme_minimal()

benford2025_trzask = compare_with_benford(df, trzaskowski2)
ggplot(benford2025_trzask, aes(x = factor(first_digit))) +
  geom_col(aes(y = n), fill = "skyblue", color = "black", width = 0.7) +
  geom_point(aes(y = expected_count), color = "red", size = 3) +
  geom_line(aes(y = expected_count, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benford's Law",
       x = "First Digit",
       y = "Count") +
  theme_minimal()


#####################
### 2020 ANALYSIS ###
#####################
round1_2020 = read.csv("protokoly2020_tura1.csv", sep=";")
round2_2020 = read.csv("protkoly2020_tura2.csv", sep=";")

clean_2020_data = function(df){
  df = df[df$`Typ obszaru` != "statek", ]
  df = df[df$`Typ obszaru` != "zagranica", ]
  df = df[df$`Typ obwodu` == "stały", ]
}

data_2020r1 = round1_2020[round1_2020$Typ.obwodu == "stały",]
data_2020r1 = data_2020r1[data_2020r1$Liczba.kart.wyjętych.z.urny >= 100, ]
data_2020r2 = round2_2020[round2_2020$Typ.obwodu == "stały",]
data_2020r2 = data_2020r2[data_2020r2$Liczba.kart.wyjętych.z.urny >= 100, ]

# Preserve and rename the necessary columns
r1_2020_colindexes = c(2, 8:11, 13, 28, 34:44)
r2_2020_colnindexes = c(2, 8:11, 13, 28, 34:35)

r1_2020_colnames = c("id", "hq", "gmina", "powiat", "voivod", "eligible1", "no_voted1",
                     "biedron", "bosak", "duda1", "holownia", "jakubiak", "kosiniakkamysz",
                     "piotrowski", "tanajno", "trzaskowski1", "witkowski", "zoltek")
r2_2020_colnames = c("id", "hq", "gmina", "powiat", "voivod", "eligible2", "no_voted2",
                     "duda2", "trzaskowski2")

data_2020r1 = data_2020r1[, r1_2020_colindexes]
colnames(data_2020r1) = r1_2020_colnames
data_2020r2 = data_2020r2[, r2_2020_colnindexes]
colnames(data_2020r2) = r2_2020_colnames

# Join the two dataframes
df2020 = merge(data_2020r1, data_2020r2, by = c("id", "hq", "gmina", "powiat", "voivod"))

# Change data in candidate and voter numbers to numeric
conversion = c("eligible1", "no_voted1", "biedron", "bosak", "duda1", "holownia",
               "jakubiak", "kosiniakkamysz", "piotrowski", "tanajno", "trzaskowski1",
               "witkowski", "zoltek", "eligible2", "no_voted2", "duda2", "trzaskowski2")
df2020[conversion] = lapply(df2020[conversion],as.numeric)

# 2020 MODELS
model3 = lm(duda2 ~ trzaskowski1 + duda1 + biedron + bosak + holownia + jakubiak +
              kosiniakkamysz + piotrowski + tanajno + 
              witkowski + zoltek + no_voted2, data = df2020)
model4 = lm(trzaskowski2 ~ trzaskowski1 + duda1 + biedron + bosak + holownia + jakubiak +
              kosiniakkamysz + piotrowski + tanajno + 
              witkowski + zoltek + no_voted2, data = df2020)
               
test_assumptions(model3)

df2020_plot = data.frame(fitted = fitted(model3), residuals = resid(model3))
ggplot(df2020_plot, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

model3_alt = lm(sqrt(duda2) ~ trzaskowski1 + duda1 + biedron + bosak + holownia + jakubiak +
                  kosiniakkamysz + piotrowski + tanajno + 
                  witkowski + zoltek, data = df2020)

gvlma(model3_alt)
df2020_alt_plot = data.frame(fitted = fitted(model3_alt), residuals = resid(model3_alt))
ggplot(df2020_alt_plot, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

### KRZYSZTOF KONTEK ANALYSIS ###
# General, for the whole country
mad_analysis_2020 = mad_analysis("2020 general", df2020, "trzaskowski2", "duda2")

# Next best thing to postal codes - regional code
# TODO: No regional differentiation in df

# KRAKOW KRAKOW ANALYSIS :DDDD
krakow2020df =  df2020 %>%
  filter(grepl("Kraków", df2020$gmina, ignore.case = TRUE))

mad_analysis_krakow_2020 = mad_analysis("2020 Kraków", krakow2020df, "trzaskowski2", "duda2")

# BENFORD'S LAW
benford2020 = compare_with_benford(df2020, duda2)

ggplot(benford2020, aes(x = factor(first_digit))) +
  geom_col(aes(y = n), fill = "skyblue", color = "black", width = 0.7) +
  geom_point(aes(y = expected_count), color = "red", size = 3) +
  geom_line(aes(y = expected_count, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benford's Law",
       x = "First Digit",
       y = "Count") +
  theme_minimal()
