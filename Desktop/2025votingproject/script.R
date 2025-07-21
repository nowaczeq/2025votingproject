library(readxl)
library(gvlma)
library(lmtest)
library(car)
library(ggplot2)
library(sandwich)

# Load the data
data_round1 = read_excel("protokoly_tura1.xlsx")
data_round2 = read_excel("protokoly_tura2.xlsx")

clean_data = function(df){
  # Remove ships, foreign stations, and other "abnormal" committees
  df = df[df$`Typ obszaru` != "statek", ]
  df = df[df$`Typ obszaru` != "zagranica", ]
  df = df[df$`Typ obwodu` == "stały", ]
  df = df[df$`Liczba kart do głosowania otrzymanych przez obwodową komisję wyborczą, ustalona po ich przeliczeniu przed rozpoczęciem głosowania z uwzględnieniem ewentualnych kart otrzymanych z rezerwy`
          >= 100, ]
}

# Remove ships, foreign stations, and other "abnormal" committees
data_round1 = clean_data(data_round1)
data_round2 = clean_data(data_round2)

# Preserve and rename the necessary columns
round1_col_indexes = c(1:7, 10, 11, 33:45)
round2_col_indexes = c(1:7, 10, 11, 32, 33)
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
              stanowski + woch + zandberg, data = df)
model2 = lm(trzaskowski2 ~ trzaskowski1 + nawrocki1 + bartoszewicz + biejat + 
              braun + hołownia + jakubiak + maciak + mentzen + senyszyn + 
              stanowski + woch + zandberg, data = df)

#Statistical assumptions test
#Non-multicollinearity
vif(model1)
vif(model2)

#Random residuals
resid1 = resid(model1)
resid2 = resid(model2)
shapiro.test(resid1)
shapiro.test(resid2)
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

# Apply Robust Standard Errors to improve the p-values
model1_alt = lm(sqrt(nawrocki2) ~ trzaskowski1 + nawrocki1 + bartoszewicz + biejat + 
                  braun + hołownia + jakubiak + maciak + mentzen + senyszyn + 
                  stanowski + woch + zandberg, data = df)
summary(model1_alt)
summary(model1)
vif(model1_alt)
gvlma(model1_alt)
dwtest(model1_alt)

### KRZYSZTOF KONTEK ANALYSIS ###
# General, for the whole country
kontek_df = df  # Put in in case we need to amend the information into the database itself

nawrocki_r2 = df$nawrocki2
trzaskowski_r2 = df$trzaskowski2

med_nawrocki = median(nawrocki_r2)
med_trzaskowski = median(trzaskowski_r2)

mad_nawrocki = mad(nawrocki_r2)
mad_trzaskowski = mad(trzaskowski_r2)

k_needed_nawrocki = (nawrocki_r2 - med_nawrocki)/ mad_nawrocki
k_needed_trzaskowski = (trzaskowski_r2 - med_trzaskowski)/ mad_trzaskowski

outliers_nawrocki = sum(abs(k_needed_nawrocki) > 3)
outliers_trzaskowski = sum(abs(k_needed_trzaskowski) > 3)



### 2020 ANALYSIS ###
round1_2020 = read.csv("protokoly2020_tura1.csv", sep=";")
round2_2020 = read.csv("protkoly2020_tura2.csv", sep=";")

clean_2020_data = function(df){
  df = df[df$`Typ obszaru` != "statek", ]
  df = df[df$`Typ obszaru` != "zagranica", ]
  df = df[df$`Typ obwodu` == "stały", ]
}

data_2020r1 = clean_2020_data(round1_2020)
data_2020r2 = clean_2020_data(round2_2020)
