#=============================== LIBRARIES ===============================
library(joeysvowels)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lsa)
library(factoextra)
library(data.table)
library(vegan)
library(gridExtra)
library(lme4)
library(lmerTest)
library(robustlmm)
library(praatpicture)
library(DHARMa)


#=============================== DATA ===============================
e_file_en_peninsular_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/e_file_en_peninsular_F.csv')
e_file_en_peninsular_F <- drop_na(e_file_en_peninsular_F)

e_file_en_peninsular_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/e_file_en_peninsular_M.csv')
e_file_en_peninsular_M <- drop_na(e_file_en_peninsular_M)

e_file_en_peruvian_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/e_file_en_peruvian_F.csv')
e_file_en_peruvian_F <- drop_na(e_file_en_peruvian_F)

e_file_en_peruvian_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/e_file_en_peruvian_M.csv')
e_file_en_peruvian_M <- drop_na(e_file_en_peruvian_M)

i_file_en_peninsular_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/i_file_en_peninsular_F.csv')
i_file_en_peninsular_F <- drop_na(i_file_en_peninsular_F)

i_file_en_peninsular_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/i_file_en_peninsular_M.csv')
i_file_en_peninsular_M <- drop_na(i_file_en_peninsular_M)

i_file_en_peruvian_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/i_file_en_peruvian_F.csv')
i_file_en_peruvian_F <- drop_na(i_file_en_peruvian_F)

i_file_en_peruvian_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/ENH/i_file_en_peruvian_M.csv')
i_file_en_peruvian_M <- drop_na(i_file_en_peruvian_M)



e_file_og_peninsular_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/e_file_og_peninsular_F.csv')
e_file_og_peninsular_F <- drop_na(e_file_og_peninsular_F)

e_file_og_peninsular_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/e_file_og_peninsular_M.csv')
e_file_og_peninsular_M <- drop_na(e_file_og_peninsular_M)

e_file_og_peruvian_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/e_file_og_peruvian_F.csv')
e_file_og_peruvian_F <- drop_na(e_file_og_peruvian_F)

e_file_og_peruvian_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/e_file_og_peruvian_M.csv')
e_file_og_peruvian_M <- drop_na(e_file_og_peruvian_M)

i_file_og_peninsular_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/i_file_og_peninsular_F.csv')
i_file_og_peninsular_F <- drop_na(i_file_og_peninsular_F)

i_file_og_peninsular_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/i_file_og_peninsular_M.csv')
i_file_og_peninsular_M <- drop_na(i_file_og_peninsular_M)

i_file_og_peruvian_F <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/i_file_og_peruvian_F.csv')
i_file_og_peruvian_F <- drop_na(i_file_og_peruvian_F)

i_file_og_peruvian_M <- read.csv('/Users/inigoparra/Desktop/sociophonetics/csv_files/OG/i_file_og_peruvian_M.csv')
i_file_og_peruvian_M <- drop_na(i_file_og_peruvian_M)


#=============================== PCA ===============================
colors = c("#00AFBB", "#E7B800", "#FC4E07")

data_transpose_1 <- transpose(e_file_og_peninsular_F)
data_transpose_2 <- transpose(e_file_en_peninsular_F)

colnames(data_transpose_1) <- rownames(e_file_og_peninsular_F)
rownames(data_transpose_1) <- colnames(e_file_og_peninsular_F)
colnames(data_transpose_2) <- rownames(e_file_en_peninsular_F)
rownames(data_transpose_2) <- colnames(e_file_en_peninsular_F)


pca_og <- prcomp(data_transpose_1, scale = TRUE)
pca_en <- prcomp(data_transpose_2, scale = TRUE)


plot_pca_og <- fviz_pca_ind(pca_og, 
                            col.ind = "cos2", 
                            gradient.cols = colors, 
                            repel = TRUE,
                            title = 'Individuals PCA - Original Tokens')

plot_pca_en <- fviz_pca_ind(pca_en, 
                            col.ind = "cos2", 
                            gradient.cols = colors, 
                            repel = TRUE,
                            title = 'Individuals PCA - A2A Tokens')
grid.arrange(plot_pca_og, plot_pca_en, ncol = 2)
dev.off()


procrustes <- protest(pca_og, pca_en)
print(procrustes)
plot(procrustes)
text(
  procrustes,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)

# The samples, when divided by condition, are concordant. That is, they are significantly
# similar when analysing their principal components.

#=============================== MODELS ===============================
linear_model_data <- read.csv('/Users/inigoparra/Desktop/linear_model_data.csv')
linear_model_data

# Get the z-scores for the percentage
linear_model_data$z_scores_v <- (linear_model_data$v_percentage-mean(linear_model_data$v_percentage)/sd(linear_model_data$v_percentage))
linear_model_data$z_scores_dur <- (linear_model_data$duration-mean(linear_model_data$duration)/sd(linear_model_data$duration))
summary(linear_model_data)

# Some descriptive stats
sd(linear_model_data$duration)
sd(linear_model_data$voiced)
sd(linear_model_data$unvoiced)
sd(linear_model_data$v_percentage)

og_lm_data <- linear_model_data %>% filter(linear_model_data$condition == 'og')
en_lm_data <- linear_model_data %>% filter(linear_model_data$condition == 'en')


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
lme_model_v_og <- lmer(z_scores_v ~ country + gender + (1 | id), data = og_lm_data)
summary(lme_model_v_og)

lme_model_v_en <- lmer(z_scores_v ~ country + gender + (1 | id), data = en_lm_data)
summary(lme_model_v_en)

lme_model_dur_og <- lmer(z_scores_dur ~ country + gender + (1 | id), data = og_lm_data)
summary(lme_model_dur_og)

lme_model_dur_en <- lmer(z_scores_dur ~ country + gender + (1 | id), data = en_lm_data)
summary(lme_model_dur_en)

#Diagnostics
resout <- simulateResiduals(lme_model_dur_en)
plot(resout)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
rlme_model_v_og <- rlmer(z_scores_v ~ country + gender + (1 | id), data = og_lm_data)
summary(rlme_model_v_og)

rlme_model_v_en <- rlmer(z_scores_v ~ country + gender + (1 | id), data = en_lm_data)
summary(rlme_model_v_en)

rlme_model_dur_og <- rlmer(z_scores_dur ~ country + gender + (1 | id), data = og_lm_data)
summary(rlme_model_dur_og)

rlme_model_dur_en <- rlmer(z_scores_dur ~ country + gender + (1 | id), data = en_lm_data)
summary(rlme_model_dur_en)


#=============================== PRAATPIC ===============================
praatpicture(
  sound = '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_female/e/bre.wav',
  frames = c('sound', 'spectrogram'),
  proportion = c(50, 50),
  pitch_plotOnSpec = TRUE,
  pitch_color = 'blue',
  draw_rectangle = c('spectrogram', 0.0255, 4800, 0.055, 100, border='blue', lwd=2)
  )

praatpicture(
  sound = '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_female/e_enhanced/bre.wav',
  frames = c('sound', 'spectrogram'),
  proportion = c(50, 50)
)
