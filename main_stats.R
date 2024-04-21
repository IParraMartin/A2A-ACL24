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
library(MASS)
library(faux)

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

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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


#=============================== PCA DATA ===============================
data_transpose_e_pen_f_og <- transpose(e_file_og_peninsular_F)
colnames(data_transpose_e_pen_f_og) <- rownames(e_file_og_peninsular_F)
rownames(data_transpose_e_pen_f_og) <- colnames(e_file_og_peninsular_F)

data_transpose_e_pen_f_en <- transpose(e_file_en_peninsular_F)
colnames(data_transpose_e_pen_f_en) <- rownames(e_file_en_peninsular_F)
rownames(data_transpose_e_pen_f_en) <- colnames(e_file_en_peninsular_F)

data_transpose_e_pen_m_og <- transpose(e_file_og_peninsular_M)
colnames(data_transpose_e_pen_m_og) <- rownames(e_file_og_peninsular_M)
rownames(data_transpose_e_pen_m_og) <- colnames(e_file_og_peninsular_M)

data_transpose_e_pen_m_en <- transpose(e_file_en_peninsular_M)
colnames(data_transpose_e_pen_m_en) <- rownames(e_file_en_peninsular_M)
rownames(data_transpose_e_pen_m_en) <- colnames(e_file_en_peninsular_M)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
data_transpose_e_per_f_og <- transpose(e_file_og_peruvian_F)
colnames(data_transpose_e_per_f_og) <- rownames(e_file_og_peruvian_F)
rownames(data_transpose_e_per_f_og) <- colnames(e_file_og_peruvian_F)

data_transpose_e_per_f_en <- transpose(e_file_en_peruvian_F)
colnames(data_transpose_e_per_f_en) <- rownames(e_file_en_peruvian_F)
rownames(data_transpose_e_per_f_en) <- colnames(e_file_en_peruvian_F)

data_transpose_e_per_m_og <- transpose(e_file_og_peruvian_M)
colnames(data_transpose_e_per_m_og) <- rownames(e_file_og_peruvian_M)
rownames(data_transpose_e_per_m_og) <- colnames(e_file_og_peruvian_M)

data_transpose_e_per_m_en <- transpose(e_file_en_peruvian_M)
colnames(data_transpose_e_per_m_en) <- rownames(e_file_en_peruvian_M)
rownames(data_transpose_e_per_m_en) <- colnames(e_file_en_peruvian_M)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
data_transpose_i_pen_f_og <- transpose(i_file_og_peninsular_F)
colnames(data_transpose_i_pen_f_og) <- rownames(i_file_og_peninsular_F)
rownames(data_transpose_i_pen_f_og) <- colnames(i_file_og_peninsular_F)

data_transpose_i_pen_f_en <- transpose(i_file_en_peninsular_F)
colnames(data_transpose_i_pen_f_en) <- rownames(i_file_en_peninsular_F)
rownames(data_transpose_i_pen_f_en) <- colnames(i_file_en_peninsular_F)

data_transpose_i_pen_m_og <- transpose(i_file_og_peninsular_M)
colnames(data_transpose_i_pen_m_og) <- rownames(i_file_og_peninsular_M)
rownames(data_transpose_i_pen_m_og) <- colnames(i_file_og_peninsular_M)

data_transpose_i_pen_m_en <- transpose(i_file_en_peninsular_M)
colnames(data_transpose_i_pen_m_en) <- rownames(i_file_en_peninsular_M)
rownames(data_transpose_i_pen_m_en) <- colnames(i_file_en_peninsular_M)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
data_transpose_i_per_f_og <- transpose(i_file_og_peruvian_F)
colnames(data_transpose_i_per_f_og) <- rownames(i_file_og_peruvian_F)
rownames(data_transpose_i_per_f_og) <- colnames(i_file_og_peruvian_F)

data_transpose_i_per_f_en <- transpose(i_file_en_peruvian_F)
colnames(data_transpose_i_per_f_en) <- rownames(i_file_en_peruvian_F)
rownames(data_transpose_i_per_f_en) <- colnames(i_file_en_peruvian_F)

data_transpose_i_per_m_og <- transpose(i_file_og_peruvian_M)
colnames(data_transpose_i_per_m_og) <- rownames(i_file_og_peruvian_M)
rownames(data_transpose_i_per_m_og) <- colnames(i_file_og_peruvian_M)

data_transpose_i_per_m_en <- transpose(i_file_en_peruvian_M)
colnames(data_transpose_i_per_m_en) <- rownames(i_file_en_peruvian_M)
rownames(data_transpose_i_per_m_en) <- colnames(i_file_en_peruvian_M)

#=============================== PCA ANALYSIS FOR (E) ===============================
pca_e_pen_f_og <- prcomp(data_transpose_e_pen_f_og, scale = TRUE)
pca_e_pen_f_en <- prcomp(data_transpose_e_pen_f_en, scale = TRUE)

pca_e_pen_m_og <- prcomp(data_transpose_e_pen_m_og, scale = TRUE)
pca_e_pen_m_en <- prcomp(data_transpose_e_pen_m_en, scale = TRUE)

pca_e_per_f_og <- prcomp(data_transpose_e_per_f_og, scale = TRUE)
pca_e_per_f_en <- prcomp(data_transpose_e_per_f_en, scale = TRUE)

pca_e_per_m_og <- prcomp(data_transpose_e_per_m_og, scale = TRUE)
pca_e_per_m_en <- prcomp(data_transpose_e_per_m_en, scale = TRUE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
colors = c("#00AFBB", "#E7B800", "#FC4E07")

plot_pca_pca_e_pen_f_og <- fviz_pca_ind(pca_e_pen_f_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_e_pen_f_en <- fviz_pca_ind(pca_e_pen_f_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

plot_pca_pca_e_pen_m_og <- fviz_pca_ind(pca_e_pen_m_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_e_pen_m_en <- fviz_pca_ind(pca_e_pen_m_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

grid.arrange(plot_pca_pca_e_pen_f_og, 
             plot_pca_pca_e_pen_f_en,
             plot_pca_pca_e_pen_m_og,
             plot_pca_pca_e_pen_m_en,
             ncol = 2)
dev.off()


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
plot_pca_pca_e_per_f_og <- fviz_pca_ind(pca_e_per_f_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_e_per_f_en <- fviz_pca_ind(pca_e_per_f_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

plot_pca_pca_e_per_m_og <- fviz_pca_ind(pca_e_per_m_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_e_per_m_en <- fviz_pca_ind(pca_e_per_m_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

grid.arrange(plot_pca_pca_e_per_f_og, 
             plot_pca_pca_e_per_f_en,
             plot_pca_pca_e_per_m_og,
             plot_pca_pca_e_per_m_en,
             ncol = 2)
dev.off()


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
procrustese_e_pen_f <- protest(pca_e_pen_f_og, pca_e_pen_f_en)
print(procrustese_e_pen_f)
plot(procrustese_e_pen_f)
text(
  procrustese_e_pen_f,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)

procrustese_e_pen_m <- protest(pca_e_pen_m_og, pca_e_pen_m_en)
print(procrustese_e_pen_m)
plot(procrustese_e_pen_m)
text(
  procrustese_e_pen_m,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
procrustese_e_per_f <- protest(pca_e_per_f_og, pca_e_per_f_en)
print(procrustese_e_per_f)
plot(procrustese_e_per_f)
text(
  procrustese_e_per_f,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)

procrustese_e_per_m <- protest(pca_e_per_m_og, pca_e_per_m_en)
print(procrustese_e_per_m)
plot(procrustese_e_per_m)
text(
  procrustese_e_per_m,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)


#=============================== PCA ANALYSIS FOR (I) ===============================
pca_i_pen_f_og <- prcomp(data_transpose_i_pen_f_og, scale = TRUE)
pca_i_pen_f_en <- prcomp(data_transpose_i_pen_f_en, scale = TRUE)

pca_i_pen_m_og <- prcomp(data_transpose_i_pen_m_og, scale = TRUE)
pca_i_pen_m_en <- prcomp(data_transpose_i_pen_m_en, scale = TRUE)

pca_i_per_f_og <- prcomp(data_transpose_i_per_f_og, scale = TRUE)
pca_i_per_f_en <- prcomp(data_transpose_i_per_f_en, scale = TRUE)

pca_i_per_m_og <- prcomp(data_transpose_i_per_m_og, scale = TRUE)
pca_i_per_m_en <- prcomp(data_transpose_i_per_m_en, scale = TRUE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
plot_pca_pca_i_pen_f_og <- fviz_pca_ind(pca_i_pen_f_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_i_pen_f_en <- fviz_pca_ind(pca_i_pen_f_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

plot_pca_pca_i_pen_m_og <- fviz_pca_ind(pca_i_pen_m_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_i_pen_m_en <- fviz_pca_ind(pca_i_pen_m_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

grid.arrange(plot_pca_pca_i_pen_f_og, 
             plot_pca_pca_i_pen_f_en,
             plot_pca_pca_i_pen_m_og,
             plot_pca_pca_i_pen_m_en,
             ncol = 2)
dev.off()


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
plot_pca_pca_i_per_f_og <- fviz_pca_ind(pca_i_per_f_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_i_per_f_en <- fviz_pca_ind(pca_i_per_f_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

plot_pca_pca_i_per_m_og <- fviz_pca_ind(pca_i_per_m_og, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - OG Tokens')

plot_pca_pca_i_per_m_en <- fviz_pca_ind(pca_i_per_m_en, 
                                        col.ind = "cos2", 
                                        gradient.cols = colors, 
                                        repel = TRUE,
                                        title = 'Individuals PCA - A2A Tokens')

grid.arrange(plot_pca_pca_i_per_f_og, 
             plot_pca_pca_i_per_f_en,
             plot_pca_pca_i_per_m_og,
             plot_pca_pca_i_per_m_en,
             ncol = 2)
dev.off()


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
procrustese_i_pen_f <- protest(pca_i_pen_f_og, pca_i_pen_f_en)
print(procrustese_i_pen_f)
plot(procrustese_i_pen_f)
text(
  procrustese_i_pen_f,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)

procrustese_i_pen_m <- protest(pca_i_pen_m_og, pca_i_pen_m_en)
print(procrustese_i_pen_m)
plot(procrustese_i_pen_m)
text(
  procrustese_i_pen_m,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
procrustese_i_per_f <- protest(pca_i_per_f_og, pca_i_per_f_en)
print(procrustese_i_per_f)
plot(procrustese_i_per_f)
text(
  procrustese_i_per_f,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)

procrustese_i_per_m <- protest(pca_i_per_m_og, pca_i_per_m_en)
print(procrustese_i_per_m)
plot(procrustese_i_per_m)
text(
  procrustese_i_per_m,
  display = 'target',
  col = 'black',
  pos = 4,
  cex = 0.8
)


#=============================== DATA PREP MODELS ===============================
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


#=============================== DATA ASSUMPTIONS ===============================
descdist(linear_model_data$z_scores_v, discrete = FALSE)
descdist(linear_model_data$z_scores_dur, discrete = FALSE)

gaussian_v <- fitdist(linear_model_data$z_scores_v, "norm")
gaussian_dur <- fitdist(linear_model_data$z_scores_dur, "norm")
plot(gaussian_v)
plot(gaussian_dur)

shapiro.test(gaussian_dur$data)


bartlett.test(z_scores_v ~ interaction(country, gender, condition), 
              data = linear_model_data)

bartlett.test(z_scores_dur ~ interaction(country, gender, condition), 
              data = linear_model_data)


#=============================== MODELS ===============================
aov_model_v_og <- aov(z_scores_v ~ country + gender, data = og_lm_data)
summary(aov_model_v_og)

aov_model_v_en <- aov(z_scores_v ~ country + gender, data = en_lm_data)
summary(aov_model_v_en)

aov_model_dur_og <- aov(z_scores_dur ~ country + gender, data = og_lm_data)
summary(aov_model_dur_og)

aov_model_dur_en <- aov(z_scores_dur ~ country + gender, data = en_lm_data)
summary(aov_model_dur_en)

#Diagnostics
results <- simulateResiduals(aov_model_v_og)
plot(results)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
glm_model_v_og <- glm(z_scores_v ~ country + gender, data = og_lm_data)
summary(glm_model_v_og)

glm_model_v_en <- glm(z_scores_v ~ country + gender, data = en_lm_data)
summary(glm_model_v_en)

glm_model_dur_og <- glm(z_scores_dur ~ country + gender, data = og_lm_data)
summary(glm_model_dur_og)

glm_model_dur_en <- glm(z_scores_dur ~ country + gender, data = en_lm_data)
summary(glm_model_dur_en)

#Diagnostics
result <- simulateResiduals(glm_model_dur_en)
plot(result)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
rlm_model_v_og <- rlm(z_scores_v ~ country + gender, data = og_lm_data)
summary(rlm_model_v_og)

rlm_model_v_en <- rlm(z_scores_v ~ country + gender, data = en_lm_data)
summary(rlm_model_v_en)

rlm_model_dur_og <- rlm(z_scores_dur ~ country + gender, data = og_lm_data)
summary(rlm_model_dur_og)

rlm_model_dur_en <- rlm(z_scores_dur ~ country + gender, data = en_lm_data)
summary(rlm_model_dur_en)

#Diagnostics
plot(rlm_model_v_og)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
rlme_model_v_og <- rlmer(z_scores_v ~ country + gender + (1 | id), data = og_lm_data)
summary(rlme_model_v_og)

rlme_model_v_en <- rlmer(z_scores_v ~ country + gender + (1 | id), data = en_lm_data)
summary(rlme_model_v_en)

rlme_model_dur_og <- rlmer(z_scores_dur ~ country + gender + (1 | id), data = og_lm_data)
summary(rlme_model_dur_og)

rlme_model_dur_en <- rlmer(z_scores_dur ~ country + gender + (1 | id), data = en_lm_data)
summary(rlme_model_dur_en)

#Diagnostics
plot(rlme_model_v_og)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
cond_model_v <- glm(z_scores_v ~ country * gender + condition, data = linear_model_data)
summary(cond_model_v)

cond_model_dur <- glm(z_scores_dur ~ country * gender + condition, data = linear_model_data)
summary(cond_model_dur)

result <- simulateResiduals(cond_model_dur)
plot(result)


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
