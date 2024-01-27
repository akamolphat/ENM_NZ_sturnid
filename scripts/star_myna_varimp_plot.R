library(tidyverse)
library(biomod2)

df <- data.frame(
  sp = c(rep("myna", 3), rep("starling", 3)),
  range = c(rep(c("native", "NZ", "nativeNZ"), 2)),
  filepath = c("data/myna/BIOMOD/native/native_fullBIOMODv1/myBiomodModelOut_native_fullBIOMODv1.Rdata",
               "data/myna/BIOMOD/NZ/NZ_fullBIOMODv1/myBiomodModelOut_NZ_fullBIOMODv1.Rdata",
               "data/myna/BIOMOD/nativeNZ/nativeNZ_BIOMODv1a/myBiomodModelOut_nativeNZ_BIOMODv1a.Rdata",
               "data/starling/BIOMOD/native/native_fullBIOMODv1/myBiomodModelOut_native_fullBIOMODv1.Rdata",
               "data/starling/BIOMOD/NZ/NZ_fullBIOMODv1/myBiomodModelOut_NZ_fullBIOMODv1.Rdata",
               "data/starling/BIOMOD/nativeNZ/nativeNZ_fullBIOMODv1/myBiomodModelOut_nativeNZ_fullBIOMODv1.Rdata"))

dfOut <- data.frame()
for (i in 1:nrow(df)){
  Rdatapath <- df$filepath[i]
  range <- df$range[i]
  species <- df$sp[i]
  load(Rdatapath)
  varimp <- apply(get_variables_importance(myBiomodModelOut), c(1,2), mean)
  varimp_norm <- varimp/matrix(rep(apply(varimp, 2, sum), nrow(varimp)), byrow = T, nrow = nrow(varimp))*100
  varimp_norm <- data.frame(varimp_norm)
  varimp_norm$variable <- rownames(varimp_norm)
  varimp_filt <- varimp_norm %>% 
    pivot_longer(!variable, names_to = "models", values_to = "variable_importance") %>% 
    filter(models == "MAXENT.Phillips") 
  varimp_filt$range <- range
  varimp_filt$species <- species
  dfOut <- rbind(dfOut, varimp_filt)
}

write.csv(dfOut, "results/mynastarling_var_imp.csv")

p <- dfOut %>%
  ggplot(aes(fill = range, x = variable, y = variable_importance)) + 
  ylab("Variable importance (%)") +
  geom_histogram(position = "dodge", stat = "identity") + 
  geom_hline(yintercept = 5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "topright",
        axis.title.y = element_blank()) +
  facet_grid(. ~ species) +
  scale_fill_manual(name = "Model training area",
                    values = c("#377EB8", "#b2df8a", "#D95F02"),
                    breaks = c("native", "nativeNZ", "NZ"),
                    labels = c("native", "native + NZ", "NZ")) +
  # labs(fill = "Model training area") +
  geom_vline(xintercept=seq(1.5, length(unique(dfOut$variable))-0.5, 1), 
             linewidth = 0.5, lty = 2,  colour="black") +
  scale_x_discrete(labels = c(
    "tree_cover_pct" = "Tree cover\npercentage",
    "pop_dens" = "Human population\ndensity",
    "gsl" = "Growing season\nlength",
    "gdd5" = "Growing degree\ndays > 5\u00B0C",
    "bio12" = "Mean annual\nprecipitation"
  )) + 
  theme(panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom")
p
png("results/mynastarling_var_imp.png", width = 8, height = 4.5, units = "in", res = 300)
p
dev.off()

# varimp_norm %>% pivot_longer(!variable, names_to = "models", values_to = "variable_importance") %>% ggplot(aes(fill = variable, x = models, y = variable_importance)) + geom_histogram(position = "dodge", stat = "identity") + coord_flip()
# 
# p <- varimp_norm %>% pivot_longer(!variable, names_to = "models", values_to = "variable_importance") %>% filter(models == "MAXENT.Phillips") %>%
#   ggplot(aes(fill = variable, x = variable, y = variable_importance)) + 
#   ylab("Variable importance (%)") +
#   geom_histogram(position = "dodge", stat = "identity") + coord_flip() +
#   scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title.y = element_blank())
# 
# png("results/myna_nativeNZ_var_imp.png", width = 6, height = 6, units = "in", res = 300)
# p
# dev.off()
# 
# 
# 
# load("data/starling/BIOMOD/nativeNZ/nativeNZ_fullBIOMODv1/myBiomodModelOut_nativeNZ_fullBIOMODv1.Rdata")
# varimp <- apply(get_variables_importance(myBiomodModelOut), c(1,2), mean)
# 
# varimp_norm <- varimp/matrix(rep(apply(varimp, 2, sum), 5), byrow = T, nrow = 5)*100
# varimp_norm <- data.frame(varimp_norm)
# varimp_norm$variable <- rownames(varimp_norm)
# 
# varimp_norm %>% pivot_longer(!variable, names_to = "models", values_to = "variable_importance") %>% ggplot(aes(fill = variable, x = models, y = variable_importance)) + geom_histogram(position = "dodge", stat = "identity") + coord_flip()
# 
# p <- varimp_norm %>% pivot_longer(!variable, names_to = "models", values_to = "variable_importance") %>% filter(models == "MAXENT.Phillips") %>%
#   ggplot(aes(fill = variable, x = variable, y = variable_importance)) + 
#   ylab("Variable importance (%)") +
#   geom_histogram(position = "dodge", stat = "identity") + coord_flip() +
#   scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title.y = element_blank())
# 

