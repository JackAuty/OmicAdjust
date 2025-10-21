################
#Generating a significant distribution like what I've seen
set.seed(1)
p <- runif(9000)
p2 <- c(p, runif(1000, max = 0.1))
p3 <- c(p2, runif(1000, max = 0.05))

#Look like it should have significance after BH
hist(p3, breaks = seq(0,1, by=0.05))

#Doesn't. I don't know what the BH is doing wrong here?
hist(p.adjust(p3, method="BH"), breaks = seq(0,1, by=0.05))



# --- Step 1: expected false positives ---
#Calculate expect false positive using bionomial estimate (mean = np, sd= npq)
n <- length(p3)
expected <- n * 0.05
sd <- sqrt(n * 0.05 * 0.95)
upper <- expected + 1.96 * sd   # 95% upper bound

cat("Expected:", expected, " 95% upper:", upper, "\n")

# --- Step 2: observed vs expected ---
#How many extra significant results do we have (using 95% upper estimate of expected false positives)
obs <- sum(p3 < 0.05)
excess <- max(0, obs - round(upper))  # "true discoveries" count
cat("Observed:", obs, " Excess true:", excess, "\n")

# --- Step 3: rank and rescale ---
r <- rank(p3, ties.method="first")

#Separate out the lowest p values taking the observed - expected significant results (excess)
# identify lowest `excess` values
ix_low <- order(p3)[1:excess]
ix_high <- setdiff(1:n, ix_low)

# rescale lowest values to [0,0.05] rescale the true significances to between 0 and 0.05
low_vals <- seq(0, 0.05, length.out = excess+2)[-c(1, excess+2)]

# rescale rest to [0.05,1]
high_vals <- seq(0.05, 1, length.out = length(ix_high)+2)[-c(1, length(ix_high)+2)]

# combine back
p_rescaled <- numeric(n)
p_rescaled[ix_low]  <- low_vals
p_rescaled[ix_high] <- high_vals



#Sledge hammer works
hist(p3, breaks = seq(0,1, by=0.05))

hist(p_rescaled, breaks = seq(0,1, by=0.05))


###############Function

OmicAdjust <- function(p, alpha = 0.05) {
  # number of tests
  n <- length(p)

  # --- Step 1: expected false positives under binomial ---
  expected <- n * alpha
  sd <- sqrt(n * alpha * (1 - alpha))
  upper <- expected + 1.96 * sd   # 95% upper bound

  # --- Step 2: observed vs expected ---
  obs <- sum(p < alpha)
  excess <- max(0, obs - round(upper))  # "true discoveries" count

  # --- Step 3: rank and rescale ---
  ix_low <- order(p)[1:excess]
  ix_high <- setdiff(1:n, ix_low)

  # rescale lowest values to [0, alpha]
  low_vals <- if (excess > 0) {
    seq(0, alpha, length.out = excess + 2)[-c(1, excess + 2)]
  } else numeric(0)

  # rescale rest to [alpha, 1]
  high_vals <- seq(alpha, 1, length.out = length(ix_high) + 2)[-c(1, length(ix_high) + 2)]

  # combine back
  p_rescaled <- numeric(n)
  if (excess > 0) p_rescaled[ix_low]  <- low_vals
  p_rescaled[ix_high] <- high_vals

  # ensure monotonicity: adjusted p-values never smaller than original
  p_final <- pmax(sort(p_rescaled), sort(p))

  return(list(
    p_rescaled = p_final,
    expected = expected,
    sd = sd,
    upper = upper,
    observed = obs,
    excess = excess
  ))
}

#################


plot(p3, p_rescaled, pch=16, col="#009999")

df_p<-cbind(p3, p_rescaled)
df_p

m=1000
ratio_sig <- 10
effect_size = 1
p_sim<-numeric(0)
for (i in 1:m){

  a<-rnorm(10)
  b<-rnorm(10, mean = effect_size)
  p_sim<-append(p_sim,t.test(a,b,var.equal =T)$p.value)

  for(i in 1:ratio_sig){
    c<-rnorm(10)
    p_sim<-append(p_sim,t.test(a,c,var.equal =T)$p.value)
  }

}
scaled<-OmicAdjust(p_sim)

scaled$expected
scaled$observed
scaled$excess

sum(scaled$p_rescaled<0.05)
sum(p_sim<0.05)
sum(OmicAdjust(p_sim)$p_rescaled<0.05)
hist(p_sim, breaks = seq(0,1, by=0.05))
hist(OmicAdjust(p_sim)$p_rescaled, breaks = seq(0,1, by=0.05))


data<- read.delim("clipboard", header=T)
data$gene_id<-toupper(data$X)
df_clean <- data[!grepl("LOC|MASTER", data$gene_id), ]

hist(df_clean$pval_lm, breaks = seq(0,1, by=0.05))
hist(p.adjust(df_clean$pval_lm, method="BH"), breaks = seq(0,1, by=0.05))
hist(OmicAdjust(df_clean$pval_lm)$p_rescaled, breaks = seq(0,1, by=0.05))

# Sort ascending (smallest p-values first)
data_sorted <- df_clean[order(df_clean$pval_lm), ]
data_sorted$p_adjust_JRA<-OmicAdjust(df_clean$pval_lm)$p_rescaled

colinfo<-data
colinfo$Tot_plas_mass

write.csvdatawrite.csv(data_sorted, "Liver_JRAadjust.csv")

head(data_sorted)


data_sorted

Genes_interest<-c("RPS16", "RPL21", "RPL31", "RPL11", "RPL14", "RPS3A", "RPS2", "UBA52", "RPL6")
Genes_interest<-c("VAMP8", "MAP2K2", "STX17", "ATG9B", "ATG9A", "ATG10", "ITPR1", "ZFYVE1")
Genes_interest<-c("VAMP8", "MAP2K2", "STX17", "ATG9B", "ATG9A", "ATG10", "ITPR1", "ZFYVE1")

Macrophage_markers <- c(
  "CD68",   # pan-macrophage
  "CSF1R",  # survival & lineage
  "CD14",   # innate immune receptor
  "ITGAM",  # CD11b, myeloid marker
  "CD163",  # scavenger receptor, M2-like
  "MRC1",   # CD206, mannose receptor
  "IL1B",   # pro-inflammatory cytokine
  "IL10"    # anti-inflammatory cytokine
)

Spleen_infection_genes <- c(
  # Innate pathogen recognition
  "TLR2",    # bacterial/fungal recognition
  "TLR4",    # LPS sensing
  "CD14",    # co-receptor for TLR4
  "MYD88",   # adaptor for TLR signalling

  # Cytokines and inflammation
  "TNF",     # pro-inflammatory cytokine
  "IL1B",    # fever and inflammation
  "IL6",     # acute phase response
  "IFNG",    # activates macrophages

  # Macrophage lineage and activity
  "CD68",    # pan-macrophage marker
  "CSF1R",   # macrophage survival
  "MRC1",    # CD206, alternative activation
  "CD163",   # scavenger receptor

  # B cell and antibody production
  "PAX5",    # B cell identity
  "AICDA",   # somatic hypermutation / class switching
  "IGHM",    # IgM heavy chain (early response)
  "IGHG1",   # IgG heavy chain (later response)

  # Complement system (bacterial clearance)
  "C1QA",
  "C3",
  "C4A"
)

RBC_anaemia_genes <- c(
  # Haemoglobin genes
  "HBA1",   # alpha-globin
  "HBA2",   # alpha-globin
  "HBB",    # beta-globin (mutated in sickle cell & beta-thalassaemia)
  "HBD",    # delta-globin
  "HBG1",   # gamma-globin

  # Erythropoiesis regulation
  "EPOR",   # erythropoietin receptor
  "EPO",    # erythropoietin (growth factor)
  "GATA1",  # transcription factor for erythroid lineage
  "KLF1",   # regulates globin switching and erythroid maturation
  "TAL1",   # transcription factor for haematopoiesis

  # RBC membrane / cytoskeleton (haemolytic anaemias)
  "ANK1",   # ankyrin (hereditary spherocytosis)
  "SPTA1",  # alpha-spectrin
  "SPTB",   # beta-spectrin
  "SLC4A1", # band 3 anion exchanger
  "EPB42",  # protein 4.2

  # Iron metabolism (iron-deficiency and sideroblastic anaemias)
  "SLC11A2", # DMT1, iron transporter
  "TFR2",    # transferrin receptor 2
  "HFE",     # iron regulation
  "ALAS2",   # first enzyme in haem biosynthesis (sideroblastic anaemia)
  "SLC25A38" # mitochondrial glycine transporter (sideroblastic anaemia)
)
mucin<-c("GALNT7", "ST3GAL1", "ST3GAL2")
Genes_interest<-mucin
library(pheatmap)

# 1) Subset to genes of interest
expr_mat <- data_sorted[data_sorted$gene_id %in% Genes_interest, ]
rownames(expr_mat) <- expr_mat$gene_id

# 2) Keep only expression columns (your case: 2:9)
#expr_mat <- log(expr_mat[, 2:9]+1)
expr_mat <- (expr_mat[, 2:9])
# 3) Coerce to numeric safely
#xpr_mat <- as.data.frame(lapply(expr_mat, function(x) as.numeric(as.character(x))))
expr_mat <- as.matrix(expr_mat)

# 4) Build grouping, set desired order: low then high
grp <- factor(colinfo$group, levels = c("Low plastic", "High plastic"))
stopifnot(length(grp) == ncol(expr_mat))  # quick sanity check

# 5) Reorder columns by group
ord <- order(grp, na.last = TRUE)
expr_mat <- expr_mat[, ord, drop = FALSE]
grp <- grp[ord]

# 6) Annotation in the same order
annotation_col <- data.frame(Group = grp)
rownames(annotation_col) <- colnames(expr_mat)

# 7) Optional gap between groups
gap_after <- sum(grp == levels(grp)[1])  # number of low plastic samples

# 8) Plot
pheatmap(expr_mat,
         annotation_col = annotation_col,
         scale = "row",
         cluster_rows = TRUE,
         cluster_cols = FALSE,      # keep your low, then high order
         gaps_col = gap_after,      # visual separator between groups
         show_rownames = TRUE,
         show_colnames = TRUE)

