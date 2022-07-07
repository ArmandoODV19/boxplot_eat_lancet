# arreglando problema de sopa

levels(as.factor(ensanut_100k$categoria))


ensanut_100k$categoria <- revalue(ensanut_100k$categoria,
                                  c("sopás_cremas_pastas" = "sopas_cremas_pastas"))


saveRDS(ensanut_100k, file = "clean_data/ensanut_100k.rds")
