# generando los grupos

categoria_de_alimentos <- levels(as.factor(ensanut_100k$categoria))
categoria_de_alimentos <- as.data.frame(categoria_de_alimentos)

detalles_alimento <- levels(as.factor(ensanut_100k$numero_alimento))
detalles_alimento <- as.data.frame(detalles_alimento)

carnes <- ensanut_100k %>%
  filter(numero_alimento %in% c("carnes_res", "carnes_res_seca", "carne_puerco",
                                "mariscos", "pescado_fresco", "pescado_seco", "pollo_a",
                                "pollo_b", "pollo_c", "atun_sardina",
                                "queso_panela_fresco_cottage", "queso_maduro"))


vegetales <- ensanut_100k %>%
  filter(numero_alimento %in% c("brocoli_coliflor", "calabacita", "cebolla",
                                "chayote", "col", "verduras_congeladas",
                                "hojas_verdes", "ejotes", "jitomate", "lechuga",
                                "nopales", "pepino", "chile_seco", "zanahoria",
                                "chile_poblano", "jicama", "limon"))

