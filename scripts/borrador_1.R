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

verduras_almidon <- ensanut_100k %>%
  filter(numero_alimento %in% c("papas_a", "papas_b"))

cereales_integrales <- ensanut_100k %>%
  filter(numero_alimento %in% c("arroz", "pan_blanco", "pan_integral",
                                "pan_dulce", "donas_churros", "galletas_saladas",
                                "cereal_caja_basico", "cereal_caja_chocolate",
                                "cereal_caja_endulzado", "cereal_caja_especialidades",
                                "cereal_caja_fibra", "cereal_caja_multiingredientes"))

frutas <- ensanut_100k %>%
  filter(numero_alimento %in% c("durazno_melocoton", "fresa", "guayaba", "mango",
                                "manzana_pera", "melon_sandia", "naranja_mandarina",
                                "papaya", "piña", "platano", "toronja", "uvas",
                                "frutas_cristalizadas_secas"))

proteina_vegetal <- ensanut_100k %>%
  filter(numero_alimento %in% c("frijoles_olla", "frijoles_refritos", "frijoles_olla_lata",
                                "frijoles_refritos_lata", "lenteja_garbazo_alubia",
                                "cacahuates_habas"))


leche <- ensanut_100k %>%
  filter(numero_alimento %in% c("otra_leche_a", "otra_leche_b", "otra_leche_c",
                                "yogur_bajo_grasa", "yogur_beber",
                                "yogur_beber_bajo_grasa", "yogur_beber_fruta",
                                "yogur_frutas", "yogur_natural", "danonino", "yakult",
                                "leche_materna"))
