# generando los graficos por grupo

carnes %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count()  %>%
  ggplot(aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


carnes %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count() %>%
  ungroup() %>%
  summarise(total = sum(freq))


####


carnes %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/51240,
                                categoria == "botanas_dulces_postres" ~ n/23608,
                                categoria == "carnes_embutido_huevo" ~ n/32858,
                                categoria == "cereales_tuberculos" ~ n/26553,
                                categoria == "comida_rapida" ~ n/4434,
                                categoria == "frutas" ~ n/33736,
                                categoria == "lacteos" ~ n/23973,
                                categoria == "leguminosas" ~ n/17356,
                                categoria == "miscelaneos" ~ n/4158,
                                categoria == "pescado_mariscos" ~ n/4522,
                                categoria == "productos_maiz" ~ n/12454,
                                categoria == "sopás_cremas_pastas" ~ n/17933,
                                categoria == "suplementos" ~ n/1654,
                                categoria == "verduras" ~ n/34699,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


carnes %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count() %>%
  ungroup() %>%
  summarise(total = sum(freq))

carnes %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


### tercer intento
# este corre y es lo que necesitamos

carnes %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)

# quitando los ceros

carnes %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(dias_comio != 0) %>%
  ggplot(aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)

