# carnes

carnes %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/17019

# verduras

vegetales %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/21413

# verduras almidon

verduras_almidon %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/4685

# lacteos

leche %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/13532

# cereales integrales

cereales_integrales %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/21629

# frutas

frutas %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/17318

# proteina vegetal

proteina_vegetal %>%
  filter(tamano_porcion == "E") %>%
  select(veces_al_dia, numero_porciones) %>%
  mutate(nat = numero_porciones*100) %>%
  summarise(total = sum(nat)) %>%
  pull(total)/17804
