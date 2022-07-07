# automatizando la funcion

# boxplot a nivel republica

eat_general_plot <- function(x){
  x %>%
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
}

eat_general_plot(x = frutas)


# boxplot por zona (norte, sur, centro)

eat_zone_plot <- function(x, zone){
  x %>%
    filter(region_nutricion == zone) %>%
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
}

eat_zone_plot(x = frutas, zone = "norte")

