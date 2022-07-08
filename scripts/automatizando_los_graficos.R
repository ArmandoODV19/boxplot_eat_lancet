# automatizando la funcion

# boxplot a nivel republica

#####################################################
#####################################################
#######IMPORTANTE LEER###############################
#####################################################
#####################################################

# LOS NOMBRES DE LAS FUNCIONES SE MANTENDRAN IGUAL, SIN EMBARGO
# SE MODIFICARAN Y SE AGREGARA EL BOXPLOT CON EL GRUPO GENERAL
# DE CADA TIPO DE ALIMENTO


#eat_general_plot <- function(x){
#  x %>%
#    select(numero_alimento, dias_comio) %>%
#    group_by(numero_alimento, dias_comio) %>%
#    summarise(n = n()) %>%
#    mutate(freq = n / sum(n)) %>%
#    filter(dias_comio != 0) %>%
#    ggplot(aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
#    geom_boxplot() +
#    geom_jitter(aes(color = dias_comio), size = 5)+
#    xlab("alimento")+
#    ylab("consumo")+
#    theme_classic()+
#    theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#    guides(fill = FALSE)
#}

#eat_general_plot(x = frutas)


# boxplot por zona (norte, sur, centro)

#eat_zone_plot <- function(x, zone){
#  x %>%
#    filter(region_nutricion == zone) %>%
#    select(numero_alimento, dias_comio) %>%
#    group_by(numero_alimento, dias_comio) %>%
#    summarise(n = n()) %>%
#    mutate(freq = n / sum(n)) %>%
#    filter(dias_comio != 0) %>%
#    ggplot(aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
#    geom_boxplot() +
#    geom_jitter(aes(color = dias_comio), size = 5)+
#    xlab("alimento")+
#    ylab("consumo")+
#    theme_classic()+
#    theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#    guides(fill = FALSE)
#}

#eat_zone_plot(x = frutas, zone = "norte")

eat_general_plot <- function(x, grupo){

  temp1 <-  x %>%
    select(numero_alimento, dias_comio) %>%
    group_by(numero_alimento, dias_comio) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    filter(dias_comio != 0)

  temp2 <- ensanut_100k %>%
    filter(numero_alimento %in% as.character(levels(as.factor(x$numero_alimento)))) %>%
    select(categoria, dias_comio) %>%
    add_column(numero_alimento = grupo) %>%
    group_by(numero_alimento, dias_comio) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    filter(dias_comio != 0)

  datamerge <- rbind(temp2, temp1)

  plot1 <- ggplot(datamerge,aes(x = numero_alimento, y = freq, fill = numero_alimento)) +
    geom_boxplot() +
    geom_jitter(aes(color = dias_comio), size = 5)+
    xlab("Alimento")+
    ylab("Frecuencia de consumo")+
    theme_classic()+
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          text = element_text(size = 20))+
    guides(fill = FALSE)

  plot1 + aes(x = fct_inorder(numero_alimento)) +
    xlab("alimento")
}

eat_general_plot(cereales_integrales, grupo = "cereales_integrales")
