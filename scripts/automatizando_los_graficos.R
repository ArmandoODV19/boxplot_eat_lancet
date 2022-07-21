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



### consumo de alimentos por zona (norte, sur, centro, cdmx) ###

eat_zone_plot <- function(x, grupo, zone){

  temp1 <-  x %>%
    filter(region_nutricion == zone) %>%
    select(numero_alimento, dias_comio) %>%
    group_by(numero_alimento, dias_comio) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    filter(dias_comio != 0)

  temp2 <- ensanut_100k %>%
    filter(region_nutricion == zone) %>%
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

eat_zone_plot(vegetales, grupo = "vegetales", zone = "cdmx")


#################################################
###AGREGANDO FUNCIONES###########################
#################################################

# funcion para graficar solo los grupos de alimentos a nivel general

eat_all_plot <- function(){
  vegetales$grupo<-"vegetales"
  frutas$grupo<-"frutas"
  proteina_vegetal$grupo<-"proteina_vegetal"
  verduras_almidon$grupo<-"verduras_almidon"
  cereales_integrales$grupo<-"cereales_integrales"
  leche$grupo<-"leche"
  carnes$grupo<-"carnes"

  gruposeat<-rbind(vegetales,frutas,proteina_vegetal, verduras_almidon,cereales_integrales,leche,carnes)

  gruposeat%>%
    select(grupo, dias_comio) %>%
    group_by(grupo, dias_comio) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    filter(dias_comio != 0)%>%
    ggplot(aes(x = grupo, y = freq, fill = grupo)) +
    geom_boxplot() +
    geom_jitter(aes(color = dias_comio), size = 2)+
    xlab("Alimento")+
    ylab("Frecuencia de consumo") +
    labs(color='Consumo(días)', fill="grupo")+
    guides(fill = "none")+
    theme_classic()+
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          text = element_text(size = 20), plot.title=element_text(hjust=1))
}

eat_all_plot()
.
