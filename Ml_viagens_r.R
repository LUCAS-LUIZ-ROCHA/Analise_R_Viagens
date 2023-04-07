#Carregando os dados

df <- read.csv(
  file = "C:/MachineLearningR/2019Viagem1.csv",
  header = TRUE,
  sep = ';',
  dec = ',',
 
)
head(df)
View(df)
dim(df)

#Resumo do dataset - valores min, max, media, mediana...
summary(df)
summary(df$Valor.passagens)


library(dplyr)

glimpse(df)

df$data.inicio <- as.Date(df$Período...Data.de.início,"%d/%m/%y")
df$data.fim <- as.Date(df$Período...Data.de.fim,"%d/%m/%y")
glimpse(df)

df$Período...Data.de.início <- NULL
df$Período...Data.de.fim <- NULL
glimpse(df)
df$data.inicio

### EXPLORAÇÃO DOS DADOS
hist(df$Valor.passagens)

summary(df$Valor.passagens)

boxplot(df$Valor.passagens)

#Calculando o desvio padrão
sd(df$Valor.passagens)

colSums(is.na(df))

#Verifcar a quantidade de categorias da coluna Situação
df$Situação <- factor(df$Situação)

str(df$Situação)

#Verificar quantidade de registros em cada categoria
table(df$Situação)
prop.table(table(df$Situação))

#Obtendo os valores em percentual de cada categoria
prop.table(table(df$Situação))*100


### Visualização dos resultados

# 1 - Qual é o valor gasto por órgão em passagens?


#Criando um dataframe com os 15 órgãos que gastam mais
library(dplyr)
p1 <- df %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)
p1

#Alterando o nome das colunas
names(p1) <- c("orgao", "valor")

p1

#Plotando os dados
library(ggplot2)
ggplot(p1, aes(x = reorder(orgao, valor), y = valor))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Valor", y = "Órgãos")


# 2 - Qual é o valor gasto por cidade?

#Criando um dataframe com as 15 cidades que gastam mais
p2 <- df %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

p2

#Alterando o nome das colunas
names(p2) <- c("destino", "valor")
p2

#Criando o gráfico
ggplot(p2, aes(x = reorder(destino, valor), y = valor))+
  geom_bar(stat = "identity", fill = "#0ba791")+
  geom_text(aes(label = valor), vjust = 0.3, size = 3)+
  coord_flip()+
  labs(x = "Valor", y = "Destino")

# 3 - Qual é a quantidade de viagens por mês?

df$data.inicio.formatada <- format(df$data.inicio, "%Y-%m")
df$data.inicio.formatada

#Criando um dataframe com a quantidade de viagens por Ano/mês
p3 <- df %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(Identificador.do.processo.de.viagem))

head(p3)

#Criando o gráfico
ggplot(p3,aes(x=data.inicio.formatada, y=qtd, group= 1))+
  geom_line()+
  geom_point()

install.packages("rmarkdown")
install.packages('tinytex')
library(tinytex)
tinytex::install_tinytex()

