###############################################################################
###############################################################################
###                   DESAFIO INDICIUM - CIENCIA DE DADOS                   ###
###                                                                         ### 
### Análises exploratórias, estatística e construção do modelo preditivo    ###
###                                                                         ###
### Caio Victor da Paz e Figueiredo                                         ###
### caiovictor.paz@gmail.com                                                ###
###                                                                         ###
###############################################################################
###############################################################################



## Pacotes necessários: 
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tm)
library(xgboost)
library(knitr)



######## Abrindo o arquivo ######  


setwd("C:/Users/caiov/Desktop/indicium")
## substituir pelo caminho do arquivo ##

  
dados <- read_csv("desafio_indicium_imdb.csv")

####### Limpeza dos dados 

#### ttransformando a coluna Runtime em numerica
dados$Runtime <- as.numeric(gsub(" min", "", dados$Runtime))

#### removendo as vírgulas e transformando em numerica
dados$Gross <- as.numeric(gsub(",", "", dados$Gross))

#### Transformando em numerico
dados$Released_Year <- as.numeric(as.character(dados$Released_Year))

glimpse(dados)

####  Verificando presença de NAs

sapply(dados, function(x) sum(is.na(x))) %>% 
  kable(col.names = c("Valores Ausentes"))



#### Análise Exploratória (EDA)
################################################################################ 

# Histograma das Notas IMDb
nota_imdb <- ggplot(dados, aes(x = IMDB_Rating)) +
  geom_histogram(binwidth = 0.1, fill = "royalblue", color = "black", alpha = 0.7) +
  labs(title = "Distribuição das Notas IMDb", x = "Nota IMDb", y = "Frequência") +
  theme_minimal()
print(nota_imdb)

# Histograma do Faturamento (Gross)
faturamento <- ggplot(dados %>% filter(!is.na(Gross)), aes(x = Gross / 1e6)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribuição do Faturamento (Gross)", x = "Faturamento (em milhões de USD)", y = "Frequência") +
  theme_minimal()
print(faturamento)

#### Gráfico de barras Diretores
diretores <- dados %>%
  count(Director, sort = TRUE) %>%
  top_n(10, n)

Lista_diretores <- ggplot(diretores, aes(x = reorder(Director, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Diretores com Mais Filmes na Lista", x = "Diretor", y = "Número de Filmes") +
  theme_minimal()
print(Lista_diretores)

#### Gráfico dos Gêneros frequentes
Generos <- dados %>%
  separate_rows(Genre, sep = ", ") %>%
  count(Genre, sort = TRUE) %>%
  top_n(10, n)

Lista_generos <- ggplot(Generos, aes(x = reorder(Genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Gêneros Mais Comuns", x = "Gênero", y = "Frequência") +
  theme_minimal()
print(Lista_generos)

###### Análises estatísticas
################################################################################ 

####  Faturamento pela nota IMDb
p1 <- ggplot(dados %>% filter(!is.na(Gross)), aes(x = IMDB_Rating, y = Gross)) +
  geom_point(alpha = 0.5, color = "navy") +
  scale_y_log10() +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Nota IMDb vs. Faturamento", x = "Nota IMDb", y = "Faturamento (Log)") +
  theme_minimal()
print(p1)

cor_1 <- cor.test(dados$IMDB_Rating, dados$Gross)
cor_1

#### Relação entre faturamento e diretor
p2 <- ggplot(df_diretores, aes(x = fct_reorder(Director, Gross, .fun = median, .desc = TRUE), y = Gross/1e6)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Relação entre Faturamento e Diretor",
       subtitle = "Diretores com mais filmes",
       x = "Diretor", y = "Faturamento Bruto ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

mod2 <- kruskal.test(Gross ~ Director, data = df_diretores)
mod2

#### Relação entre Nota imdb e diretor
p3 <- ggplot(df_diretores, aes(x = fct_reorder(Director, IMDB_Rating, .fun = median, .desc = TRUE), y = IMDB_Rating)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Nota do IMDb por Diretor",
       x = "Diretor", y = "Nota do IMDb") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

anova_nota <- aov(IMDB_Rating ~ Director, data = df_diretores)
anova_nota

#### Relação diretor e numero de votos
p4 <- ggplot(df_diretores, aes(x = fct_reorder(Director, No_of_Votes, .fun = median, .desc = TRUE), y = No_of_Votes/1000)) +
  geom_boxplot(fill = "lightcoral", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  scale_y_log10() +
  labs(title = "Log de Votos por Diretor",
       x = "Diretor", y = "Número de Votos (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p4)

anova_voto <- aov(No_of_Votes ~ Director, data = df_directors)
anova_voto

#### Relação entre Numero de votos e faturamento
p6 <- ggplot(dados, aes(x = No_of_Votes/1000, y = Gross/1e6)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relação entre Número de Votos e Faturamento",
       subtitle = paste("Correlação:", round(cor(dados$No_of_Votes, dados$Gross, use = "complete.obs"), 2)),
       x = "Número de Votos", y = "Faturamento Bruto") +
  theme_minimal()
print(p6)

cor_6 <- cor.test(dados$No_of_Votes, dados$Gross)
cor_6

#### Relação entre Gênero e Faturamento

# Extrair o primeiro gênero de cada filme

dados <- dados %>%
  mutate(
    Genero_principal = sapply(strsplit(Genre, ","), function(x) {
      if(length(x) > 0) trimws(x[1]) else NA
    })
  ) %>%
  filter(!is.na(Genero_principal))

generos_frequentes <- dados %>%
  count(Genero_principal) %>%
  top_n(10, n) %>%
  pull(Genero_principal)

dados_generos <- dados %>%
  filter(Genero_principal %in% generos_frequentes)

p11 <- ggplot(dados_generos, aes(x = fct_reorder(Genero_principal, Gross, .fun = median, .desc = TRUE), y = Gross/1e6)) +
  geom_boxplot(fill = "lightgoldenrod", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Relação entre Gênero e Faturamento",
       subtitle = "Gêneros mais comuns",
       x = "Gênero Principal", y = "Faturamento Bruto") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p11)

mod2 <- kruskal.test(Gross ~ Primary_Genre, data = df_clean)
mod2

#### 3.6 Relação entre Gênero e Número de Votos
# 3.6 Relação entre Gênero e Número de Votos
p12 <- ggplot(dados_generos, aes(x = fct_reorder(Genero_principal, No_of_Votes, .fun = median, .desc = TRUE), y = No_of_Votes/1000)) +
  geom_boxplot(fill = "plum", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  scale_y_log10() +
  labs(title = "Relação entre Gênero e Número de Votos",
       subtitle = "Gêneros mais comuns (log)",
       x = "Gênero Principal", y = "Número de Votos (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p12)

mod3 <- kruskal.test(No_of_Votes ~ Primary_Genre, data = df_clean)

#### Regressão Linear Multipla

modelo_linear <- lm(Gross ~ IMDB_Rating + Meta_score + Runtime + No_of_Votes, data = dados)
summary(modelo_linear))

###### recomendação de filme
################################################################################ 

recomendacao <- dados %>%
  filter(!is.na(Gross)) %>%
    filter(IMDB_Rating >= 8.5, No_of_Votes > 1500000) %>%
  arrange(desc(IMDB_Rating), desc(No_of_Votes)) %>%
  head(1)

recomendacao$Series_Title

###### Construção do modelo preditivo
################################################################################

#### peparação dos Dados
mod <- dados %>%
  select(IMDB_Rating, Meta_score, No_of_Votes, Gross, Runtime, Released_Year, Director, Genre)

#### Imputação de NAs (usando a media)
mod$Meta_score[is.na(mod$Meta_score)] <- mean(mod$Meta_score, na.rm = TRUE)
mod$Gross[is.na(mod$Gross)] <- mean(mod$Gross, na.rm = TRUE)
mod$Runtime[is.na(mod$Runtime)] <- mean(mod$Runtime, na.rm = TRUE)

#### Transformando variáveis 
mod <- mod %>%
  mutate(
    log_votes = log1p(No_of_Votes),
    log_gross = log1p(Gross),
    movie_age = 2025 - Released_Year
   )

#### Divisão de treino
set.seed(09) 
indices <- sample.int(n = nrow(mod), size = floor(0.8 * nrow(mod)))
dados_treino <- mod[indices, ]
dados_teste <- mod[-indices, ]

#### Target Encoding para as variáveis categóricas
dir <- dados_treino %>%
  group_by(Director) %>%
  summarise(director_encoded = mean(IMDB_Rating))

dados_treino$Genero_principal <- sapply(strsplit(dados_treino$Genre, ","), `[`, 1)
dados_teste$Genero_principal <- sapply(strsplit(dados_teste$Genre, ","), `[`, 1)

gen <- dados_treino %>%
  group_by(Genero_principal) %>%
  summarise(genre_encoded = mean(IMDB_Rating))


# Aplicando dados de treino e teste
dados_treino <- dados_treino %>%
  left_join(dir, by = "Director") %>%
  left_join(gen, by = "Genero_principal")

dados_teste <- dados_teste %>%
  left_join(dir, by = "Director") %>%
  left_join(gen, by = "Genero_principal")

# Preencher NAs no teste 
dados_treino$director_encoded[is.na(dados_treino$director_encoded)] <- mean(dados_treino$IMDB_Rating)
dados_treino$genre_encoded[is.na(dados_treino$genre_encoded)] <- mean(dados_treino$IMDB_Rating)
dados_teste$director_encoded[is.na(dados_teste$director_encoded)] <- mean(dados_treino$IMDB_Rating) # usa a média do treino
dados_teste$genre_encoded[is.na(dados_teste$genre_encoded)] <- mean(dados_treino$IMDB_Rating) 


#### Treinando o modelo de Gradient Boosting 
features <- c("Meta_score", "Runtime", "log_votes", "log_gross", "movie_age", "director_encoded", "genre_encoded")
target <- "IMDB_Rating"

dtreino <- xgb.DMatrix(data = as.matrix(dados_treino[, features]), label = dados_treino[[target]])
dteste <- xgb.DMatrix(data = as.matrix(dados_teste[, features]), label = dados_teste[[target]])

xgb_model <- xgb.train(
  data = dtreino,
  nrounds = 100,
  verbose = 0
)

#### Realizando as previsões 
predictions <- predict(xgb_model, dteste)

#### Performance do modelo
rmse <- sqrt(mean((dados_teste[[target]] - predictions)^2))

rmse

#### nota IMDb filme 
################################################################################

shawshank <- dados %>%
  filter(Series_Title == "The Shawshank Redemption")

#### Verificar se o filme está presente no dataset
if(nrow(shawshank) > 0) {
  # Exibir a nota do IMDb
  nota_imdb <- shawshank$IMDB_Rating
  nota_imdb
  
#### aplicando no modelo
  
#### Preparar dados do "The Shawshank Redemption"
filme <- data.frame(
Series_Title = "The Shawshank Redemption",
Released_Year = 1994,
Certificate = "A",
Runtime = 142,
Genre = "Drama",
Meta_score = 80,
Director = "Frank Darabont",
Star1 = "Tim Robbins",
Star2 = "Morgan Freeman",
Star3 = "Bob Gunton",
Star4 = "William Sadler",
No_of_Votes = 2343110,
Gross = 28341469,
stringsAsFactors = FALSE
)  
  

# Aplicar as mesmas transformações
filme<- filme %>%
  mutate(
    log_votes = log1p(No_of_Votes),
    log_gross = log1p(Gross),
    movie_age = 2025 - Released_Year,
    Genero_principal = "Drama"
  )

# Aplicar target encoding
filme <- filme %>%
  left_join(dir, by = "Director") %>%
  left_join(gen, by = "Genero_principal")

if(is.na(filme$director_encoded)) {
  filme$director_encoded <- mean(dados_treino$IMDB_Rating)
}

if(is.na(filme$genre_encoded)) {
  filme$genre_encoded <- mean(dados_treino$IMDB_Rating)
}

####Preparar matriz para predição
nota <- xgb.DMatrix(data = as.matrix(filme[, features]))

nota_prevista <- predict(xgb_model, nota)

nota_prevista
