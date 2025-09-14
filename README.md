# desafio_indicium
Desafio da Indicium para a carreira de ciência de dados


O desafio foi realizado em 2 programas diferentes.

Utilizei o software R para criação dos gráficos, análises estatísticas e construção do modelo preditivo, o mesmo modelo foi gerado em python

ambos os códigos estão disponíveis de forma original e em formato .pkl

INSTRUÇÕES:

-Baixe todos os arquivos em uma mesma pasta, ela deverá ser utilizada como diretório para realização da análise.
-Forneça o caminho completo da pasta, ex: C:\\Users\\SeuUsuario\\Indicium\\modelo_regressão_indicium.pkl).
-Importe as bibliotecas necessárias, ex: import pandas as pd
                                        import pickle 
-carregue o conjunto de dados desafio_indicium_imdb.csv e o arquivo .pkl, ex: # Carregando o conjunto de dados
                                                                               dados = pd.read_csv('Indicium/desafio_indicium_imdb.csv')

                                                                              # Carregando o modelo de regressão
                                                                              with open('Indicium/modelo_regressão_indicium.pkl', 'rb') as arquivo_modelo:
                                                                              modelo = pickle.load(arquivo_modelo)                                        



preparare os dados de teste (as colunas que o modelo espera) no mesmo formato que foram usados no treinamento.
EX:
caracteristicas_para_prever = dados[['Runtime', 'IMDB_Rating', 'Gross']]
# Faça as previsões usando o modelo carregado
previsoes = modelo.predict(caracteristicas_para_prever)
# Adicione as previsões ao seu DataFrame
dados['previsao_modelo'] = previsoes
# Exiba as primeiras linhas do DataFrame com as previsões
print(dados.head())

O código também funciona em google collab ou similares, para isso 

-Faça o Upload dos arquivos
   - Clique no botão "Fazer upload para o armazenamento da sessão".
   - Selecione o arquivo desafio_indicium_imdb.csv e o arquivo modelo_regresão_indicium.pkl no seu computador e faça o upload.

import pandas as pd
import pickle

# Carregando o arquivo .csv
dados = pd.read_csv('desafio_indicium_imdb.csv')
# Carregando o arquivo .pkl
with open('modelo_regresão_indicium.pkl', 'rb') as arquivo_modelo:
    modelo = pickle.load(arquivo_modelo)

Agora você pode usar o modelo.

Para rodar o projeto em R basta baixar o arquivo "desafio_indicium.R" e abri-lo em um ambiente de desenvolvimento, recomendo a utilização do Rstudio, porém o código também funciona em jupyter notebook.

No RStudio:
  -Clique em Session > Set Working Directory > Choose Directory...
  -Navegue até a pasta Indicium e selecione
Por linha de código: setwd("C:/caminho/para/Indicium")) lembre-se de colocar o caminho para a pasta correta.

no canto superior do script clique em RUN
  
  
