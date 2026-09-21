# Script R para Análise Estatística Associativa de Dados de Saúde do Trabalhador (SmartLab/INSS)

Este script foi desenvolvido para servir como um guia didático completo para iniciantes na análise de dados com R. Ele cobre desde a importação e organização dos dados até a aplicação de testes estatísticos de associação (normalidade, correlação de Pearson, Spearman, Kendall e teste de Wilcoxon), com explicações linha a linha e boas práticas de visualização. O exemplo utiliza os dados de perfil de doenças e ocupações do SmartLab, mas a lógica pode ser adaptada para qualquer conjunto de dados.

------------------------------------------------------------------------

## 1. Configuração do ambiente

Antes de começar, é importante entender **por que** seguimos uma ordem específica na análise:

1.  **Verificar a normalidade dos dados** – Muitos testes estatísticos (como a correlação de Pearson) assumem que os dados seguem uma distribuição normal. Se essa suposição for violada, precisamos de alternativas não paramétricas (Spearman, Kendall, Wilcoxon etc.);
2.  **Escolher o teste adequado** – A normalidade define se usaremos um teste paramétrico ou não paramétrico;
3.  **Interpretar os resultados** – Saber o que o p-valor e o coeficiente de correlação significam na prática, considerando os limites de cada teste estatístico.

Vamos começar instalando e carregando os pacotes (extensões) necessários. O `tidyverse` é uma coleção de pacotes que facilita a manipulação e visualização de dados. O `ggpubr` e `ggpmisc` ajudam a adicionar equações e p-valores em gráficos. O `nortest` oferece testes de normalidade adicionais. O `corrplot` é útil para matrizes de correlação.

``` r
# -------------------------------------------------------------------------
# 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES
# -------------------------------------------------------------------------

# A função `if (!require(...))` verifica se o pacote está instalado.
# Se não estiver, ele instala. Isso evita erros caso o pacote já exista.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("ggpmisc")) install.packages("ggpmisc")
if (!require("nortest")) install.packages("nortest")
if (!require("corrplot")) install.packages("corrplot")
if (!require("readxl")) install.packages("readxl")   # Para ler arquivos Excel (.xlsx)
if (!require("writexl")) install.packages("writexl") # Para exportar dados para Excel

# Carregando os pacotes na sessão atual
library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(nortest)
library(corrplot)
library(readxl)
library(writexl)
```

**Dica:** Sempre execute este bloco no início de cada sessão de trabalho. Se você reiniciar o R, precisará sempre carregar os pacotes novamente.

------------------------------------------------------------------------

## 2. Importação e organização dos dados

Nesta seção, importamos os dados do arquivo `perfil_de_doenças_dados.xlsx`. O arquivo tem duas tabelas lado a lado: uma para afastamentos acidentários (B91) e outra para não acidentários (B31). Precisamos organizá-las em um formato **tidy** (longo), onde cada linha é uma observação e cada coluna é uma variável.

``` r
# -------------------------------------------------------------------------
# 2. IMPORTAÇÃO E ORGANIZAÇÃO DOS DADOS
# -------------------------------------------------------------------------

# IMPORTANTE: a planilha tem DUAS linhas de cabeçalho. A primeira tem os títulos
# mesclados ("Afastamentos B91" e "Afastamentos B31") e a segunda tem "Doença"/"Quantidade".
# Se lermos do jeito padrão, o `read_excel` usa a 1ª linha como nome das colunas e os
# valores de "Quantidade" viram TEXTO, o que quebra os gráficos com escala log
# (erro "Error in log(x, base): non-numeric argument to mathematical function").
# Solução: `skip = 1` pula a 1ª linha e `col_names = FALSE` evita usar cabeçalho.
dados_brutos <- read_excel("perfil_de_doenças_dados.xlsx", sheet = 1,
                           skip = 1, col_names = FALSE)
# O símbolo "<-" significa recebe, indica que estamos atribuíndo objetos a um determinado vetor, chamado de dataframe.

# Vamos inspecionar as primeiras linhas para entender a estrutura.
head(dados_brutos)
# A estrutura esperada é:
# Colunas 1 e 2: Doença e Quantidade para B91
# Colunas 4 e 5: Doença e Quantidade para B31
# A coluna 3 fica vazia (separador entre os dois blocos).
# A primeira linha de dados ainda é o cabeçalho repetido ("Doença"/"Quantidade"),
# por isso ela é removida logo abaixo com o filter().

# Para facilitar, vamos renomear as colunas e separar os dois blocos.
# O operador `%>%` (pipe ou mutate) encadeia operações: o resultado de uma função vira o input da próxima.
dados_b91 <- dados_brutos %>%
  select(Doenca_B91 = 1, Quantidade_B91 = 2) %>%  # Seleciona a 1ª e 2ª colunas
  filter(!is.na(Doenca_B91)) %>%                  # Remove linhas onde a doença está vazia
  filter(Doenca_B91 != "Doença") %>%              # Remove a linha de cabeçalho repetido
  mutate(
    Categoria = "B91",                             # Cria uma coluna indicando a categoria
    Quantidade_B91 = as.numeric(Quantidade_B91)    # Converte de texto para número (ESSENCIAL)
  )

dados_b31 <- dados_brutos %>%
  select(Doenca_B31 = 4, Quantidade_B31 = 5) %>%
  filter(!is.na(Doenca_B31)) %>%
  filter(Doenca_B31 != "Doença") %>%
  mutate(
    Categoria = "B31",
    Quantidade_B31 = as.numeric(Quantidade_B31)    # Converte de texto para número (ESSENCIAL)
  )

# Agora unimos os dois data frames em um só, usando `bind_rows`.
# Isso cria uma estrutura tidy: uma linha por doença/categoria.
dados_tidy <- bind_rows(
  dados_b91 %>% rename(Doenca = Doenca_B91, Quantidade = Quantidade_B91),
  dados_b31 %>% rename(Doenca = Doenca_B31, Quantidade = Quantidade_B31)
)

# Vamos verificar o resultado
glimpse(dados_tidy)
# A função `glimpse` mostra a estrutura: tipo de cada coluna e as primeiras linhas.
# CONFIRA: a coluna Quantidade deve aparecer como <dbl> (número), e NÃO como <chr> (texto).
stopifnot(is.numeric(dados_tidy$Quantidade))  # trava aqui se a conversão não funcionou

```

**Explicação do que foi feito:** - `read_excel(skip = 1, col_names = FALSE)` ignora o cabeçalho mesclado da planilha. - `select()` escolhe colunas. - `mutate()` cria novas colunas e converte a `Quantidade` de texto para número com `as.numeric()`. - `filter()` remove linhas indesejadas. - `bind_rows()` empilha data frames. - `glimpse()` é como um “raio-X” da estrutura dos dados. - `stopifnot()` faz o script parar com um erro claro caso a coluna não seja numérica.

**Atenção (causa dos erros mais comuns):** a planilha original tem **duas linhas de cabeçalho**. Se você ler sem `skip = 1`, a coluna `Quantidade` vira texto e você receberá erros como `Error in log(x, base): non-numeric argument to mathematical function` (ao usar `scale_y_log10()` nos gráficos) e `is.numeric(x) não é TRUE` (no `shapiro.test()`). A conversão com `as.numeric()` resolve.

**Atenção:** Se seus dados do SmartLab tiverem mais colunas (por exemplo, ocupação, atividade econômica), você pode adaptar este código para incluí-las. A lógica é sempre a mesma: cada linha deve ser uma observação completa.

------------------------------------------------------------------------

## 3. Padronização das nomenclaturas

O relatório aponta que algumas categorias aparecem com **nomes diferentes para a mesma doença** — por exemplo, “Depressões e Episódios Depressivos” e “Episódios Depressivos e Depressões” (mesmas palavras, ordem trocada). Se não padronizarmos, o R trata cada grafia como uma doença distinta, o que **distorce as contagens, os gráficos e todos os testes** (correlação, Wilcoxon etc.). Também há erros de digitação, como “Stress e **Adtaptação**” (o correto é “Adaptação”).

A boa prática é criar uma **tabela de-para** (um dicionário de nomes) e aplicá-la logo após a montagem dos dados, **antes** de qualquer análise.

``` r
# -------------------------------------------------------------------------
# 3. PADRONIZAÇÃO DAS NOMENCLATURAS
# -------------------------------------------------------------------------

# 1) Dicionário de nomes: cada "Nome_Original" vira um "Nome_Padronizado".
#    Use sempre a forma mais completa e correta como nome final.
padronizacao <- tribble(
  ~Nome_Original,                        ~Nome_Padronizado,
  "Episódios Depressivos e Depressões",  "Depressões e Episódios Depressivos",  # ordem trocada
  "Stress e Adtaptação",                 "Stress e Adaptação"                   # erro de digitação
)

# 2) Aplicamos o de-para aos dados.
dados_tidy <- dados_tidy %>%
  left_join(padronizacao, by = c("Doenca" = "Nome_Original")) %>%  # junta o nome padronizado
  mutate(
    Doenca = ifelse(is.na(Nome_Padronizado), Doenca, Nome_Padronizado)  # mantém o original se não houver regra
  ) %>%
  select(-Nome_Padronizado)                                        # remove a coluna auxiliar

# 3) Somamos as quantidades das linhas que ficaram com o MESMO nome
#    dentro da MESMA categoria (B91 ou B31) — é aqui que as duplicatas se fundem.
dados_tidy <- dados_tidy %>%
  group_by(Doenca, Categoria) %>%
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE), .groups = "drop")

# 4) Conferindo o resultado (as duas grafias antigas devem ter virado uma só)
dados_tidy %>%
  filter(str_detect(Doenca, "Depress|Stress")) %>%
  arrange(Doenca, Categoria) %>%
  print()

# Verificação: não deve sobrar nenhum nome da lista de originais.
stopifnot(!any(dados_tidy$Doenca %in% padronizacao$Nome_Original))

# Exportando os dados já padronizados para um CSV, que será usado no Flourish.
write_csv(dados_tidy, "dados_smartlab_tidy.csv")
```

**Resultado esperado:** as duas grafias de depressão são fundidas em uma só por categoria: B91 = 50 + 12 = **62** e B31 = 1399 + 406 = **1805**. O total de linhas cai de 158 para **156**.

**Dica:** sempre que encontrar novas duplicatas ou erros de digitação, basta acrescentar uma linha na `tribble()`. Esse dicionário documenta as decisões de padronização do projeto.

**Cuidado:** padronize **antes** de criar `dados_correlacao` (seção 6) e antes de rodar os testes. Se padronizar depois, o `pivot_wider()` terá criado linhas separadas para cada grafia e a correlação ficará incorreta.

------------------------------------------------------------------------

## 4. Análise exploratória inicial

Antes de qualquer teste estatístico, é fundamental **explorar** os dados para entender sua distribuição, valores ausentes e possíveis outliers (dispersão dos dados)

``` r
# -------------------------------------------------------------------------
# 4. ANÁLISE EXPLORATÓRIA
# -------------------------------------------------------------------------

# Resumo estatístico das quantidades por categoria
resumo_geral <- dados_tidy %>%
  group_by(Categoria) %>%  # Agrupa por B91 ou B31
  summarise(
    n = n(),                          # Número de doenças listadas
    media = mean(Quantidade),         # Média das quantidades
    mediana = median(Quantidade),     # Mediana
    desvio = sd(Quantidade),          # Desvio padrão
    min = min(Quantidade),            # Valor mínimo
    max = max(Quantidade)             # Valor máximo
  )

print(resumo_geral)

# Verificando valores ausentes
colSums(is.na(dados_tidy))  # Conta NAs por coluna

# Visualização inicial: boxplot das quantidades por categoria
ggplot(dados_tidy, aes(x = Categoria, y = Quantidade, fill = Categoria)) +
  geom_boxplot() +
  scale_y_log10() +  # Escala logarítmica para lidar com a grande variação
  labs(
    title = "Distribuição das Quantidades de Afastamentos por Categoria",
    x = "Categoria (B91 = Acidentário, B31 = Não Acidentário)",
    y = "Quantidade (escala log)"
  ) +
  theme_minimal()

```

**O que observar:** - A média é muito diferente da mediana? Isso sugere assimetria. - Há valores extremos (outliers)? A escala logarítmica ajuda a visualizá-los. - O desvio padrão é grande em relação à média? Indica alta variabilidade.

------------------------------------------------------------------------

## 5. Teste de Normalidade (Shapiro-Wilk)

**Por que fazer isso?** O teste de Shapiro-Wilk verifica se uma amostra vem de uma população com distribuição normal. A hipótese nula (H0) é que os dados **são** normais. Se o p-valor for **maior que 0,05**, não rejeitamos a normalidade. Se for **menor que 0,05**, os dados **não** seguem uma distribuição normal, e devemos usar testes não paramétricos.

``` r
# -------------------------------------------------------------------------
# 5. TESTE DE NORMALIDADE
# -------------------------------------------------------------------------

# O teste de Shapiro-Wilk requer uma amostra entre 3 e 5000 observações.
# Vamos aplicar separadamente para cada categoria.
shapiro_b91 <- dados_tidy %>%
  filter(Categoria == "B91") %>%
  pull(Quantidade) %>%  # Extrai o vetor de quantidades
  shapiro.test()

shapiro_b31 <- dados_tidy %>%
  filter(Categoria == "B31") %>%
  pull(Quantidade) %>%
  shapiro.test()

# Exibindo os resultados
print(shapiro_b91)
print(shapiro_b31)

# Interpretação:
# Se p-value < 0,05: os dados NÃO são normais -> use testes não paramétricos.
# Se p-value >= 0,05: os dados SÃO normais -> pode usar testes paramétricos.
```

**Dica:** O teste de Shapiro-Wilk é sensível a amostras muito grandes. Para amostras \> 5000, use o teste de Anderson-Darling (`ad.test` do pacote `nortest`).

``` r
# Alternativa para amostras grandes: Anderson-Darling
ad_b91 <- dados_tidy %>%
  filter(Categoria == "B91") %>%
  pull(Quantidade) %>%
  ad.test()

ad_b31 <- dados_tidy %>%
  filter(Categoria == "B31") %>%
  pull(Quantidade) %>%
  ad.test()

print(ad_b91)
print(ad_b31)
```

------------------------------------------------------------------------

## 6. Análise de Correlação

Agora que sabemos se os dados são normais ou não, podemos escolher o coeficiente de correlação adequado. Vamos calcular **Pearson** (paramétrico, para dados normais), **Spearman** e **Kendall** (não paramétricos, para dados não normais ou ordinais).

**Importante:** Para calcular correlação, precisamos de **duas variáveis numéricas**. No nosso caso, podemos correlacionar as quantidades de B91 e B31 para cada doença. Para isso, precisamos **empilhar** os dados de forma que cada linha tenha a quantidade de B91 e B31 para a mesma doença.

``` r
# -------------------------------------------------------------------------
# 6. ANÁLISE DE CORRELAÇÃO
# -------------------------------------------------------------------------

# Primeiro, vamos criar um data frame onde cada doença é uma linha,
# com colunas separadas para B91 e B31.
dados_correlacao <- dados_tidy %>%
  pivot_wider(
    names_from = Categoria,   # Os valores de "Categoria" viram nomes de colunas
    values_from = Quantidade  # Os valores de "Quantidade" preenchem essas colunas
  ) %>%
  filter(!is.na(B91) & !is.na(B31))  # Mantém apenas doenças presentes em ambas as categorias

# Visualizando a estrutura
head(dados_correlacao)

# --- Correlação de Pearson (paramétrica) ---
# Requer normalidade bivariada e relação linear.
pearson_test <- cor.test(
  x = dados_correlacao$B91,
  y = dados_correlacao$B31,
  method = "pearson"
)
print(pearson_test)

# --- Correlação de Spearman (não paramétrica) ---
# Baseada nos ranks (postos) dos dados. Não requer normalidade.
# `exact = FALSE` evita o aviso "não é possível computar o valor de p exato com o de desempate"
# quando há valores repetidos (empates) nos dados.
spearman_test <- cor.test(
  x = dados_correlacao$B91,
  y = dados_correlacao$B31,
  method = "spearman",
  exact = FALSE
)
print(spearman_test)

# --- Correlação de Kendall (não paramétrica) ---
# Também baseada em ranks, mas mais robusta para amostras pequenas.
# `exact = FALSE` evita o aviso "não é possível computar o valor de p exato com o de desempate"
# quando há valores repetidos (empates) nos dados.
kendall_test <- cor.test(
  x = dados_correlacao$B91,
  y = dados_correlacao$B31,
  method = "kendall",
  exact = FALSE
)
print(kendall_test)

# --- Comparação dos resultados ---
# Vamos criar uma tabela resumo com os coeficientes e p-valores.
resultados_cor <- tibble(
  Metodo = c("Pearson", "Spearman", "Kendall"),
  Coeficiente = c(pearson_test$estimate, spearman_test$estimate, kendall_test$estimate),
  P_valor = c(pearson_test$p.value, spearman_test$p.value, kendall_test$p.value)
) %>%
  mutate(
    Significativo = ifelse(P_valor < 0.05, "Sim", "Não"),
    Interpretacao = case_when(
      abs(Coeficiente) >= 0.7 ~ "Forte",
      abs(Coeficiente) >= 0.4 ~ "Moderada",
      abs(Coeficiente) >= 0.2 ~ "Fraca",
      TRUE ~ "Muito fraca / inexistente"
    )
  )

print(resultados_cor)
```

**Como interpretar:**

| Coeficiente (r ou ??) | Interpretação |
|----------------------|---------------|
| 0,00 – 0,19          | Muito fraca   |
| 0,20 – 0,39          | Fraca         |
| 0,40 – 0,69          | Moderada      |
| 0,70 – 0,89          | Forte         |
| 0,90 – 1,00          | Muito forte   |

- **Sinal positivo:** quando uma variável aumenta, a outra também aumenta (proporcionais);
- **Sinal negativo:** quando uma aumenta, a outra diminui (inversamente proporcionais);
- **P-valor \< 0,05:** a correlação é estatisticamente significativa (Probabilidade de que não foi por acaso).

**Qual escolher?** Se os dados **não** são normais (como geralmente acontece com contagens de afastamentos), o **Spearman** ou **Kendall** são mais apropriados.
O Kendall é útil quando há muitos empates (valores repetidos) ou amostras pequenas.

------------------------------------------------------------------------

## 7. Teste de Wilcoxon (Mann-Whitney)

O teste de Wilcoxon (também chamado de Mann-Whitney) é usado para comparar **duas amostras independentes** quando os dados não seguem uma distribuição normal. Ele testa se as distribuições das duas amostras são iguais (H0) versus a alternativa de que uma tende a ter valores maiores que a outra.

``` r
# -------------------------------------------------------------------------
# 7. TESTE DE WILCOXON (MANN-WHITNEY)
# -------------------------------------------------------------------------

# Vamos comparar as quantidades de afastamentos entre B91 e B31.
# A hipótese nula é que as medianas das duas categorias são iguais.
# OBS.: o método de fórmula do wilcox.test() já assume amostras independentes.
# Por isso NÃO se usa `paired = FALSE` aqui (isso gera o erro
# "cannot use 'paired' in formula method"). Se quiser amostras pareadas, use x e y vetoriais.
wilcox_test <- wilcox.test(
  Quantidade ~ Categoria,  # Fórmula: variável dependente ~ variável de agrupamento
  data = dados_tidy,
  exact = FALSE,           # Usa aproximação normal (recomendado para amostras grandes)
  conf.level = 0.95
)

print(wilcox_test)

# Interpretação:
# Se p-value < 0,05: rejeitamos H0. Há diferença significativa entre as categorias.
# Se p-value >= 0,05: não rejeitamos H0. Não há evidência de diferença.
```

**O que o teste de Wilcoxon faz?** Ele converte os valores em ranks (postos) e compara a soma dos ranks entre os grupos. É uma alternativa robusta ao teste t de Student quando a normalidade não é atendida.

------------------------------------------------------------------------

## 8. Visualização dos Resultados

### 8.1 Gráfico de Dispersão com Reta de Regressão

``` r
# -------------------------------------------------------------------------
# 8. VISUALIZAÇÃO DOS RESULTADOS
# -------------------------------------------------------------------------

# Gráfico de dispersão: B91 vs B31
ggplot(dados_correlacao, aes(x = B91, y = B31)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 3) +  # Pontos
  geom_smooth(method = "lm", se = TRUE, color = "darkred") + # Reta de regressão
  scale_x_log10() +  # Escala log para melhor visualização
  scale_y_log10() +
  labs(
    title = "Correlação entre Afastamentos Acidentários (B91) e Não Acidentários (B31)",
    subtitle = "Cada ponto representa uma doença",
    x = "Quantidade de Afastamentos B91 (escala log)",
    y = "Quantidade de Afastamentos B31 (escala log)",
    caption = "Fonte: SmartLab/INSS • Elaborado no R"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )
```

### 8.2 Boxplot Comparativo (B91 vs B31)

``` r
# Boxplot comparando as distribuições
ggplot(dados_tidy, aes(x = Categoria, y = Quantidade, fill = Categoria)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Comparação das Quantidades de Afastamentos por Categoria",
    x = "Categoria",
    y = "Quantidade (escala log)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

### 8.3 Matriz de Correlação (para múltiplas variáveis)

Se você tiver mais de duas variáveis numéricas (por exemplo, quantidade por doença, por ocupação, por atividade econômica), pode criar uma matriz de correlação.

``` r
# Exemplo hipotético: suponha que você tenha um data frame com várias variáveis
# Vamos simular com os dados que temos (apenas B91 e B31)
matriz_cor <- cor(
  dados_correlacao %>% select(B91, B31),
  use = "complete.obs",
  method = "spearman"
)

corrplot(
  matriz_cor,
  method = "circle",
  type = "lower",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Matriz de Correlação de Spearman",
  mar = c(0, 0, 2, 0)
)
```

------------------------------------------------------------------------

## 9. Exportação para o Flourish

O Flourish é uma ferramenta de visualização de dados online que permite criar gráficos interativos sem programação. Para usá-lo, você precisa exportar seus dados em formato CSV (ou Excel) e fazer o upload no site.

``` r
# -------------------------------------------------------------------------
# 9. EXPORTAÇÃO PARA O FLOURISH
# -------------------------------------------------------------------------

# Exportando os dados tidy para CSV (já fizemos isso antes, mas vamos garantir)
write_csv(dados_tidy, "dados_para_flourish.csv")

# Se preferir Excel:
write_xlsx(dados_tidy, "dados_para_flourish.xlsx")

# Instruções para o Flourish:
# 1. Acesse https://flourish.studio/ e crie uma conta gratuita.
# 2. Clique em "New visualization" e escolha um template (ex.: Bar chart race, Sankey, etc.).
# 3. Na aba "Data", faça upload do arquivo CSV ou Excel que você exportou.
# 4. Mapeie as colunas: arraste "Doenca" para o eixo de categorias e "Quantidade" para o eixo de valores.
# 5. Personalize cores, títulos e legendas conforme necessário.
# 6. Publique e compartilhe o link da visualização.
```

**Dica:** O Flourish aceita URLs de dados ao vivo (por exemplo, um link raw do GitHub). Se você atualizar o CSV no GitHub, o gráfico no Flourish pode ser atualizado automaticamente.

------------------------------------------------------------------------

## 10. Dicas

1.  **Sempre comece um novo script com um cabeçalho** descrevendo o objetivo, autor e data.
2.  **Use comentários** (`#`) para explicar o que cada bloco de código faz. Isso ajuda você e outras pessoas a entenderem o script depois.
3.  **Organize seu projeto em pastas:** `Dados/` para arquivos de entrada, `Scripts/` para códigos, `Graficos/` para saídas.
4.  **Use o RStudio:** ele facilita a visualização de data frames, o gerenciamento de projetos e a execução de scripts.
5.  **Aprenda a usar o `%>%` (pipe):** ele torna o código mais legível, encadeando operações.
6.  **Verifique sempre a estrutura dos dados** com `str()`, `glimpse()` ou `head()` antes de analisar.
7.  **Cuidado com valores ausentes (NA):** muitos testes falham se houver NAs. Use `na.rm = TRUE` ou remova-os com `drop_na()`.
8.  **Não confunda correlação com causalidade:** uma correlação significativa não prova que uma variável causa a outra.
9.  **Documente suas decisões estatísticas:** por que escolheu Spearman em vez de Pearson? Por que usou Wilcoxon?
10. **Pratique com dados públicos:** o SmartLab oferece dados abertos. Explore outras localidades e categorias.

------------------------------------------------------------------------

## 11. Considerações Finais e Limitações

- **Dados do SmartLab:** O SmartLab agrega dados do INSS e do SINAN. Os dados de afastamentos (B91 e B31) são baseados em concessões de benefícios, o que pode subestimar a real prevalência de doenças ocupacionais devido à subnotificação.
- **Categorias “Outros”:** Em ambas as categorias, “Outros” representa uma parcela significativa dos afastamentos, o que indica baixa especificidade diagnóstica e dificulta análises detalhadas.
- **Nomenclaturas duplicadas:** Como observado no relatório, existem categorias com nomes semelhantes (ex.: “Depressões e Episódios Depressivos” e “Episódios Depressivos e Depressões”). Esses nomes são padronizados na **seção 3**, que cria um dicionário de-para e soma as quantidades das grafias equivalentes.
- **Causalidade:** Este script foca em **associação**, não em causalidade. Para inferir causalidade, seriam necessários estudos longitudinais com controle de variáveis de confusão.

------------------------------------------------------------------------

## 12. Livro

- **Livro “R for Data Science”:** <https://r4ds.had.co.nz/> – Excelente recurso gratuito para aprender R.
