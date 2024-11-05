# Resumos
### P1 - 2023
### Limpando o ambiente para não haver conflitos com variáveis já existentes
rm(list = ls())
# Questão 1

# Criando vetores para criar as matrizes.
A <- matrix(c(28, 32, 8, 9, 49, 7, 21, 35, 28, 10, 47, 43, 15, 34, 2, 48, 42, 19, 32, 26, 45, 44, 39, 50, 26), 
            ncol = 5,
            byrow = TRUE)
A

B <- matrix(c(0, 26, 3, 8, 30, 35, 12, 19, 27, 27, 27, 24, 12, 17, 29, 31, 36, 40, 35, 8, 24, 43, 31, 21, 39),
            ncol = 5,
            byrow = TRUE)
B            

## MULTIPLICAÇÃO DE MATRIZES SEMPRE COM %*%
ptransposta_AB <- tcrossprod(A, B) 
C <- solve(ptransposta_AB)
C

ptransposta_B <- crossprod(B, B)
ptransposta_B
D <- B %*% ptransposta_B
D
P <- tcrossprod(D, B)
P

# Item a) FALSO
auto_vv <- eigen(P) ## eigen calcula autovalores e autovetores
auto_vv
auto_vet <- auto_vv$vectors ## assim eu seleciono apenas o que me interessa
auto_vet
soma_auto_vet <- sum(auto_vet)
soma_auto_vet

# Item b) VERDADEIRO
diagonal_C <- diag(C) ## diag pega a diagonal da matriz
soma_diagonal_C <- sum(abs(diagonal_C)) ## abs pega o valor absoluto e sum faz a soma desses valores 
soma_diagonal_C

# Item c) Falso
A_Inferior <- lower.tri(A) ## lower.tri pega a parte inferior da matriz
A_Inferior
A[lower.tri(A)] ## devolve os valores da parte inferior da matriz
soma_triangA <- sum(A[lower.tri(A)]) ## soma os valores da parte inferior da matriz
soma_triangA

# Item d) VERDADEIRA
determinante_A <- det(A) ## det calcula o determinante
determinante_A <- abs(determinante_A) ## abs pega o valor absoluto
determinante_A
log_determinante_A <- log10(determinante_A) ## log10 calcula o logaritmo na base 10
log_determinante_A

determinante_B <- det(B)
determinante_B <- abs(determinante_B)
determinante_B
log_determinante_B <- log10(determinante_B)
log_determinante_B

pm_AB <- A %*% B ## %*% faz a multiplicação de matrizes
determinante_pm_AB <- det(pm_AB) 
determinante_pm_AB <- abs(determinante_pm_AB)
determinante_pm_AB
log_determinante_pm_AB <- log10(determinante_pm_AB)
log_determinante_pm_AB

# Item e) FALSO
ptransposta_AB
E <- solve(ptransposta_AB) ## solve calcula a inversa da matriz
E
diag_E <- diag(E)
diag_E
max(diag_E) ## max pega o maior valor do vetor


# Questão 2 
dados_choc <- read.csv("C:\\Users\\lispo\\Downloads\\chocolate.csv")
dados_choc
library(dplyr)
library(stringr)

# Item a) INCORRETO
dados_choc$local_compania <- tolower(dados_choc$local_compania) ## tolower deixa tudo em minúsculo
dados_choc <- dados_choc %>% 
  mutate(local_compania = str_remove_all(local_compania, "[.,]")) ## str_remove_all remove os caracteres que estão dentro do colchetes

any(is.na(dados_choc$local_compania)) ## any verifica se há algum NA
países_produção <- unique(dados_choc$local_compania) ## unique pega os valores únicos
países_produção
n_países_produção <- length(países_produção) ## length pega o tamanho do vetor
n_países_produção

# Item b) CORRETA

sub_tabela <- dados_choc %>% 
  filter(str_detect(ingredientes, "4")) ## str_detect verifica se há o número 4 na coluna ingredientes

sub_tabela2 <- sub_tabela %>%
  filter(str_count(caracteristicas, ",") == 1) ## str_count conta quantas vezes a vírgula aparece na coluna caracteristicas e devolve quando for o valor pedido
sub_tabela2

n_choc <- nrow(sub_tabela2) ## nrow pega o número de linhas
n_choc

# Item c) INCORRETA

sub_tabela3 <- dados_choc %>%
  filter(str_detect(ingredientes, "5"))
n_choc2 <- nrow(sub_tabela3)
n_choc2

# Item d) INCORRETO
sub_tabela4 <- dados_choc %>%
  filter(str_detect(caracteristicas, "sweet|nutty|cocoa|roasty|creamy|earthy|sandy|fatty")) ## | é o operador lógico OU
n_choc3 <- nrow(sub_tabela4)
n_choc3

# Item e) INCORRETO

sub_tabela5 <- dados_choc %>%
  filter(str_detect(ingredientes, "S\\*")) ## \\ é para escapar o * e entender como um caractere normal
n_choc4 <- nrow(sub_tabela5)
n_choc4

# Questão 3
install.packages("data.table")
install.packages("tidyr")
library(data.table)
library(dplyr)
require(data.table)
require(dplyr)
require(tidyr)
dados_art <- read.csv("/home/est/gfp24/Documentos/Art.csv.gz") 
dados_moma <- fread("/home/est/gfp24/Documentos/Art_Moma.csv.gz")

# Item a) 
exp_art <- dados_moma[, .(total_apresentacoes = sum(whitney_count_to_year)), by = artist_unique_id] ## sum faz a soma dos valores e o by especifica que é de cada id
top_3art <- exp_art[order(-total_apresentacoes)][1:3] ## order ordena os valores e o - é para ordenar do maior para o menor
top_3art
sub_dadosart <-  dados_art %>%
  filter(artist_unique_id %in% c("96", "145", "365")) ## filter filtra os valores que estão no vetor
sub_dadosart

unique(sub_dadosart$artist_name) ## unique pega os valores únicos


# Item b)
require(stringr)
unique(dados_art$artist_nationality)
sub_nac <- dados_art %>%
  filter(artist_nationality %in% c("Swiss","Mexican","Japanese"))
nrow(sub_nac)

# Item c)
nac_dad <- dados_art %>%
  filter(artist_nationality %in% c("Swiss"))

exp_dad <- dados_moma %>%
  filter(artist_unique_id %in% c("27", "36", "132", "174", "345", "202" ))

exp_dad2 <- exp_dad[, .(total_apresentacoes = sum(whitney_count_to_year)), by = artist_unique_id]
exp_dad2

total <- exp_dad2[total_apresentacoes <= 2]
total
nrow(total)

print ("pronto")
print("oi")
print (2+3)
print (2*3)
