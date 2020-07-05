#CO_CATEGAD	C?digo da categoria administrativa da IES	

#1 = P?blica Federal
#2 = P?blica Estadual
#3 = P?blica Municipal
#4 = Privada com fins lucrativos
#5 = Privada sem fins lucrativos
#7 = Especial

#area_curso
#C?digo da ?rea de enquadramento do curso no Enade
#&CO_GRUPO==1401&
#"21 = Arquitetura e Urbanismo
#72 = Tecnologia em An?liseise e Desenvolvimento de Sistemas
#76 = Tecnologia em Gest?o da Produ??oo Industrial
#79 = Tecnologia em Redes de Computadores
#701 = Matem?tica (Bacharelado)
#702 = Matem?tica (Licenciatura)
#903 = Letras-Portuguesa (Bacharelado)
#904 = Letras-Portuguesa (Licenciatura)
#905 = Letras-Portuguesa e Ingl?s (Licenciatura)
#906 = Letras-Portuguesa e Espanhol (Licenciatura)
#1401 = F?sica (Bacharelado)
#1402 = F?sica (Licenciatura)
#1501 = Qu?mica (Bacharelado)
#1502 = Qu?mica (Licenciatura)
#1601 = Ci?ncias Biol?gicas (Bacharelado)
#1602 = Ci?ncias Biol?gicas (Licenciatura)
#2001 = Pedagogia (Licenciatura)
#2401 = Hist?ria (Bacharelado)
#2402 = Hist?ria (Licenciatura)
#2501 = Artes Visuais (Licenciatura)
#3001 = Geografia (Bacharelado)
#3002 = Geografia (Licenciatura)
#3201 = Filosofia (Bacharelado)
#3202 = Filosofia (Licenciatura)
#3502 = Educa??oo F?sica (Licenciatura)


#CO_REGIAO_CURSO	C?digo da regi?o de funcionamento do curso	
#1 = Norte
#2 = Nordeste
#3 = Sudeste
#4 = Sul
#5 = Centro-Oeste

#NU_IDADE	Idade do inscrito em 26/11/2017	min = 10  max = 95

#TP_SEXO	Tipo de sexo	M = Masculino #F = Feminino  


#CO_TURNO_GRADUACAO	C?digo do turno de gradua??o	
#1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno

#NT_GER	Nota bruta da prova - Media ponderada da forma??o geral (25%) e componente espec?fico (75%). 
#(valor de 0 a 100)	

#QE_I01	Qual o seu estado civil?	

#A = Solteiro(a).
#B = Casado(a).
#C = Separado(a) judicialmente/divorciado(a).
#D = Vi?vo(a).
#E = Outro.

#QE_I02	Qual a sua cor ou ra?a?	

#A = Branca.
#B = Preta.
#C = Amarela.
#D = Parda.
#E = Ind?gena.
#F = N?o quero declarar.

#QE_I08	
#Qual a renda total de sua fam?lia, incluindo seus rendimentos?	

#A = At? 1,5 sal?rios m?nimo (até R$ 1.405,50).
#B = De 1,5 a 3 sal?rios m?nimos (R$ 1.405,51 a R$ 2.811,00).
#C = De 3 a 4,5 sal?rios m?nimos (R$ 2.811,01 a R$ 4.216,50).
#D = De 4,5 a 6 sal?rios m?nimos (R$ 4.216,51 a R$ 5.622,00).
#E = De 6 a 10 sal?rios m?nimos (R$ 5. 622,01 a R$ 9.370,00).
#F = De 10 a 30 sal?rios m?nimos (R$ 9.370,01 a R$ 28.110,00).
#G = Acima de 30 sal?rios m?nimos (mais de R$ 28.110,00).

#QE_I21	Algu?m em sua fam?lia concluiu um curso superior?

#A = Sim.
#B = N?o.

#QE_I23 Quantas horas por semana, aproximadamente, voc? dedicou aos estudos, excetuando as horas de aula?

#A = Nenhuma, apenas assisto as aulas.
#B = De uma a tr?s.
#C = De quatro a sete.
#D = De oito a doze.
#E = Mais de doze.

library(readr)
library(ggplot2)
library(plotly)
library(e1071)
require(dplyr)
require(Hmisc)
require(esquisse)
require(devtools)

#Direcionando a pasta no diret?rio, para o R
setwd("C:\\Estatistica_R\\ENC_Materiais_e_Tutoriais")
getwd()

#Carregando o banco de dados
microdados_enade <- read.table("MICRODADOS_ENADE_2017.txt",
                               header = TRUE, 
                               sep=";", 
                               dec = ",", 
                               colClasses=c(NT_OBJ_FG="numeric"))

enade2017 = read_csv2("MICRODADOS_ENADE_2017.txt") 

View(microdados_enade)

#Verificando as dimens?es
dim(enade2017)
dim(microdados_enade)

#Selecionando as vari?veis desejadas
microdados_enade_filtrados= enade2017 %>%
  dplyr::select(CO_CATEGAD,CO_GRUPO,CO_REGIAO_CURSO,
                NU_IDADE,TP_SEXO,CO_TURNO_GRADUACAO, 
                NT_GER,QE_I01,QE_I02,
                QE_I08,QE_I21,QE_I23,
                NT_OBJ_FG,
                NT_OBJ_CE
  )      
#Vendo o nome das colunas do Data Frame
names(microdados_enade_filtrados)

#Verificando as dimens?es
dim(microdados_enade_filtrados)

#dicion?rio:
#QE_I01	Qual o seu estado civil?	
#CO_CATEGAD C?digo da categoria administrativa da IES	(P?blica Federal,estadual..Privada..)
#CO_GRUPO Curso(Matem?tica, estat?stica, psicologia..)
#CO_GRUPO (Norte, nordeste, sul..)
#CO_GRUPO (Masculino/Feminino)

#Verificando a classe das vari?veis
class(microdados_enade$QE_I01)
class(microdados_enade$CO_CATEGAD)
class(microdados_enade$CO_GRUPO)
class(microdados_enade$CO_REGIAO_CURSO)
class(microdados_enade$TP_SEXO)


#Filtrando os dados s? para os profissionais de an?lise e desenvolvimento de sistemas (t.i)
microdados_ti= microdados_enade_filtrados %>% filter(CO_GRUPO==72) 

head(microdados_ti)


#Verificando as dimens?es
dim(microdados_ti)

#Certificando que o filtro funcionou
table(microdados_ti$CO_GRUPO)

#Criando categorias na vari?vel Q1_I01(Estado civil)
microdados_ti$estado_civil=ifelse(microdados_ti$QE_I01=="A","Solteiro(a)",
                                  ifelse(microdados_ti$QE_I01=="B","Casado(a)",
                                         ifelse(microdados_ti$QE_I01=="C","Separado(a)",
                                                ifelse(microdados_ti$QE_I01=="D","Vi?vo(a)","Outro"
                                                ))))

#AJUDA A ENTENDER CASE_WHEN
professor=c("THIAGO MARQUES","ADRIANA SILVA","OUTRO","OUTRO2")
case_when(professor=="THIAGO MARQUES" ~ "AULA LEGAL",
          professor=="ADRIANA SILVA" ~ "AULA LEGAL",
          TRUE ~"CHATA")

#Criando categorias na vari?vel Q1_I01(Estado civil) no Dplyr
microdados_ti = microdados_ti %>% 
  mutate(estado_civil2 = case_when(
    QE_I01 == "A" ~ "Solteiro(a)",
    QE_I01 == "B" ~ "Casado(a)",
    QE_I01 == "C" ~ "Separado(a)",
    QE_I01 == "D" ~ "Vi?vo(a)",
    QE_I01 == "E" ~ "Outro"
  )) 

microdados_ti = microdados_ti %>% 
  mutate(regiao = case_when(
    CO_REGIAO_CURSO == 1 ~ "Norte",
    CO_REGIAO_CURSO == 2 ~ "Nordeste",
    CO_REGIAO_CURSO == 3 ~ "Sudeste",
    CO_REGIAO_CURSO == 4 ~ "Sul",
    CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"
  )) 

#sexo
microdados_ti = microdados_ti %>% 
  mutate(sexo = case_when(
    TP_SEXO == "M" ~ "Masculino",
    TP_SEXO == "F" ~ "Feminino"
  )) 

microdados_ti = microdados_ti %>% 
  mutate(hestudos = case_when(
    QE_I23 == "A" ~ "Nenhuma, apenas assisto ?s aulas",
    QE_I23 == "B" ~ "De uma a tr?s",
    QE_I23 == "C" ~ "De quatro a sete",
    QE_I23 == "D" ~ "De oito a doze",
    QE_I23 == "E" ~ "Mais de doze"
  )) 

#Resumindo os dados
s=summary(microdados_ti)  
d=describe(microdados_ti)

#Selecionando s? os resumos de interesse
d$CO_REGIAO_CURSO
d$CO_REGIAO_CURSO$values$frequency
d$CO_REGIAO_CURSO$values$frequency/sum(d$CO_REGIAO_CURSO$values$frequency)
s[1:7,1:12]

#Achando as frequ?ncias simples das vari?veis de outra forma:
t=table(microdados_ti$estado_civil)
p=prop.table(t)

#resumo estado civil
describe(microdados_ti$estado_civil)
unique(microdados_ti$estado_civil)

#resumo estado civil2
describe(microdados_ti$estado_civil2)
unique(microdados_ti$estado_civil2)

#Total, agrupado por Estado civil
microdados_ti %>% 
  select(estado_civil) %>% 
  group_by(estado_civil) %>% 
  summarise(total = n())

#m?dia, agrupada por Estado civil
microdados_ti %>% 
  select(estado_civil,NT_OBJ_FG) %>% 
  group_by(estado_civil) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#Contando a Quantidade De NA(observa??es fatantes) No Banco De Dados
resumo_nas=microdados_ti %>%
  select(everything()) %>%  
  summarise_all(funs(sum(is.na(.))))

#Visualizando
View(resumo_nas)

#Removendo as NA?S De Uma Vari?vel Espec?fica
teste_na=microdados_ti %>%
  filter(!is.na(estado_civil))

#Confirmando
describe(teste_na$estado_civil)

#Removendo  NAS De Todas As Vari?veis, Que possuem NA
microdados_ti_sem_NA=microdados_ti %>% na.omit()

resumo_nas=microdados_ti_sem_NA %>%
  select(everything()) %>%  
  summarise_all(funs(sum(is.na(.))))

#Visualizando
View(resumo_nas)


#Segunda Possibilidade De Excluir NA em Todas As Vari?veis
microdados_ti2 = microdados_ti %>% 
  filter(complete.cases(.))

#Certificando Que NAS FORAM REMOVIDAS
microdados_ti2 %>%
  select(everything()) %>%  
  summarise_all(funs(sum(is.na(.))))

#Quatidade De Linhas Do Banco Original
dim(microdados_ti)[1]
#Quatidade De Linhas Do Banco sem os NAS
dim(microdados_ti_sem_NA)[1]

#Total de linhas removidas que continhm NAS
total_linhas_excluidas=dim(microdados_ti)[1]-dim(microdados_ti_sem_NA)[1]

#Estat?sticas descritivas da vari?vel NOTA

#Calulando o Tamanho do vetor de notas
length(microdados_ti_sem_NA$NT_OBJ_CE)
#Calculando a M?dia
mean(microdados_ti_sem_NA$NT_OBJ_CE)
#Calculando a mediana
#De forma direta
median(microdados_ti_sem_NA$NT_OBJ_CE)
#teoria
#Como temos n par = 9636, teremos duas posi??es centrais (n/2) e (n/2+1)
#9636/2=4818 e mediana=(obs4818+obs4819)/2
#Calculando teoricamente
(sort(microdados_ti_sem_NA$NT_OBJ_CE)[4818]+sort(microdados_ti_sem_NA$NT_OBJ_CE)[4819])/2
#Moda
#Primeira etapa: Calcular as frequ?ncias simples
fs=table(microdados_ti_sem_NA$NT_OBJ_CE)
#Calcular o m?ximo das frequ?ncias simples
maximo=max(fs)
#trazer os nomes que correspondem as observa??es das fs
nomes=names(fs)
#trazer os nomes que satisfazem a compara??o l?gica
moda_texto=nomes[fs==maximo]
#Transformar em n?mero
moda_numero=as.numeric(moda_texto)


#max(table(microdados_ti_sem_NA$NT_OBJ_CE))
#parte1=microdados_ti_sem_NA %>% 
#      select(NT_OBJ_CE)  %>% 
#      table()            %>%
#      max()              

#moda= as.numeric (names(table(microdados_ti_sem_NA$NT_OBJ_CE))[table(microdados_ti_sem_NA$NT_OBJ_CE) == max(table(microdados_ti_sem_NA$NT_OBJ_CE))])

#M?ximo das frequ?ncias ? 1161 e corresponde a observa??o n?mero 50, logo 40 ? a moda.
#Logo temos que a m?dia(42.09)>mediana(40)=moda(40), logo n?o podemos afirmar que a distribui??o ? assim?trica, contudo apresentando
#uma leve simetria, que s? poderemos afirmar pelo c?lculo do coeficiente de assimetria de pearson.

#Para calcular a assimetria:
skewness(microdados_ti_sem_NA$NT_OBJ_CE)

#Coefieciente de assimetria de pearson>0, logo ter? assimetria positiva e concentra??o a esquerda.

#A kurtose do que o R calcula, ? padronizada, tirando -3, comparada a da normal
kurtosis(microdados_ti_sem_NA$NT_OBJ_CE)

#pelo R, temos que se 
#k>0, leptoc?rtica
#k=0, Mesoc?rtica
#k<0, Platic?rtica

#Consideramos ent?o platic?rtica.

p=ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE)) +
  geom_bar(col="black", 
           fill="blue", 
           alpha = .2)+
  labs(title="Gr?fico de colunas da nota dos alunos de an?lise de sistemas")+
  labs(x="nota", y="Frequ?ncia")
p
ggplotly(p)

p=ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE))+
  geom_density(col=2,size = 1, aes(y = 5 * ..count..))+
  labs(title="Curva de densidade da nota dos alunos de an?lise de sistemas")+
  labs(x="nota", y="Frequ?ncia relativa")
p
ggplotly(p)

p=ggplot(microdados_ti_sem_NA,aes(x=NT_OBJ_CE))+
  geom_histogram(color = "black", fill = "blue", bins = 50) +
  geom_density(col=2,size = 1, aes(y = 5 * ..count..))+
  labs(title="Histograma e curva de densidade da nota dos alunos de an?lise de sistemas")+
  labs(x="nota", y="Frequ?ncia relativa")
p
ggplotly(p)


#An?lises gr?ficas point click
microdados_ti_mod= microdados_ti_sem_NA %>% 
  select(estado_civil,NT_OBJ_FG,regiao,hestudos,sexo) %>% 
  group_by(estado_civil,regiao,hestudos,sexo) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))


#devtools::install_github("dreamRs/esquisse")

esquisser(viewer = "browser")



