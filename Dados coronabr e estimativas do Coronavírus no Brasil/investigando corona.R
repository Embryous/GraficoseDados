#Importa as bibliotecas e define o work directory para salvar os gráficos
library(coronabr)
library(tidyverse)
library(scales)
setwd("")

#Importa os dados utilizando o pacote coronabr
dados <- get_corona_br(by_uf = T)

#Cria a base de dados com as estimativas e retira o último dia que ainda não foi totalmente contabilizado em relação a casos e mortes
dados_total <- dados%>%group_by(date)%>%summarize(confirmados =sum(confirmed), mortes = sum(deaths))
colnames(dados_total)[1]<-"dia"
dados_total<-dados_total%>%mutate(mortalidade = (mortes/confirmados)*100, estim_1=mortes/0.01, estim_06 = round(mortes/0.006), estim_085 = round(mortes/0.0085))
dados_total <- dados_total[-85,]

#Transforma a base de wide para long para fazer o gráfico
dados_longo_estimativa <- dados_total%>%select(c(dia, confirmados,estim_1, estim_0598, estim_085))%>%pivot_longer(-dia, names_to = "Estimativas", values_to = "Casos")

#Cria o gráfico de estimativas                                                      
ggplot(dados_longo_estimativa, aes(dia, Casos, color=Estimativas))+
  geom_line()+
  labs(y="Casos", x= "Dia", title= "Casos Confirmados x Estimativa", caption = "Dados do pacote Coronabr")+
  theme_classic()+
  coord_cartesian(ylim=c(0,3000000))+
  scale_y_continuous(labels = comma)+
  scale_color_manual(name = "Estimativas", labels = c("Confirmados", "Estimativa 0,6%", "Estimativa 0,85%", "Estimativa 1%"), values = c("Red", "Blue", "Purple","Green"))

#Usa os dados de estimativas para fazer as proporções de quantos casos estão sendo contabilizados em relação às estimativas
#Necessário filtrar as datas para quando os valores são maiores e retirar o último dia onde nem todos os casos e mortes forem contabilizados
casos_explicados <- dados_total%>%mutate(not1 = confirmados/estim_1, not2= confirmados/estim_085, not3=confirmados/estim_06)%>%select(dia, not1, not2, not3)%>%filter(dia>="2020-03-19")
casos_explicados<- casos_explicados[-61,]

#Transforma os dados de proporções de wide para long para o gráfico
dados_longo_explicados <- casos_explicados%>%pivot_longer(-dia, names_to = "Estimativas", values_to = "Porcentagem")

#Cria o gráfico com as propoções de casos contabilizados dadas as estimativas
ggplot(dados_longo_explicados, aes(dia, Porcentagem, color=Estimativas))+
  geom_line()+
  labs(y="Porcentagem", x= "Dia", title= "Porcentagem de Casos Confirmados", caption = "Dados do pacote Coronabr")+
  theme_classic()+
  coord_cartesian(ylim=c(0,0.5))+
  scale_color_manual(name = "Estimativas", labels = c("Estimativa 0,6%", "Estimativa 0,85%", "Estimativa 1%"), values = c("Red", "Blue", "Green"))

