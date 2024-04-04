setwd("~/Documenti/profai/03 - Statistica Inferenziale/00.PROGETTO/")

################################################################################
# 1)
data <- read.csv("neonati.csv", stringsAsFactors = T)
# View(data)


################################################################################
# 3)
attach(data)
summary(data)
library(ggplot2)
library(tidyr)
library(dplyr)


# Sistemo valori molto anomali (es. madri di età 1 anno)
data <- data[Anni.madre > 10,]
n <- nrow(data)

# Analisi descrittiva
p1 <- ggplot()+
    geom_histogram(aes(x = Anni.madre),
                   binwidth = 1,
                   fill = "lightblue",
                   col = 1,
                   lwd = 0.15)+
    scale_x_continuous(breaks = seq(10, max(Anni.madre), 5))+
    scale_y_continuous(breaks = seq(0, 200, 25))+
    labs(x = "Età della madre",
         y = "Numero di madri")+
    theme_bw()

p2 <- ggplot()+
    geom_bar(aes(x = N.gravidanze),
             binwidth = 1,
             fill = "lightblue",
             col = 1)+
    scale_x_continuous(breaks = seq(0, max(N.gravidanze), 1))+
    scale_y_continuous(breaks = seq(200, 1000, 200))+
    labs(x = "Gravidanze sostenute in passato",
         y = "Numero di madri")+
    theme_bw()

p3 <- ggplot()+
    geom_bar(aes(x = Fumatrici),
             fill = "lightblue",
             col = 1)+
    scale_x_continuous(breaks = c(0, 1), labels = c("No", "Si"))+
    labs(x = "Madre fumatrice",
         y = "Numero di madri")+
    theme_bw()

p4 <- ggplot()+
    geom_bar(aes(x = Gestazione),
             fill = "lightblue",
             col = 1)+
    scale_x_continuous(breaks = seq(min(Gestazione)+1, max(Gestazione), 2))+
    scale_y_continuous(breaks = seq(0,700,100))+
    labs(x = "Settimane di gestazione",
         y = "Numero di madri")+
    theme_bw()

p5 <- ggplot()+
    geom_density(aes(x = Peso),
                 fill = "lightblue")+
    geom_vline(xintercept = median(Peso), col = 2)+
    geom_label(aes(x = median(Peso),
                   y = .5 * max(density(Peso)$y),
                   label = median(Peso)))+
    scale_x_continuous(breaks = seq(1000, 5000, 500))+
    scale_y_continuous(breaks = NULL)+
    labs(x = "Peso del neonato (grammi)",
         y = "")+
    theme_bw()

p6 <- ggplot()+
    geom_density(aes(x = Lunghezza),
                 fill = "lightblue")+
    geom_vline(xintercept = median(Lunghezza), col = 2)+
    geom_label(aes(x = median(Lunghezza),
                   y = .5 * max(density(Lunghezza)$y),
                   label = median(Lunghezza)))+
    scale_x_continuous(breaks = seq(300,550,50))+
    scale_y_continuous(breaks = NULL)+
    labs(x = "Lunghezza del neonato (mm)",
         y = "")+
    theme_bw()

p7 <- ggplot()+
    geom_density(aes(x = Cranio),
                 fill = "lightblue")+
    geom_vline(xintercept = median(Cranio), col = 2)+
    geom_label(aes(x = median(Cranio),
                   y = .5 * max(density(Cranio)$y),
                   label = median(Cranio)))+
    scale_x_continuous(breaks = seq(250, 375, 25))+
    scale_y_continuous(breaks = NULL)+
    labs(x = "Diametro del cranio del neonato (mm)",
         y = "")+
    theme_bw()

p8 <- ggplot()+
    geom_bar(aes(x = Tipo.parto),
             fill = "lightblue",
             col = 1)+
    scale_x_discrete(labels = c("Ces" = "Cesareo", "Nat" = "Normale"))+
    labs(x = "Tipo di parto",
         y = "Numero di parti")+
    theme_bw()

p9 <- ggplot()+
    geom_bar(aes(x = Ospedale),
             col = 1,
             fill = "lightblue")+
    scale_x_discrete(labels = c("osp1" = "Ospedale1", "osp2" = "Ospedale2", "osp3" = "Ospedale3"))+
    labs(x = "Ospedale di provenienza",
         y = "Numero di parti")+
    theme_bw()

p10 <- ggplot()+
    geom_bar(aes(x = Sesso),
             col = 1,
             fill = c("pink", "deepskyblue"))+
    labs(x = "Sesso del neonato",
         y = "Numero di neonati")+
    theme_bw()

library(patchwork)
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
    plot_layout(ncol = 2) + 
    plot_annotation(title = "Variabili del dataset", tag_levels = "I", tag_suffix = ")")

indici.statistici <- function(x){
    
    indici_posizione <- summary(x)
    
    indici_dispersione <- c("Intervallo:" = max(x)-min(x),
                            "IQR:" = IQR(x),
                            "dev.st:" = sd(x),
                            "var:" = var(x),
                            "CV:" = sd(x)/mean(x) * 100)
    
    indici_forma <- c("Asimmetria" = moments::skewness(x),
                      "Curtosi" = moments::kurtosis(x) - 3)
    
    indici <- c(indici_posizione, indici_dispersione, indici_forma)
    return(indici)
}

qualitative_vars <-  c("Fumatrici", "Tipo.parto", "Ospedale", "Sesso")
quantitative_vars <- names(data)[!(names(data) %in% qualitative_vars)]

for (x in quantitative_vars){
    print(x)
    idx <- indici.statistici(data[[x]])
    df_idx <- t(round(as.data.frame(idx), 2))
    
    write.table(x, file = "indici_statistici.csv", append = T)
    write.table(df_idx, file = "indici_statistici.csv", append = T)
}

freq_tables <- list()
for (x in qualitative_vars){
    t <- table(data[x])
    freq_tables <- append(freq_tables, t)
}


################################################################################
# 4)

# Fonte (per media peso/lunghezza neonati):
    # Ospedale pediatrico Bambino Gesù (importante centro di ricerca pediatrico)
    # https://www.ospedalebambinogesu.it/da-0-a-30-giorni-come-si-presenta-e-come-cresce-80012/
t.test(Peso, mu = 3300, conf.level = 0.95, alternative = "two.sided")
t.test(Lunghezza, mu = 500, conf.level = 0.95, alternative = "two.sided")


################################################################################
# 5)

# Tutte le differenze seguenti sono significative
t.test(Peso ~ Sesso, paired = F)
t.test(Lunghezza ~ Sesso, paired = F)
t.test(Cranio ~ Sesso, paired = F)

# Invece il tipo di parto (Nat/Ces) risulta indipendente dal sesso del neonato
chisq.test(table(Tipo.parto, Sesso))    


################################################################################
# 6)

tipo_parto_per_ospedale <- table(Ospedale, Tipo.parto)
prop.test(tipo_parto_per_ospedale)
chisq.test(tipo_parto_per_ospedale)
# p-valore alto -> le proporzioni non sono significativamente diverse
# Nota: prop.test(tipo_parto_per_ospedale) dà lo stesso identico risultato.



################################################################################
########################   ANALISI MULTIDIMENSIONALE   #########################
################################################################################
# 0)

# Prima di tutto, verifico che la variabile risposta (Peso) sia approssimativamente normale,
# andando a vedere gli indici di forma ed effettuando un test di Shapiro-Wilk.
# Lo verifico in anticipo perché eventuali allontanamenti dalla normalità
# della variabile risposta, spesso ricadono anche sui residui.

moments::skewness(Peso)
moments::kurtosis(Peso) - 3
shapiro.test(Peso)
# Noto che la var risposta non segue una distribuzione normale...
# So che in questo caso sarebbe opportuno usare un GLM, ma provo comunque con un modello di regressione lineare


################################################################################
# 1)

# Indago le relazioni a due a due tra le variabili quantitative
    # (risultano significative gestazione, lunghezza, cranio)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

data_quantitative <- data %>% select(all_of(quantitative_vars))
pairs(data_quantitative, upper.panel = panel.smooth, lower.panel = panel.cor)


# Indago anche per le variabili qualitative
    # (risulta significativa solo sesso; mi sarei aspettato pure Fumatrici)
boxplot(Peso ~ Sesso)
t.test(Peso ~ Sesso)

boxplot(Peso ~ Fumatrici)
t.test(Peso ~ Fumatrici)

boxplot(Peso ~ Tipo.parto)
#mean(Peso[Tipo.parto == "Nat"]); mean(Peso[Tipo.parto == "Ces"])
t.test(Peso ~ Tipo.parto)

boxplot(Peso ~ Ospedale)
pairwise.t.test(Peso, Ospedale, paired = F, pool.sd = T, p.adjust.method = "bonferroni")


################################################################################
# 2) 

mod1 <- lm(Peso ~ . , data = data)
summary(mod1)
BIC(mod1)
# R^2_adj = 0.72, è un valore ragionevole.

################################################################################
# 3)

# Questo è il modello creato con la procedura stepwise automatica (puntando al BIC minore)
stepwise_mod <- MASS::stepAIC(mod1, direction = "both", k = log(n))
summary(stepwise_mod)

# Faccio esperimenti manualmente:
mod2 <- update(mod1, ~.-Anni.madre)
summary(mod2)
anova(mod2, mod1)
BIC(mod2, mod1)
car::vif(mod2)

mod3 <- update(mod2, ~.-Ospedale)
summary(mod3)
BIC(mod3, mod2)
anova(mod3, mod2)

mod4 <- update(mod3, ~.-Fumatrici)
summary(mod4)
BIC(mod4, mod3)
anova(mod4, mod3)

# Ora tutte le variabili hanno p-value < 0.05.
# Provo a togliere le meno significative (prima Tipo.parto, poi N.gravidanze) per vedere cosa succede:
mod5 <- update(mod4, ~.-Tipo.parto)
summary(mod5)
BIC(mod4, mod5)
anova(mod4, mod5)

mod6 <- update(mod5, ~.-N.gravidanze)
summary(mod6)
BIC(mod5, mod6)    # SALE per la prima volta -> tengo N.gravidanze (mod5 va bene)
anova(mod5, mod6)

car::vif(mod5)
################################################################################
# 4)

# EFFETTI NON LINEARI
    # Da pairs() noto un lieve effetto quadratico tra Gestazione e la Y

mod10 <- update(mod5, ~.+I(Gestazione^2))
summary(mod10)
BIC(mod10, mod5)
anova(mod10, mod5)
# Il p-value di Gestazione è passato da 0.3% a 11%;
# R^2_adj è salito di 0.04% (quasi niente), il BIC è salito(!), e per l'ANOVA la
# differenza è significativa (p-value 0.02)
# Visto ciò, non tengo Gestazione^2


################################################################################
# 5)

par(mfrow = c(2,2))
plot(mod5)

shapiro.test(residuals(mod5))
plot(density(residuals(mod5)))
ggplot()+
    geom_density(aes(x = residuals(mod5)))+
    geom_vline(xintercept = mean(residuals(mod5)), col = 2)+
    theme_bw()+
    labs(x = "Residui", y = "")+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
# Il test di Shapiro-Wilk rifiuta la nomalità dei residui,
# ma l'analisi grafica sembra mostrare una relativa normalità
# (nel Q-Q plot si vede una retta, con piccola parte delle code che driftano; nel density plot si vede una normale, solo che la coda dx è molto lunga)
# Quindi questo probabilmente non inficia significativamente la potenza predittiva del modello

lmtest::dwtest(mod5)    # p-val=0.11 => residui non autocorrelati (bene)
lmtest::bptest(mod5)    # p-val=2e-16 => !!! ho eteroschedasticità (ahia)

cook <- cooks.distance(mod5)
cook[cook > 0.5]    # solo un valore anomalo. Lo vado a vedere con View(data)
                    # Noto che Lunghezza=315 e Peso=4370. Confrontando il peso con,
                    # quello di neonati di lunghezza simile, è estremamente diverso quindi
                    # sarà stato un errore di battitura, molto probabilmente.


# 3 outlier
car::outlierTest(mod5)

# 151 leverage
lev <- hatvalues(mod5)
p <- sum(lev)
soglia <- 2 * p/n
plot(lev)
abline(h=soglia,col=2,lwd=2)
lev[lev > soglia]; length(lev[lev > soglia])


################################################################################
# 6)

# R^2_adj è 0.72, cioè il 72% della variabilità del Peso è spiegato dal modello. Quindi il modello sembra abbastanza buono per spiegare la variabilità della var risposta.


################################################################################
# 7)

newdata_nat <- data.frame(N.gravidanze=2, Gestazione=39, Lunghezza=mean(Lunghezza), Cranio=mean(Cranio), Tipo.parto=factor("Nat"), Sesso=factor("F"))
newdata_ces <- data.frame(N.gravidanze=2, Gestazione=39, Lunghezza=mean(Lunghezza), Cranio=mean(Cranio), Tipo.parto=factor("Ces"), Sesso=factor("F"))
pred1 <- predict(mod5, newdata = newdata_nat)
pred2 <- predict(mod5, newdata = newdata_ces)
prediction = 1 / n * (pred1 * dim(data[Tipo.parto == "Nat",])[1]
                      + pred2 * dim(data[Tipo.parto == "Ces",])[1])
# Viene previsto un peso di 3257g


################################################################################
# 8)

summary(mod5)

p100 <- ggplot()+
    geom_point(aes(x = Gestazione,
                   y = Peso,
                   col = Sesso),
               alpha = 0.5,
               position = "jitter")+
    # Questa geom e la successiva servono per creare i bordi neri alle linee di fit
    geom_smooth(aes(x = Gestazione[Sesso == "M"],
                    y = Peso[Sesso == "M"]),
                col = "black",
                lwd = 1.5,
                method = "lm",
                formula = y ~ x + I(x^2),
                se = F)+
    geom_smooth(aes(x = Gestazione[Sesso == "F"],
                    y = Peso[Sesso == "F"]),
                col = "black",
                lwd = 1.5,
                method = "lm",
                formula = y ~ x + I(x^2),
                se = F)+
    geom_smooth(aes(x = Gestazione,
                    y = Peso,
                    col = Sesso),
                method = "lm",
                formula = y ~ x + I(x^2),
                se = F)+
    labs(x = "Settimane di gestazione",
         y = "Peso del neonato (g)")+
    theme_bw()+
    theme(legend.position = "none")
    
p101 <- data %>% 
    filter(N.gravidanze <= 4) %>% 
ggplot()+
    geom_boxplot(aes(x = as.factor(N.gravidanze),
                     y = Peso))+
    geom_smooth(aes(x = 1+N.gravidanze,
                    y = Peso),
                lwd = 1.25,
                method = "lm",
                se = F)+
    labs(x = "Numero di gravidanze precedenti",
         y = "Peso del neonato (g)")+
    theme_bw()

p102 <- ggplot()+
    geom_point(aes(x = Lunghezza,
                   y = Peso,
                   col = Sesso),
               alpha = 0.25,
               position = "jitter")+
    # Questa geom e la successiva servono per creare i bordi neri alle linee di fit
    geom_smooth(aes(x = Lunghezza[Sesso == "M"],
                    y = Peso[Sesso == "M"]),
                col = 1,
                lwd = 1.5,
                method = "lm",
                se = F)+
    geom_smooth(aes(x = Lunghezza[Sesso == "F"],
                    y = Peso[Sesso == "F"]),
                col = 1,
                lwd = 1.5,
                method = "lm",
                se = F)+
    geom_smooth(aes(x = Lunghezza,
                    y = Peso,
                    col = Sesso),
                method = "lm",
                se = F)+
    scale_x_continuous(breaks = seq(300, 550, 50))+
    scale_y_continuous(breaks = seq(1000, 5000, 500))+
    labs(x = "Lunghezza del neonato (mm)",
         y = "Peso del neonato (g)")+
    theme_bw()+
    theme(legend.position = "none")


p103 <- ggplot()+
    geom_point(aes(x = Cranio,
                   y = Peso,
                   col = Sesso),
               alpha = 0.4,
               position = "jitter")+
    # Questa geom e la successiva servono per creare i bordi neri alle linee di fit
    geom_smooth(aes(x = Cranio[Sesso == "M"],
                    y = Peso[Sesso == "M"]),
                col = 1,
                lwd = 1.5,
                method = "lm",
                se = F)+
    geom_smooth(aes(x = Cranio[Sesso == "F"],
                    y = Peso[Sesso == "F"]),
                col = 1,
                lwd = 1.5,
                method = "lm",
                se = F)+
    geom_smooth(aes(x = Cranio,
                    y = Peso,
                    col = Sesso),
                method = "lm",
                se = F)+
    scale_x_continuous(breaks = seq(260, 380, 40))+
    scale_y_continuous(breaks = seq(1000, 5000, 500))+
    labs(x = "Lunghezza cranio del neonato (mm)",
         y = "Peso del neonato (g)")+
    theme_bw()+
    theme(legend.position = "none")




p100 + p101 + p102 + p103 +
    plot_layout(ncol = 2, guides = "collect") + 
    plot_annotation(title = "Relazioni tra la variabile risposta (Peso) e i regressori")

