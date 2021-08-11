
library(tidyverse)
library(tm)
library(tidytext)
library(pdftools)
library(here)
library(ggthemes)

#Load fonts.
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

#Scrape data from PDFs.
suarez1979 <- pdf_text("1979suarez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Suárez 1979")
sotelo1981 <- pdf_text("1981sotelo.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Sotelo 1981")
gonzalez1982 <- pdf_text("1982gonzalez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "González 1982")
gonzalez1986<- pdf_text("1986gonzalez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "González 1986")
gonzalez1989<- pdf_text("1989gonzalez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "González 1989")
gonzalez1993 <- pdf_text("1993gonzalez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "González 1993")
aznar1996 <- pdf_text("1996aznar.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Aznar 1996")
aznar2000 <- pdf_text("2000aznar.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Aznar 2000")
zapatero2004 <- pdf_text("2004zapatero.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Zapatero 2004")
zapatero2008 <- pdf_text("2008zapatero.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Zapatero 2008")
rajoy2011 <- pdf_text("2011rajoy.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Rajoy 2011")
rajoy2016 <- pdf_text("2016rajoy.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Rajoy 2016")
sanchez2019 <- pdf_text("2019sanchez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Sánchez 2019")
sanchez2020 <- pdf_text("2020sanchez.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Sánchez 2020")



full_parties <- mget(ls()) %>% bind_rows() 


#Remove custom stopwords.

#stopwords <- c()

#Remove stopwords from corpus.
parties_wo_stopwords<- full_parties %>% anti_join(as_tibble(stopwords("spanish")), by = c("word" = "value"))


#Create new dataframe with word counts.

count_words <- parties_wo_stopwords %>% count(party, word, sort = TRUE)

total_words <- count_words %>% group_by(party) %>% summarise(total = sum(n))

words_parties <- left_join(count_words, total_words)


#Remove custom stopwords.

#stopwords <- c("")

parties_tf_idf <- words_parties %>%
  #filter(n > 2 & !word %in% stopwords) %>% 
  bind_tf_idf(word, party, n) %>% 
  mutate(party = factor(party, levels = c("Suárez 1979", "Sotelo 1981", "González 1982",
                                          "González 1986", "González 1989", "González 1993",
                                          "Aznar 1996", "Aznar 2000", "Zapatero 2004", 
                                          "Zapatero 2008", "Rajoy 2011", "Rajoy 2016",
                                          "Sánchez 2019", "Sánchez 2020")))



parties_tf_idf <- parties_tf_idf %>% mutate(year = str_sub(party, -4))
parties_tf_idf$year <- as.numeric(parties_tf_idf$year)
class(parties_tf_idf$year)



order_color <- c("mediumvioletred", "seagreen3", "tan4",
                 "mediumvioletred", "seagreen3", "tan4",
                 "mediumvioletred", "seagreen3", "tan4",
                 "mediumvioletred", "seagreen3", "tan4",
                 "mediumvioletred", "seagreen3")

#Plot.

parties_tf_idf %>% 
  group_by(party)%>%
  slice_max(tf_idf, n = 9, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(str_to_title(word), tf_idf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~party, ncol = 3, scales = "free") +
  labs(title = "¿Cuáles son las palabras más diferenciales de los candidatos en las primarias del PSOE-A?", 
       subtitle = "Palabras más frecuentes durante el debate en comparación con su frecuencia en el del resto (tf-idf)", 
       y = NULL, x = NULL, caption = "Fuente: lamoncloa.gob.es | @horrorchess89") +
  theme_test(base_family = "Bahnschrift") +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 5),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = order_color)



parties_tf_idf %>%
  filter(word %in% c("españa", "empleo", "democracia", "libertad", "corrupción", "desempleo",
                     "constitución", "unidad", "mujeres",
                     "igualdad", "progreso", "justicia", "sanidad",
                     "diálogo", "economía",
                     "situación", "económica", "españoles", "cataluña",
                     "terrorismo",  "europa", "educación", "jóvenes",
                     "género", "paro",  "derechos",
                     "violencia",  "mundo", 
                     "pueblo", "españolas",
                     "europea", "confianza", "pasado", "futuro",
                     "ciencia", "acuerdo", "programa",
                     "acción", "crisis", "país",
                     "compromiso", "derecho", "vamos",  "diversidad",
                     "inflación", "fiscal", "digital", 
                     "sociedad", "social", "ley", "política", "esfuerzo", "crecimiento", 
                     "cooperación", "sistema", "política", "gobierno")) %>%
  ggplot(aes(year, n / year)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~ word) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5)) +
  labs(y = "% frequency of word in investiture debate",
       x = "") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 6))
        

p <- parties_tf_idf %>%
  filter(word %in% c(
    "acuerdo",
    "cataluña",
    "compromiso",
    "economía",
    "democracia",
    "derecho",
    "derechos",
    "diálogo",
    "españa",
    "igualdad",
    "europa",
    "españoles",
    "género",
    "gobierno",
    "país",
    "sistema",
    "social",
    "sociedad",
    "terrorismo",
    "vamos",
    "constitución",
    "mujeres",
    "empleo",
    "política",
    "libertad")) %>%
  ggplot(aes(year, n / year)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~ word, scales = "free_y") +
  labs(title = "Changes in the frequency of words over time within investiture speeches in Spain (1979-2020)", 
       subtitle = "", 
      caption = "Source: lamoncloa.gob.es | @dataR_amateur") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in investiture speech",
       x = "") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 8.5)) +
  theme(plot.title = element_text(hjust = 0.5)) 

p
p + ggsave("investidura.png", width = 13, height = 8.5, dpi = 500)
