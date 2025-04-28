###########################################
###     WSTĘP DO EKSPLORACJI DANYCH     ###
###           LABORATORIUM 9            ###
###########################################


# Grafy -------------------------------------------------------------------

# Instalacja pakietów -----------------------------------------------------
install.packages("igraph")
install.packages('tidygraph')
install.packages('ggraph')


library(igraph)
library(tidygraph)
library(ggraph)

# W jaki sposób można reprezentować grafy? --------------------------------

# Istnieje wiele sposobów na przechowywanie struktur grafowych. 
# Bardziej zaawansowane metody wykorzystują formaty takie 
# jak np. gml, i trzymają wszystkie infromacje w jednym pliku.

?read.graph
dolphins_graph <- read_graph("data/dolphins.gml", format = "gml")
dolphins_graph

# Inne podejścia są natomiast bardziej prymitywne 
# i wykorzystują formaty takie jak csv, aby w dwóch osobnych
# plikać trzymać informacje o wierzchołkach oraz krawędziach. 
# W takich strukturach często zauważyć można dodatkowe informacje, 
# takie jak np. waga krawędzi, albo dodatkowe cechy wierzchołków.

LOTR_edges <- read.csv('data/LOTR-edges.csv')
LOTR_nodes <- read.csv('data/LOTR-nodes.csv', sep = '\t')
View(LOTR_edges)
View(LOTR_nodes)

# W niektórych przypadkach, sieci (networks - inna nazwa na grafy) 
# mogą być reprezentowane poprzez bardzo proste struktury, 
# opisujące jedynie zbiór krawędzi.

Erdos_edges <- read.csv('data/ca-Erdos992.csv', sep = ' ', header = FALSE)
head(Erdos_edges)

# Wizualizacja grafów ----------------------------------------------------

# igraph ------------------------------------------------------------------

# igraph jest jednym z najpopularniejszych narzędzi służących 
# do analizy grafów. Poza samą wizualizacją sieci, 
# jest on przede szystkim zorientowany na ich analizę, oraz generowanie.

# Zbiór danych dolphins

?igraph.plotting
?layout
plot.igraph(dolphins_graph)

# Zbiór danych Zachary's karate club


### Zadanie 1
# Analogicznie do zbioru dolphins, poprawcie graf przedstawiający relacje między 
# członkami klubu Zacharego, tak aby wizualizacja była czytelniejsza.


# ggraph ------------------------------------------------------------------

# Alternatywą do wizualizacji grafów za pomocą biblioteki igraph jest 
# inspirowany ggplotem, pakiet ggraph. Jest on zorientowany jedynie 
# w kierunku wizualizacji i zasadniczo oferuje więcej możliwości niż sam igraph, 
# jednak jest też trochę bardziej skomplikowany.


# Zbiór danych dolphins

tg <- tidygraph::as_tbl_graph(dolphins_graph) %>% 
  tidygraph::activate(nodes)
tg

edge_list <- tg %>%
  activate(edges) %>%
  data.frame()

node_list <- tg %>%
  activate(nodes) %>%
  data.frame()

node_list$degree <- rep(0, nrow(node_list))
node_list$id <- node_list$id + 1

for (i in 1:nrow(node_list)) {
  node_list$degree[i] <- sum(edge_list$from == node_list$id[i]) + sum(edge_list$to == node_list$id[i])
}
head(edge_list)
head(node_list)

ig <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)

ig %>%
  ggraph(layout = "auto") +
  geom_node_point() +
  geom_edge_link() +
  geom_node_text(aes(label = label))


# Analiza grafowa ---------------------------------------------------------

# Aby uzupełnić naszą wiedzę na temat grafów, jako że policzyliśmy 
# stopnie wierzchołków w grafie dokanmy także sprawdzenia
# czy sieć delfinów jest siecią rzeczywistą.

ggplot(node_list, aes(degree)) +         
  geom_histogram(bins = 10) +         
  labs(title = "Histogram of nodes degree (bin = 10)", x = "Wieghted node degree", y = "Number of nodes") +         
  theme_minimal()

# Z powyższego grafu wynika, że rozkład stopni wierzchołków nie do końca posiada gruby ogon oraz ma dość mało wierzchołków.

cat('Clustering coefficient:', transitivity(ig),'\nDiameter of the graph:', 
    diameter(ig, directed = FALSE, weights = NULL))

# Ponadto zobaczyć możemy, że funkcja transitivity (innna nazwa na clsutering coefficient, zakres wartości [0,1]) 
# jest niewielka 0.31, natomiast średnica grafu (najdłuższa z najkrótszych ścieżek między wierchołkami) wynosi, aż 8.

# Niniejsza sieć wykazuje zatem pewne cechy sieci rzeczywistej (clustering coefficient), natomiast nie jest ich zbyt wiele.

### Zadanie 2
# Analogicznie do zbioru dolphins, poprawcie graf przedstawiający relacje między 
# członkami klubu Zacharego, tak aby wizualizacja była czytelniejsza.
# Dokonaj analizy czy sieć jest rzeczywista czy nie.


# Tekst -------------------------------------------------------------------

# Proces eksploracji tekstu
# Krok 1: Zgromadzenie danych: strony internetowe, e-maile, media społecznościowe, blogi i inne.
# Krok 2: Wstępne przetwarzanie tekstu: czyszczenie tekstu, tokenizacja, filtrowanie, stemming, 
# lematyzacja, przetwarzanie językowe, rozpoznawanie części mowy i ujednoznacznienie znaczenia słów.
# Krok 3: Ekstrakcja informacji, wyszukiwanie informacji, kategoryzacja, grupowanie, wizualizacja i podsumowanie.

# https://rpubs.com/vipero7/introduction-to-text-mining-with-r
# https://www.rdocumentation.org/packages/tidytext/versions/0.3.4 


# tidytext ----------------------------------------------------------------

# Instalacja pakietów -----------------------------------------------------
install.packages("tidytext")
install.packages("janeaustenr")


library(tidytext)
library(janeaustenr)
library(dplyr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(line = row_number()) %>% 
  ungroup()

# tokenizacja = podział każdej linii na słowa
?unnest_tokens

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text)

# czyszczenie tekstu = usunięcie słów stop (stopwords)
?get_stopwords

tidy_books <- tidy_books %>%
  anti_join(get_stopwords())

word_count <- tidy_books %>%
  count(word, sort = TRUE) 

# wizualizacja

# wordcloud2 
install.packages("wordcloud2")
library(wordcloud2) 

wordcloud2(data = word_count, size=1.6)

# quanteda - bardziej skomplikowany pakiet (tokens, corpus)
# https://quanteda.io/index.html
install.packages("quanteda")
install.packages("quanteda.textplots")

library("quanteda")
library("quanteda.textplots")

textplot_wordcloud(dfm(tokens(tidy_books$word)))

# analiza sentymentu
?get_sentiments

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = line %/% 80, sentiment) %>% 
  tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# wizualizacja
library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
