library(dplyr)

setwd("./dane")

years <- c(as.character(2015:2020))

rating_name <- read.csv("rating_name.csv", sep = ",")

rating_name %>% 
  group_by(federation) %>% 
  summarise(n = n()) %>% 
  filter(n != 0) %>% 
  select(federation) -> countries

szachy <- read.csv("chess_games.csv")

## rapid wolfdomix
df <- read.csv("partie_szachowe_wolfdomix.csv")
df_white <- df  %>% 
  filter(White == "wolfdomix")
df_white <- df_white %>% 
  select(WhiteElo, Date)
colnames(df_white) <-c("Elo","Date")

df_black <- df  %>% 
  filter(Black == "wolfdomix")
df_black <- df_black %>% 
  select(BlackElo, Date)
colnames(df_black) <-c("Elo","Date")

df_final <- rbind(df_white,df_black)
df_final$Date <- as.Date(df_final$Date, format = "%Y.%m.%d")

wolfdomix_rapid <- df_final %>% arrange(Date)
wolfdomix_rapid <- wolfdomix_rapid %>% mutate(Osoba = "Dominik")

#### bullet wolfdomix

df_bullet <- read.csv("partie_szachowe_wolfdomix_bullet.csv")

df_white_bullet <- df_bullet  %>% 
  filter(White == "wolfdomix")
df_white_bullet <- df_white_bullet %>% 
  select(WhiteElo, Date)
colnames(df_white_bullet) <-c("Elo","Date")

df_black_bullet <- df_bullet  %>% 
  filter(Black == "wolfdomix")
df_black_bullet <- df_black_bullet %>% 
  select(BlackElo, Date)
colnames(df_black_bullet) <-c("Elo","Date")

df_final_bullet <- rbind(df_white_bullet,df_black_bullet)
df_final_bullet$Date <- as.Date(df_final_bullet$Date, format = "%Y.%m.%d")

wolfdomix_bullet <- df_final_bullet %>% arrange(Date)
wolfdomix_bullet <- wolfdomix_bullet %>% mutate(Osoba = "Dominik")


## blitz wolfdomix

df_blitz <- read.csv("partie_szachowe_wolfdomix_blitz.csv")
df_white_blitz <- df_blitz %>% 
  filter(White == "wolfdomix")
df_white_blitz <- df_white_blitz %>% 
  select(WhiteElo, Date)
colnames(df_white_blitz) <- c("Elo","Date")

df_black_blitz <- df_blitz %>% 
  filter(Black == "wolfdomix")
df_black_blitz <- df_black_blitz %>% 
  select(BlackElo, Date)
colnames(df_black_blitz) <- c("Elo","Date")

df_final_blitz <- rbind(df_black_blitz,df_white_blitz)
df_final_blitz$Date <- as.Date(df_final_blitz$Date, format = "%Y.%m.%d")

wolfdomix_blitz <- df_final_blitz %>% arrange(Date)
wolfdomix_blitz <- wolfdomix_blitz %>% mutate(Osoba = "Dominik")


### blitz kuba

kuba_blitz_df <- read.csv("partie_szachowe_kuba_blitz.csv")
kuba_white_blitz <- kuba_blitz_df %>% 
  filter(White == "ILoveWalking") %>% 
  select(WhiteElo, Date) 
colnames(kuba_white_blitz) <- c("Elo","Date")


kuba_black_blitz <- kuba_blitz_df %>% 
  filter(Black == "ILoveWalking") %>% 
  select(BlackElo, Date)
colnames(kuba_black_blitz) <- c("Elo","Date")

kuba_blitz <- rbind(kuba_black_blitz,kuba_white_blitz)
kuba_blitz <- kuba_blitz %>% mutate(Osoba = "Jakub")
kuba_blitz$Date <- as.Date(kuba_blitz$Date, format = "%Y.%m.%d")
kuba_blitz <- kuba_blitz %>% arrange(Date)

### rapid kuba
kuba_rapid_df <- read.csv("partie_szachowe_kuba_rapid.csv")
kuba_white_rapid <- kuba_rapid_df %>% 
  filter(White == "ILoveWalking") %>% 
  select(WhiteElo, Date) 
colnames(kuba_white_rapid) <- c("Elo","Date")


kuba_black_rapid <- kuba_rapid_df %>% 
  filter(Black == "ILoveWalking") %>% 
  select(BlackElo, Date)
colnames(kuba_black_rapid) <- c("Elo","Date")

kuba_rapid <- rbind(kuba_black_rapid,kuba_white_rapid)
kuba_rapid <- kuba_rapid %>% mutate(Osoba = "Jakub")
kuba_rapid$Date <- as.Date(kuba_rapid$Date, format = "%Y.%m.%d")
kuba_rapid <- kuba_rapid %>% arrange(Date)

## daniel rapid 
daniel_rapid_df <- read.csv("partie_szachowe_daniel_rapid.csv")
daniel_white_rapid <- daniel_rapid_df %>% 
  filter(White == "DanielZawadzki2004") %>% 
  select(WhiteElo, Date) 
colnames(daniel_white_rapid) <- c("Elo","Date")


daniel_black_rapid <- daniel_rapid_df %>% 
  filter(Black == "DanielZawadzki2004") %>% 
  select(BlackElo, Date)
colnames(daniel_black_rapid) <- c("Elo","Date")

daniel_rapid <- rbind(daniel_black_rapid,daniel_white_rapid)
daniel_rapid <- daniel_rapid %>% mutate(Osoba = "Daniel")
daniel_rapid$Date <- as.Date(daniel_rapid$Date, format = "%Y.%m.%d")
daniel_rapid <- daniel_rapid %>% arrange(Date)

## daniel blitz
daniel_blitz_df <- read.csv("partie_szachowe_daniel_blitz.csv")
daniel_white_blitz <- daniel_blitz_df %>% 
  filter(White == "DanielZawadzki2004") %>% 
  select(WhiteElo, Date) 
colnames(daniel_white_blitz) <- c("Elo","Date")


daniel_black_blitz <- daniel_blitz_df %>% 
  filter(Black == "DanielZawadzki2004") %>% 
  select(BlackElo, Date)
colnames(daniel_black_blitz) <- c("Elo","Date")

daniel_blitz <- rbind(daniel_black_blitz,daniel_white_blitz)
daniel_blitz <- daniel_blitz %>% mutate(Osoba = "Daniel")
daniel_blitz$Date <- as.Date(daniel_blitz$Date, format = "%Y.%m.%d")
daniel_blitz <- daniel_blitz %>% arrange(Date)

## daniel bullet
daniel_bullet_df <- read.csv("partie_szachowe_daniel_bullet.csv")
daniel_white_bullet <- daniel_bullet_df %>% 
  filter(White == "DanielZawadzki2004") %>% 
  select(WhiteElo, Date) 
colnames(daniel_white_bullet) <- c("Elo","Date")


daniel_black_bullet <- daniel_bullet_df %>% 
  filter(Black == "DanielZawadzki2004") %>% 
  select(BlackElo, Date)
colnames(daniel_black_bullet) <- c("Elo","Date")

daniel_bullet <- rbind(daniel_black_bullet,daniel_white_bullet)
daniel_bullet <- daniel_bullet %>% mutate(Osoba = "Daniel")
daniel_bullet$Date <- as.Date(daniel_bullet$Date, format = "%Y.%m.%d")
daniel_bullet <- daniel_bullet %>% arrange(Date)


df_rapid <- rbind(rbind(wolfdomix_rapid,daniel_rapid),kuba_rapid)
df_blitz <- rbind(rbind(wolfdomix_blitz,daniel_blitz),kuba_blitz)
df_bullet<- rbind(wolfdomix_bullet,daniel_bullet)

