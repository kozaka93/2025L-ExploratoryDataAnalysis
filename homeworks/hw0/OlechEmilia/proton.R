install.packages("proton")
library(proton)
proton()
# Problem 1
str(employees)
head(employees)
john_info <- employees[employees$name == "John" & employees$surname == "Insecure", ]
print(john_info)
john_login <- john_info$login
print(john_login)
proton(action = "login", login = john_login)

#Problem 2
for (password in top1000passwords) {
  response <- proton(action = "login", login = john_login, password = password)
  if (response == "Success! User is logged in!") {
    print(paste("Found password:", password))
    break
  }
}
# Problem 3
data("employees")
pietraszko_login <- employees[employees$surname == "Pietraszko", "login"]
print(pietraszko_login)
data("logs")
pietraszko_logs <- logs[logs$login == pietraszko_login, ]
most_frequent_host <- names(sort(table(pietraszko_logs$host), decreasing = TRUE))[1]
print(most_frequent_host)
proton(action = "server", host = most_frequent_host)

# Problem 4
data("bash_history")
common_commands <- c("ls", "cd", "vim", "vi", "mcedit", "nano", "cat", "echo", 
                     "rm", "cp", "mv", "touch", "mkdir", "rmdir", "find", "grep", 
                     "chmod", "chown", "ps", "top", "htop", "kill", "exit", "history", 
                     "pwd", "whoami", "man", "tar", "wget", "curl", "scp", "ssh", 
                     "ping", "df", "du", "mount", "umount", "alias", "unalias")

potential_passwords <- setdiff(bash_history[!grepl(" ", bash_history)], common_commands)
print(potential_passwords)
proton(action = "login", login = pietraszko_login, password = "DHbb7QXppuHnaXGN")







