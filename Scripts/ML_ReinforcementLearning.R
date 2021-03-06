# Forma��o IA e ML - UDEMY
# Reinforcement Learning
# instala��o e importa��o do pacote 
install.packages("ReinforcementLearning")

library(ReinforcementLearning)
# Cria��o do ambiente, usando a fun��o gridworldEnvironment, do pr�prio pacote
ambiente <- gridworldEnvironment
# Visualiza��o do ambiente
print(ambiente)

# Cria��o dos estados e a��es que ser�o tomados no ambiente
estados <- c("s1", "s2", "s3", "s4")
acoes <- c("up", "down", "left", "right")

# Gera��o de amostras de experi�ncias a partir das fun��es 
dados <- sampleExperience(N = 1000, env = ambiente, states = estados, actions = acoes)
head(dados)

# Gera��o do modelo de predi��o 
# (amostras, estados, amostras, recompensas, novo estado, controle (taxa de aprendizado, fator de desconto, fator de explora��o))
modelo <- ReinforcementLearning(dados, s = "State", a = "Action", r = "Reward", 
                                s_new = "NextState", control =list(alpha = 0.1, gamma = 0.5, epsilon = 0.1))
# Mostrar o modelo e a melhor politica
modelo

policy(modelo)

