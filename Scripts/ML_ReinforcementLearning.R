# Formação IA e ML - UDEMY
# Reinforcement Learning
# instalação e importação do pacote 
install.packages("ReinforcementLearning")

library(ReinforcementLearning)
# Criação do ambiente, usando a função gridworldEnvironment, do próprio pacote
ambiente <- gridworldEnvironment
# Visualização do ambiente
print(ambiente)

# Criação dos estados e ações que serão tomados no ambiente
estados <- c("s1", "s2", "s3", "s4")
acoes <- c("up", "down", "left", "right")

# Geração de amostras de experiências a partir das funções 
dados <- sampleExperience(N = 1000, env = ambiente, states = estados, actions = acoes)
head(dados)

# Geração do modelo de predição 
# (amostras, estados, amostras, recompensas, novo estado, controle (taxa de aprendizado, fator de desconto, fator de exploração))
modelo <- ReinforcementLearning(dados, s = "State", a = "Action", r = "Reward", 
                                s_new = "NextState", control =list(alpha = 0.1, gamma = 0.5, epsilon = 0.1))
# Mostrar o modelo e a melhor politica
modelo

policy(modelo)

