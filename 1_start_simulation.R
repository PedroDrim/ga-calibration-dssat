# Carregando funcoes de inicializacao
source(".//src//loader.R")

# Carregando pacotes
load.packages()

# Compilando funcoes
compile.functions()

# Carregando funcoes compiladas
load.functions()

# Lendo arquivo de configuracao
input = config.treatment(".//StartValues_bean.config")

#===================================#
# Executar a verificacao de parametros da simulacao
correlationMatrix = runSimulationWarmup(input)

# Executar calibracao
runSimulationGA(input, correlationMatrix)
#===================================#
