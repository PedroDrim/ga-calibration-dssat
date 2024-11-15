#===============================================#
# Funcao responsavel por replicar o ambiente de execussao do dssat no diretorio do experimento
# @param (character) dssatFile => Arquivo binario de execussao do dssat
# @param (character) experimentDirectory => Diretorio contendo o experimento original (baseline) 
# @param (character) simulationDirectory => Diretorio em que a simulacao ira acontecer 
# @returns (character[]) Arquivos necessarios para a execussao da simulacao
filesDssat = function(dssatFile, experimentDirectory, simulationDirectory) {
  # Montando regex de busca
  baseFiles = '.ECO$|.SPE$|.WTH$|.X$|.SOL$|.CUL$|.T$|.A$|.ERR$|.CDE$|.L47$'

  # Arquivos do diretorio de experimento
  experiment = list.files(experimentDirectory, pattern = baseFiles, full.names = T)

  # Copiando arquivos para o diretorio da rodada
  status = file.copy(c(experiment, dssatFile), simulationDirectory, recursive = T)

  # Retornando nomes dos arquivos necessarios para execussao
  dssatFiles = list.files(simulationDirectory, pattern = baseFiles, full.names = TRUE)
  return(dssatFiles)
}
#===============================================#

#===============================================#
# Funcao responsavel por criar os diretorios referentes a execucao das rodadas
# @param (matrix) multiplyMatrix => Matrix contendo as multiplicacoes necessarias durante a rodada
# @param (character) templateId => Identificador do diretorio de simulacao 
# @param (list) inputList => Lista de parametros de execussao
# @returns (list) Lista contendo os diretorios prontos para teste dos individuos da rodada
createSimulationDirectories = function(multiplyMatrix, templateId, inputList) {
  # Obtendo diretorio de execucao
  dirRun = inputList$dirRun

  # Limpando diretorio de execucao
  unlink(sprintf("%s//.", dirRun), recursive = T)

  # Indices da matrix de multiplicacao
  matrix.index = 1:nrow(multiplyMatrix)

  # Obtendo nome dos diretorios de simulacao
  simulationDirectories = sprintf("%s//%s_%s", dirRun, templateId, matrix.index)

  # Criando diretorio da rodada
  status = sapply(simulationDirectories, dir.create, showWarnings = F, mode = "777")

  # Obtendo parametros do cultivar
  cultivarFile = inputList$cultivarFile
  cultivar = inputList$cultivar
  
  # Obtendo parametros do dssat
  dssatFile = inputList$dssatFile
  experimentDir = inputList$dirExperiment

  # Preparando diretorios
  simulation.list = lapply(matrix.index, function(index, multiplyMatrix, simulationDirectories, cultivarFile, cultivar, dssatFile, experimentDir) {
    # Obtendo diretorio da simulacao
    simulatedDirectory = simulationDirectories[index]

    # Criando linha de cultivar
    newCultivar = makeCultivar(cultivarFile, multiplyMatrix[index,], cultivar)

    # Copiando arquivo necessarios para o dssat
    files.list = filesDssat(dssatFile, experimentDir, simulatedDirectory)
  
    # Escrevendo cultivar
    writeCultivar(newCultivar, simulatedDirectory)

    # Retornando lista de arquivos
    return(files.list)
  }, multiplyMatrix, simulationDirectories, cultivarFile, cultivar, dssatFile, experimentDir)  

  # Aplicando nomes dos diretorios de simulacao aos arquivos
  names(simulation.list) = simulationDirectories

  # Retornando diretorios das simulacoes
  return(simulation.list)
}
#===============================================#

#===============================================#
# Funcao responsavel por executar o dssat via comando
# @param (character) simulationDirectory => Diretorio em que a simulacao ira acontecer 
# @param (character) dssatFile => Arquivo binario de execussao do dssat
# @param (character) model => Modelo da simulacao dssat 
# @returns (boolean) Validacao da saida do modelo
executeDssat = function(simulationDirectory, dssatFile, model) {
  # Definindo ponto de retorno
  homeDirectory = getwd()
  setwd(simulationDirectory)

  # Extraindo nome base do arquivo de execucao
  dssatFile = strsplit(dssatFile, split = "//")[[1]]
  dssatFile = dssatFile[length(dssatFile)]
  
  # Executando dssat
  command = paste0("./", dssatFile, " " , model," B", " DSSBatch.v47")
  system(command, ignore.stdout = T)
  
  # Voltando ao ponto original
  setwd(homeDirectory)

  # Verificando existencia de arquivos necessarios
  validationRegex = sprintf("%s.OUT$", c("Evaluate", "PlantGro")) |> paste0(collapse = "|")
  outputValidation = list.files(simulationDirectory, pattern = validationRegex)
  outputValidation = length(outputValidation) == 2

  # Retornando validacao
  return(outputValidation)
}
#===============================================#

#===============================================#
# Funcao responsavel por criar o arquivo Batch do dssat
# @param (character) crop => Tipo do cultivar a ser simulado
# @param (character) x_file => Nome do arquivo .X
# @param (character) filename => Nome do arquivo de saida gerado 
# @param (character[]) treatmentId => Identificadores dos tratamentos
CSMbatch = function(crop, x_file, filename, treatmentId) {

  # Identificador da rodada
  tn = as.numeric(treatmentId)

  # Tratando o caminho do arquivo X
  x_file = strsplit(x_file, "/")[[1]]
  x_file = x_file[length(x_file)]

  # Batchfile headers
  outbatch = sprintf("$BATCH(%s)", crop)
  outbatch = c(outbatch, sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", "CO"))
  outbatch = c(outbatch, sprintf("%6s %86s %6i %6i %6i %6i",            
                                 paste0(x_file),
                                 tn,  # Variable for treatment number            
                                 1,  # Default value for RP element            
                                 0,  # Default value for SQ element            
                                 1,  # Default value for OP element            
                                 0))
  

  cat(outbatch, file = filename, sep = "\n", append = F)
}
#===============================================#

#===============================================#
# Funcao responsavel por executar o dssat para uso interno
# @param (character[]) simulationFiles => Arquivos necessarios para a simulacao do dssat
# @param (character) model => Modelo da simulacap
# @param (character) dssatFile => Nome do arquivo de execussao do dssat 
# @param (character[]) calibration => Variaveis de calibracao selecionados para a analise
# @returns (data.table) Resultado da execussao e analise do modelo dssat
runDssat = function(simulationFiles, model, dssatFile, calibration) {
  # Diretorio da simulacao
  simulationDir = dirname(simulationFiles[1])

  # Extraindo arquivo X
  x_file = simulationFiles[grep(".X$", simulationFiles)]

  # Lendo os tratamentos da rodada
  treatmentId = readTreatmentsId(x_file)
    
  # Lendo a regiao da rodada
  region = readRegion(x_file)
  
  # Gerando arquivo DSSBatch
  batchFile = paste0(simulationDir, "//", "DSSBatch.v47")

  # Criando arquivo de Batch do dssat
  CSMbatch("BEAN", x_file, batchFile, treatmentId)
  
  # Executando dssat
  simulationStatus = executeDssat(simulationDir, dssatFile, model)

  # Retornando NA caso simulacao tenha dado erro
  if(!simulationStatus) {
    # Removendo diretorio de execucao temporario
    unlink(simulationDir, recursive = T)
    return(NA)
  }

  # Extraindo arquivo T
  t_file = simulationFiles[grep(".T$", simulationFiles)]
  t_file = t_file[-grep(".LST$", t_file)]

  # Carregando arquivo T
  tData = readTfile(t_file)

  # Carregando Evaluate.OUT
  runEvaluate = readEvaluate(simulationDir, region) |> evaluateDifference(calibration)

  # Carregando Plantgro.OUT
  runPlantgrout = readPlantgrout(simulationDir, treatmentId) |> plantgroDifference(tData, calibration)
    
  # Unindo resposta
  run = merge(runEvaluate, runPlantgrout, by = "TN", all.x = T)

  # Removendo diretorio de execucao temporario
  unlink(simulationDir, recursive = T)
  return(run)
}
#===============================================#