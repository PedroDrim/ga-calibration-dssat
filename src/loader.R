#===============================================#
# Carregando pacotes necessarios
load.packages = function() {
  require(ggplot2)
  require(reshape2)
  require(gridExtra)

  require(doParallel)
  require(data.table)
  require(compiler)
}
#===============================================#

#===============================================#
# Compinando funcoes
compile.functions = function() {
  arquivos = list.files(path = ".//src", pattern = ".R$", full.names = T, recursive = T)
  arquivos_out = gsub("\\.R", replacement = "\\.Rc", arquivos)
  
  for(i in 1:length(arquivos)) {
    cmpfile(arquivos[i], arquivos_out[i])
  }
}
#===============================================#

#===============================================#
# Carregando funcoes compiladas
load.functions = function() {
  arquivos = list.files(path = ".//src", pattern = ".Rc$", full.names = T, recursive = T)
  res = sapply(arquivos, loadcmp)
}
#===============================================#

#===============================================#
# Funcao responsavel pela leitura do arquivo de configuracao
config.treatment = function(arq.config) {
  
  con = file(arq.config)
  open(con)
  
  linhas = readLines(con)
  linhas = linhas[linhas != ""]
  linhas = linhas[-grep("!",linhas)]
  
  input = list()
  for(i in linhas){
    i = strsplit(i,"=")[[1]]
    
    index = gsub(" ","",i[1])
    valor = gsub(" ","",i[2])
    valor = strsplit(valor,",")[[1]]
    
    input[[index]] = valor
  }
  
  close(con)
  
  return(input)
}
#===============================================#
