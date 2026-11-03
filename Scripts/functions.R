#FUNCIONES PARA EL PAPER DE DIETAS PARLAMENTARIAS

#Función para cargar los paquetes a utilizar. Si es que no encuentra el paquete lo instala
cargar_paquetes <- function(paquetes) {
  
  #Revisar que cada paquete esté instalado. Si no está instalado, se procede a instalarlo.
  for (paquete in paquetes) {
    
    if (paquete %in% rownames(installed.packages())) {
      
      print(paste0("El paquete ", paquete, " ya está instalado."))
      
    } else {
      
      install.packages(paquete)
      
      print(paste0("Instalando el paquete", paquete, "..."))
      
    }
    
  }
  
  
  #Cargar paquetes
  print("Paquetes cargados:")
  sapply(paquetes, require, character.only = T, quietly = T)
  
}