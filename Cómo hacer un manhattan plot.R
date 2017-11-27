#El primer paso para generar el Manhattan es instalar los siguientes
#paquetes 'data.table' y 'qqman', posteriormente accedemos a ellos por
#medio de  los siguientes comandos:
library(data.table)
library(qqman)

#Después, le diremos a R que nos posicione a donde se encuentre la
#carpeta que albergue los archivos con el Manhattan, para ello (y si queremos)
#usamos el comando 'getwd()' para ubicarnos en nuestra computadora y
#luego:
getwd()
setwd("Lab/Archivos sobre adicciones/Manhattan/Manhattan final/") #Aquí direccionaremos a R a la carpeta contenedora, es decir
#le ponemos la dirección

#A continuación, agrego el archivo con los datos del GWAS sobre
#alcoholismo solamente con las columnas que el paquete 'qqman'
#necesita para poder elaborar el manhattan plot por medio de este comando

Alcoholism<-fread("mptest.txt", sep = "\t", header = T, stringsAsFactors = F )

#El script se refiere a: 'fread' como a 'file read', por lo que se introduce el
#nombre del arcivho desde la posición en la que estamos posicionados en R
# 'sep=' se refiere a cómo está separado (comas, puntos o tabulador),
#en este caso, el archivo en .txt está separado por tabulador, así que asignamos
#dicho valor. La primera fila es el encabezado, por lo que R lo tomará como tal
#y por default se pone "T" y como al archivo que vamos a introducir no es para
#hacer un estudio estadístico per se, al valor de
#'StringsAsFactors' se le pone 'F' para denotar esto.
#Además, el archivo que ya está arreglado se llama 'mptest.txt' por 'Manhattan
#plot test'.

colnames(Alcoholism) <- c("SNP", "CHR", "BP", "P") #Este comando es para cambiar
#el nombre de las columnas en el objeto 'Alcoholism' (que es donde metimos
#el archivo para el Manhattan)

#En este punto se cambiar el tipo de argumento la columna de interés, en este
#caso 'CHR' de 'character' a 'numeric', por lo que se hará lo mismo para la
#nueva lista con la nueva información

Alcoholism$CHR <- as.numeric(Alcoholism$CHR)

#Finalmente, obtendremos el Manhattan plot con el siguiente comando
manhattan(Alcoholism)

#El paquete en R tiene configuraciones predeterminadas que no nos sirven de
#mucho, así que lo modificaremos de la siguiente manera:
manhattan(subset(Alcoholism), genomewideline = F)
#No estoy demasiado seguro de lo que signifique en la gráfica el
#'genomewideline', así que la quitamos dejándole como valor 'False' o 'F'
#pero es la otra línea la que importa, ya que es el valor de P predeterminado,
#por eso lo dejamos como está

#Una opción interesante de este paquete es la visualización de los cromosomas
#que desees, esto se logra así:
manhattan(subset(Alcoholism, CHR==1))

#Y así sin la rayita que te comenté:
manhattan(subset(Alcoholism, CHR==1), genomewideline = F)

#EXTRA: En un primer intento para saber qué SNPs eran los importantes,
#había buscado todos los SNPs de manera 'manual' puesto que TODOS eran
#estadisticamente significativos. El paquete te permite resaltar los SNPs que
#quieras siempre y cuando los agregues como un vector, por lo que hice lo sig.

SIC1<- c("rs7553212", "rs2749097", "rs61776290", "rs35738462", "rs514341", "rs1362153", "rs3738443", "rs6701037", "rs3131513", "rs4478858", "rs75562159", "rs195204")
#SIC se refiere a: 'SNPs of Interest; Chromosome - 1'
manhattan(subset(Alcoholism, CHR ==1), highlight = SIC1)

#Aquí es donde me entró la duda de si alguno tenía un valor de P mayor a 0.05
#así que con ayuda de Israel, pude resolver esta duda y la respuesta fue:
#NINGUNO. Sin embargo, en el Manhattan se pueden visualizar algunos
#polimorfismos que resaltan más, así que saqué el promedio de todos los SNPs
SICA <- Alcoholism[Alcoholism$P >= mean(Alcoholism$P),]
#SICA se refiere a: SNPs of Interest: All

#Desglosándolo: Le pedí a R que creara un nuevo objeto llamado 'SICA'
#en el que, del objeto previo llamado 'Alcoholism' pudiera tener en 'SICA'
#El primedio del valor de P de TODOS, y es que si se lo pides sin el signo
# de pesos, tratará de hacer la acción para todas las colmnas del archivo
#por eso el comando está escrito así

#en este punto hago una observación: Al parecer todos los SNPs  que aparecen en
#este estudio son estadísticamente significativos, TODOS, pues incluso hay SNPs
#coon un valor de P de 4.00E-211 como te dije el jueves.

#A continuación, el siguiente comando es utilizado para delimitar los
#polimorfismos con un valor de P menores o iguales a 1X10⁻⁸, que es el valor
#promedio que poseen casi todos los SNPs para este estudio.
#Si lo quieres corroborar por ti misma, puedes correr el siguiente comando:
mean(Alcoholism$P)
summary(Alcoholism$P)
P <- Alcoholism[Alcoholism$P <= 3.609e-06]

#Con el siguiente comando le pido a R una lista de los SNPs que estén por debajo
#de la media (los valores más extremadamente chiquitos, pues)
snps <- P$SNP

#Si quieres conocer qué tipo de objetos son 'P' y 'snps', corre los siguientes
#comandos:
typeof(P)
typeof(snps)

snps #Cuando corres esto, te dará la lista de SNPs

#FINALMENTE, obtenemos el manhattan plot sólo con los valores resaltados que
#queríamos mediante lo siguiente:
manhattan(Alcoholism, highlight = snps, genomewideline = F)
################################################################################
#Más configuraciones para el manhattan:
#manhattan(gwasResults, main = "Manhattan Plot", ylim = c(0, 10), cex = 0.6,
#cex.axis = 0.9, col = c("blue4", "orange3"), suggestiveline = F, genomewideline = F,
#chrlabs = c(1:20, "P", "Q"))
#'main=' para añadir título, incrementar el eje de las y con '(ylim=)', reduce el
# tamaño de punto a 60% con '(cex=)' y reduce el tamaño de la fuente de las etiquetas de los ejes
#a 90% '(cex.axis=)'. Mientras estamos en ello, se puede también cambiar el color de la gráfica por
#medio de '(col=)',

#Esto es un copy-paste del script de Hugo, lo usé para obtener el pdf, jejeje
## Primera versión ##
# Paso 1. Abrir el archivo:
pdf(file="Alcoholism.pdf", width = 9, height = 7)

# Paso 2. Crear la figura en el archivo
manhattan(Alcoholism, highlight = snps, genomewideline = F)

# Paso 3. Cerrar el archivo
dev.off()

write.table(P,"P", sep = "\t")
#Propiedad de Hugo.
