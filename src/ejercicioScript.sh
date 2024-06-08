#!/usr/bin/env bash
# Para ejecutar: Dar permisos con chmod u+x ejercicioScript.sh
# En terminal poner sh ejercicioScript.sh 
# El primer parámetro será para el archivo de entrada, y el segundo el de salida, con respecto a la carpeta a la que estamos. 
# EJEMPLO: sh ejercicioScript.sh input/in.jly output/Jelly.java
export entrada=$1
export salida=$2
echo $(racket jly_to_java.rkt) 
