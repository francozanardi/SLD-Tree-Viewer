# SLD Tree Viewer
Aplicación web en java que permite visualizar un árbol SLD a partir de un código fuente en Prolog y una consulta sobre éste.

## Obtenención del proyecto
Clonar el repositorio desde GitHub:
```
$ git clone https://github.com/francozanardi/SLD-Tree-Viewer.git
```

## Pre-requisitos
* [Maven](http://maven.apache.org/)
* Swi-prolog ?

## Instalación
Una vez dentro de la carpeta que contiene el repositorio, se debe realizar lo siguiente.
1. Contruir el proyecto con [Maven](http://maven.apache.org/).
```
    $ mvn clean install
```
2. Agregar _JPL_ a las variables de entorno del sistema. ¿?

## Ejecución
Ejecutar _Tomcat_:
```
    $ mvn tomcat7:run
```
La web correrá en 
```
    http://localhost:8080/sld_tree_viewer
```
## Tecnologías utilizadas.
Las principales tecnologías utilizadas son:
* Maven
* Spring MVC
* Swi-prolog?

