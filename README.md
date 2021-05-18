# SLD Tree Generator
Aplicación web en java que permite visualizar un árbol SLD a partir de un código fuente en Prolog y una consulta sobre éste.
![Ejemplo en ejecución](/images/example.png)

## Obtención del proyecto
Clone el repositorio desde GitHub.
```
$ git clone https://github.com/francozanardi/SLD-Tree-Generator.git
```

O descárguelo [aquí](https://github.com/francozanardi/SLD-Tree-Generator/archive/master.zip)

## Pre-requisitos
* [Swi-prolog](https://www.swi-prolog.org/download/stable)
* [JAVA JDK 8u251](https://www.oracle.com/ar/java/technologies/javase/javase8u211-later-archive-downloads.html) o una versión inferior
* [Maven](http://maven.apache.org/download.cgi)

Para instalar Maven es recomendable seguir esta [guía](http://maven.apache.org/install.html) oficial.

## Instalación
Una vez dentro de la carpeta que contiene el repositorio, debe realizar lo siguiente.
1. Agregue **Swi-prolog** a la variable de entorno _PATH_ en caso de que no lo esté.
2. Es importante que se haya agregado la variable de entorno **JAVA_HOME** y que esta tenga la ruta de una versión menor o igual a Java 8u251. Teniendo configurado correctamente la variable de entorno _PATH_ para que use la variable **JAVA_HOME**.
3. Utilizando la consola, ubíquese en el directorio donde haya descargado el repositorio.
4. Descargue e instale las dependencias del proyecto con [Maven](http://maven.apache.org/).
```
    $ mvn clean install
```

## Ejecución
Ejecute la web utilizando **Tomcat7**.
```
    $ mvn tomcat7:run
```
Podrá acceder a la web desde 
```
    http://localhost:8080/sld_tree_generator
```

## Principales tecnologías utilizadas
* [Maven](http://maven.apache.org/)
* [Spring MVC](https://spring.io/)
* [Swi-prolog](https://www.swi-prolog.org/)

## Estado actual
El proyecto se encuentra en etapa de desarrollo y su documentación está incompleta. Además pueden haber casos no verificados que provoquen errores inesperados.

## Alcance del proyecto
Predicados predefinidos simulados.
* [,/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%27,%27)/2)
* [;/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3B)/2)
* [->/2](https://www.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2)
* [not/1](https://www.swi-prolog.org/pldoc/man?predicate=not/1)
* [!/0](https://www.swi-prolog.org/pldoc/doc_for?object=!/0)
* [repeat/0](https://www.swi-prolog.org/pldoc/doc_for?object=repeat/0)

Además debemos considerar que:
* El programa también tiene en cuenta el uso de predicados dinámicos. Es decir, se tiene el funcionamiento esperado con [assert/1](https://www.swi-prolog.org/pldoc/man?predicate=assert/1), [retract/1](https://www.swi-prolog.org/pldoc/doc_for?object=retract/1) y otros.
* Las sustituciones realizadas pueden visualizarse al presionar las ramas del árbol. Éstas se encuentran en etapa de desarrollo y pueden presentar errores.
* Internamente se calcula todo el árbol SLD antes de mostrarse. Por lo cual, si la cantidad de soluciones de la consulta es potencialmente _infinita_, no podremos obtener un resultado.

## Detalles técnicos
Internamente, el proyecto consta de:
* Un programa escrito en Prolog el cual funciona como meta-intérprete. A partir de otro programa en Prolog y una consulta sobre éste, genera el árbol SLD de dicha consulta.
* La aplicación web realizada en Java, utilizando el framework [Spring MVC](https://spring.io/). Ésta se encarga de resolver las peticiones del cliente, recibiendo como datos el código fuente de un programa y la respectiva consulta. Luego, deriva estos datos al generador del árbol SLD mencionado, para ello utiliza la librería [JPL](https://jpl7.org/). Finalmente, los elementos del árbol (nodos, ramas) son enviados al cliente a medida que los solicite.
* La vista principal, donde se solicita la entrada de datos y se realiza la representación gráfica del árbol. Para ello se hace uso de Javascript, en donde se realizan las solicitudes al servidor utilizando AJAX y se representa el árbol SLD utilizando una modificación de la librería [Treant JS](https://fperucic.github.io/treant-js/)

