CREATE DATABASE sldtree;

USE sldtree;

CREATE TABLE notificacion_error(
	id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	query VARCHAR(128) NOT NULL,
	source_code TEXT NOT NULL,
	descripcion TEXT NOT NULL,
	
	
	CONSTRAINT PK_notificacion_error
	PRIMARY KEY (id)
	
) ENGINE=InnoDB;

CREATE USER 'admin'@'localhost' IDENTIFIED BY 'admin';

GRANT ALL PRIVILEGES ON sldtree.* TO 'admin'@'localhost' WITH GRANT OPTION;