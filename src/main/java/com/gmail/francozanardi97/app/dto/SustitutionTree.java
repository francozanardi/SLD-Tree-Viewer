package com.gmail.francozanardi97.app.dto;

public class SustitutionTree {
	private int id;
	private int idNodo; //esta es la id del nodo hijo de la rama en donde se encuentra la sustitucion. Es decir, el nodo que provocó dicha sustitución.
	private String sustitution;
	
	public SustitutionTree(int id, int idNodo, String sust) {
		this.id = id;
		this.idNodo = idNodo;
		this.sustitution = sust;
	}
	
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}

	public int getIdNodo() {
		return idNodo;
	}

	public void setIdNodo(int idNodo) {
		this.idNodo = idNodo;
	}

	public String getSustitution() {
		return sustitution;
	}
	public void setSustitution(String sustitution) {
		this.sustitution = sustitution;
	}
	
	
}
