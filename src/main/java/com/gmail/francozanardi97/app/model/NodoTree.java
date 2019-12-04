package com.gmail.francozanardi97.app.model;

public class NodoTree {
	private String rotulo;
	private int id;
	private int idRama; //sera el id de la rama que conecta a este nodo con el padre.
//	protected int columnasOcupadas;


	public NodoTree(int id, int idRama, String rot) {
		rotulo = rot;
		this.id = id;
		this.idRama = idRama;
//		columnasOcupadas = 1;
	}

	public String getRotulo() {
		return rotulo;
	}

	public void setRotulo(String rotulo) {
		this.rotulo = rotulo;
	}
		
	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public int getIdRama() {
		return idRama;
	}

	public void setIdRama(int idRama) {
		this.idRama = idRama;
	}
	
	

//	public int getColumnasOcupadas() {
//		return columnasOcupadas;
//	}
//
//	public void setColumnasOcupadas(int columnasOcupadas) {
//		this.columnasOcupadas = columnasOcupadas;
//	}
//	
	

}
