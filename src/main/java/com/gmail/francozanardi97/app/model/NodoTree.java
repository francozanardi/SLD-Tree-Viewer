package com.gmail.francozanardi97.app.model;

public class NodoTree {
	private String rotulo;
	private int id;
//	protected int columnasOcupadas;


	public NodoTree( int id, String rot) {
		rotulo = rot;
		this.id = id;
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

//	public int getColumnasOcupadas() {
//		return columnasOcupadas;
//	}
//
//	public void setColumnasOcupadas(int columnasOcupadas) {
//		this.columnasOcupadas = columnasOcupadas;
//	}
//	
	

}
