package com.gmail.francozanardi97.app.dto;

public class RamaTree {
	protected int padre, hijo;
	protected int id;
	
	public RamaTree(int id, int padre, int hijo) {
		this.padre = padre;
		this.hijo = hijo;
		this.id = id;
	}
	
	public int getPadre() {
		return padre;
	}
	
	public void setPadre(int padre) {
		this.padre = padre;
	}
	
	public int getHijo() {
		return hijo;
	}
	
	public void setHijo(int hijo) {
		this.hijo = hijo;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}


	
}
