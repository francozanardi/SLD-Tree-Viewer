package treeSLD;

public class RamaTree extends ElementoTree {
	protected NodoTree padre, hijo;
	protected boolean cut;
	
	public RamaTree(NodoTree padre, NodoTree hijo) {
		this.padre = padre;
		this.hijo = hijo;
	}
	
	public NodoTree getPadre() {
		return padre;
	}
	public void setPadre(NodoTree padre) {
		this.padre = padre;
	}
	public NodoTree getHijo() {
		return hijo;
	}
	public void setHijo(NodoTree hijo) {
		this.hijo = hijo;
	}
	public boolean isCut() {
		return cut;
	}
	public void setCut(boolean cut) {
		this.cut = cut;
	}
	
	
}
