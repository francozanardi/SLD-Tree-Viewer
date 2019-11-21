package treeSLD;

public class NodoTree extends ElementoTree {
	protected String rotulo;
	protected int columnasOcupadas;
	
	public NodoTree(String rot) {
		rotulo = rot;
		columnasOcupadas = 1;
	}

	public String getRotulo() {
		return rotulo;
	}

	public void setRotulo(String rotulo) {
		this.rotulo = rotulo;
	}

	public int getColumnasOcupadas() {
		return columnasOcupadas;
	}

	public void setColumnasOcupadas(int columnasOcupadas) {
		this.columnasOcupadas = columnasOcupadas;
	}
	
	

}
