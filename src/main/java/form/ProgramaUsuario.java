package form;

public class ProgramaUsuario {
	private String sourceCode;
	private String queryProlog;
//	private boolean occurs_check;
//	private boolean show_substitutions;
	// acá iría si activó occurs check y demás.
	
	public String getQueryProlog() {
		return queryProlog;
	}

	public void setQueryProlog(String queryProlog) {
		this.queryProlog = queryProlog;
	}
	

	public String getSourceCode() {
		return sourceCode;
	}

	public void setSourceCode(String sourceCode) {
		this.sourceCode = sourceCode;
	}

//	public boolean isOccurs_check() {
//		return occurs_check;
//	}
//
//	public void setOccurs_check(boolean occurs_check) {
//		this.occurs_check = occurs_check;
//	}
//
//	public boolean isShow_substitutions() {
//		return show_substitutions;
//	}
//
//	public void setShow_substitutions(boolean show_substitutions) {
//		this.show_substitutions = show_substitutions;
//	}


	
}
