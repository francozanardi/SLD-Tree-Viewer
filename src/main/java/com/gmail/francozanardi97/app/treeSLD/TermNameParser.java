package com.gmail.francozanardi97.app.treeSLD;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;

import org.jpl7.JPLException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

import com.gmail.francozanardi97.app.dto.NodoTree;
import com.gmail.francozanardi97.app.dto.ProgramaUsuario;

public class TermNameParser {
	
	private Map<String, String> repVars;
	private String varName;
	private NodoTree raiz;
	
	public TermNameParser() {
		this.varName = "";
		this.raiz = null;
	}
	
	private String toInfix(Term term) {
		Query q;
		boolean hasSolution;
		
		if(term.type() != Prolog.COMPOUND) {
			if(term.type() == Prolog.ATOM) {
				return term.name();
			}
			return term.toString();
		}
		
		
		if(term.arity() == 1) {
			q = new Query(String.format("current_op(_, fy, '%s'); current_op(_, fx, '%s')", term.name(), term.name()));
			hasSolution = q.hasSolution();
			q.close();
			
			if(hasSolution) {
				return term.name() + "" + toInfix(term.arg(1));
			} else {
				return term.name() + "(" + toInfix(term.arg(1)) + ")";
			}
			
			
		} 
		
		if(term.arity() == 2) {

			if(term.name().equals("[|]") || term.name().equals("'[|]'") ) {
				System.out.println("-----> term: " + term.toString());
				try {
					return termArrayToInfix(term.toTermArray());
				} catch(JPLException e) {
					return "[" + toInfix(term.arg(1)) + " | " + toInfix(term.arg(2)) + "]";
				}
				
				
			}

			q = new Query(String.format("current_op(_, xfy, '%s'); current_op(_, xfx, '%s'); current_op(_, yfx, '%s')", term.name(), term.name(), term.name()));
			hasSolution = q.hasSolution();
			q.close();

			if(hasSolution) {
				String arg1 = toInfix(term.arg(1));
				String arg2 = toInfix(term.arg(2));
				
				if(term.name().equals("+") && arg2.charAt(0) == '-') {
					return "(" + arg1 + " - " + arg2.substring(1) + ")";
				}
				
				return "(" + arg1 + " " + term.name()  + " " + arg2 + ")";
			} else {
				return term.name() + "(" + toInfix(term.arg(1)) + ", " + toInfix(term.arg(2)) + ")";
			}

		}
		
		if(term.arity() > 2) {
			String pred = term.name() + "(";
			
			for(int i = 1; i <= term.arity(); i++) {
				 pred += toInfix(term.arg(i)) + ", ";
			}
			
			return pred.substring(0, pred.length()-2) + ")";
		}
		
		
		return term.toString();

		
	}
	
	public String termArrayToInfix(Term[] terms) {
		String inf[] = new String[terms.length];
		
		for(int i = 0; i < terms.length; i++) {
			String infix = toInfix(terms[i]);
			inf[i] = infix;
		}
		
		return Arrays.toString(inf);
	}
	
	
	public void init(NodoTree raiz, ProgramaUsuario pu){
		this.raiz = raiz;
		String consulta = pu.getQueryProlog();
		String rotRaiz = raiz.getRotulo().substring(1, raiz.getRotulo().length()-1);
		
		repVars = new Hashtable<>();
		
		int c = 0;
		int r = 0;
		
		String varName;
		String varRep;
		while(c < consulta.length() && r < rotRaiz.length()) {
			if(consulta.charAt(c) != rotRaiz.charAt(r)) {
				varName = "";
				varRep = "" + rotRaiz.charAt(r++);
				
				for(; r < rotRaiz.length() && Character.isDigit(rotRaiz.charAt(r)); r++) {
					varRep += rotRaiz.charAt(r);
				}
				
				if(r < rotRaiz.length()) {
					for(; c < consulta.length() && rotRaiz.charAt(r) != consulta.charAt(c); c++) {
						varName += consulta.charAt(c);
					}	
				} else {
					varName = consulta.substring(c, consulta.length());
				}

				
				repVars.put(varRep, varName);
				
			}
			
			c++;
			r++;
		}
		
	}

	
	private boolean estaEntreComillas(int charIndex, String string) {
		boolean doblesAbiertas = false;
		boolean simplesAbiertas = false;
		
		if(charIndex >= string.length()) {
			return false;
		}
		
		int i = 0;
		while(i < charIndex) {
			if(string.charAt(i) == '"' && !simplesAbiertas) {
				doblesAbiertas = !doblesAbiertas;
			} else if(string.charAt(i) == '\'' && !doblesAbiertas) {
				simplesAbiertas = !simplesAbiertas;
			}
			
			i++;
		}
		
		return simplesAbiertas || doblesAbiertas;
	}
	
	public String replaceRepVars(String rotuloNodo) {
		if(raiz == null) {
			return "";
		}
		
		String newRot = "";
		String rep;
		
		int i = 0;
		while(i < rotuloNodo.length()) {
			if(rotuloNodo.charAt(i) == '_' && !estaEntreComillas(i, rotuloNodo)) {
				rep = "_";
				i++;
				while(i < rotuloNodo.length() && Character.isDigit(rotuloNodo.charAt(i))) {
					rep += rotuloNodo.charAt(i++);
				}
				
				String var = repVars.get(rep);
				if(var != null) {
					newRot += var;
				} else {
					updateVarName();
					newRot += varName;
					repVars.put(rep, varName);
				}
				
			} else {
				newRot += rotuloNodo.charAt(i);
				i++;
			}
			
			
		}
		
		return newRot;
	}
	
	
	private void updateVarName() {
		if(varName.length() == 0) {
			varName = "A";
			if(repVars.get(varName) != null) {
				updateVarName();
			}
			
		} else {
			
			int i = varName.length()-1;
			char ultimoChar;
			boolean huboReemplazo = false;
			
			while(i >= 0) {
				ultimoChar = varName.charAt(i);
				
				if(ultimoChar == 90) {
					varName = varName.substring(0, i) + 'A' + varName.substring(i+1);
					huboReemplazo = true;
				} else {
					break;
				}
				
				
				i--;
			}
			
			ultimoChar = varName.charAt(varName.length()-1);
			char newChar = (char) (ultimoChar+1);
			
			if(huboReemplazo) {
				varName += newChar;
			} else {
				varName = varName.substring(0, varName.length()-1) + newChar;
			}
			
			
			if(repVars.get(varName) != null) {
				updateVarName();
			}
		}

	}
	
	public String parseSustitution(String sust) {
		if(raiz == null) {
			return "";
		}
		
		String sustReplaced = replaceRepVars(sust);
		sustReplaced = sustReplaced.replaceAll(" \\=", "\\,");
		
		return sustReplaced;
	}

}
