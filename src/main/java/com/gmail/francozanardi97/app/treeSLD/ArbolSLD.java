package com.gmail.francozanardi97.app.treeSLD;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.JPLException;
import org.jpl7.Util;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;
import org.springframework.stereotype.Component;

import com.gmail.francozanardi97.app.model.NodoTree;
import com.gmail.francozanardi97.app.model.ProgramaUsuario;
import com.gmail.francozanardi97.app.model.RamaTree;

public class ArbolSLD {
	
	private Query queryCrearSLD;
	private int fotogramaActual;
	private String pathPU;
	private String namePU;
	private ProgramaUsuario programaUsuario;
	
	public ArbolSLD(String namePU, String pathPU, ProgramaUsuario pu) {
		fotogramaActual = 0;
		
		this.pathPU = pathPU;
		this.namePU = namePU;
		this.programaUsuario = pu;
		
		crearArbolSLD();
	}
	

	private void crearArbolSLD() {
		cargarUserProgram();
		createQuerySLD();
	}
	
	private void createQuerySLD() {
		queryCrearSLD = new Query(String.format("crearSLD((%s), '%s', '%s')", programaUsuario.getQueryProlog(), namePU, pathPU.replace('\\', '/')));
//		        new Query( 
//			            "crearSLD", 
//			            new Term[] {new Atom(programaUsuario.getQueryProlog()), new Atom(namePU), new Atom(pathPU)} 
//			        );
				
				
		
		queryCrearSLD.allSolutions();
	}
	
	private void cargarUserProgram() {
		File f = new File(pathPU);
		try {
			FileWriter w = new FileWriter(f);
			w.append(programaUsuario.getSourceCode());
			w.flush();
			w.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public boolean nextSolution() {
//		if(!queryCrearSLD.hasMoreSolutions()) {
//			System.out.println("No hay más soluciones mostro");
//			return false;
//		}
		
//		queryCrearSLD.oneSolution();
//		
//		queryCrearSLD.open();
//		queryCrearSLD.hasMoreSolutions();
//		queryCrearSLD.nextSolution();
//		queryCrearSLD.hasMoreSolutions();
//		queryCrearSLD.nextSolution();
//		System.out.println("Hay más soluciones capoide");
		return true;
	}
	
	public boolean hasMoreSolutions() {
		return queryCrearSLD.hasMoreSolutions();
	}

	
	public void siguienteFotograma() {
		fotogramaActual++;
	}
	
	public int getFotograma() {
		return fotogramaActual;
	}
	
	/*
	private void crearQueryArbol() {
		queryArbol = new Query("arbol", new Term[] {new Atom(namePU), new Variable("E")});
		
		for(Map<String, Term> s : queryArbol.allSolutions()) {
			if(s.get("E").name().equals("nodo")) {
				Term[] t = s.get("X").arg(4).toTermArray();
				for(Term ta: t) {
					System.out.println("Aux: " + ta.toString());
				}
			}
		}
		queryArbol.close();
	}*/
	
	
	private String toInfix(Term term) {
		String infix = "";
	
		while(term.arity() == 2) {
			infix += term.name();
		}
		return null;
	}
	
	private String toPrefix(Term term) {
		if(term.type() == Prolog.COMPOUND && term.arity() == 1) {
			return term.name() + " " + term.arg(1);
		} else {
			if(term.type() == Prolog.COMPOUND && term.arity() == 2) {
				return term.name() + "(" + toInfix_(term.arg(1)) + ", " + toInfix_(term.arg(2)) + ")";
			} else {
				return term.toString();
			}
		}
	}
	
	
	private String toInfix_(Term term) {
		Query q;
		boolean hasSolution;
		
		if(term.type() != Prolog.COMPOUND) {
			return term.toString();
		}
		
		
		if(term.arity() == 1) {
			q = new Query(String.format("current_op(_, fy, %s); current_op(_, fx, %s)", term.name(), term.name()));
			hasSolution = q.hasSolution();
			q.close();
			
			if(hasSolution) {
				return term.name() + "" + toInfix_(term.arg(1));
			} else {
				return term.name() + "(" + toInfix_(term.arg(1)) + ")";
			}
			
			
		} 
		
		if(term.arity() == 2) {

			if(term.name().equals("[|]") || term.name().equals("'[|]'") ) {
				System.out.println("-----> term: " + term.toString());
				try {
					return termArrayToInfix(term.toTermArray());
				} catch(JPLException e) {
					return "[" + toInfix_(term.arg(1)) + " | " + toInfix_(term.arg(2)) + "]";
				}
				
				
			}

			q = new Query(String.format("current_op(_, xfy, %s); current_op(_, xfx, %s); current_op(_, yfx, %s)", term.name(), term.name(), term.name()));
			hasSolution = q.hasSolution();
			q.close();

			if(hasSolution) {
				return "(" + toInfix_(term.arg(1)) + " " + term.name() + " " + toInfix_(term.arg(2)) + ")";
			} else {
				return term.name() + "(" + toInfix_(term.arg(1)) + ", " + toInfix_(term.arg(2)) + ")";
			}

		}
		
		if(term.arity() > 2) {
			String pred = term.name() + "(";
			
			for(int i = 1; i <= term.arity(); i++) {
				 pred += toInfix_(term.arg(i)) + ", ";
			}
			
			return pred.substring(0, pred.length()-2) + ")";
		}
		
		
		return term.toString();

		
	}
	
	private String termArrayToInfix(Term[] terms) {
		String inf[] = new String[terms.length];
		
		for(int i = 0; i < terms.length; i++) {
			String infix = toInfix_(terms[i]);
			inf[i] = infix;
		}
		
		return Arrays.toString(inf);
	}
	
	public NodoTree getRaiz() {
		Query q = new Query(String.format("arbol('%s', nodo(ID, -1, %d, Rotulo))", namePU, fotogramaActual));

		Map<String, Term> solucion = q.oneSolution();
		
		NodoTree raiz;

		raiz = new NodoTree(
								Integer.parseInt(solucion.get("ID").toString()),
								-1,
								termArrayToInfix(solucion.get("Rotulo").toTermArray())
							);
		
		q.close();
		
		return raiz;
	}
	
	public NodoTree[] getNodosActuales() {	
		Query q = new Query(String.format("arbol('%s', nodo(ID, IDPadre, %d, Rotulo)), arbol('%s', rama(IDRama, _, IDPadre, ID, _))", namePU, fotogramaActual, namePU));
		
		
		Map<String, Term>[] soluciones = q.allSolutions();
		NodoTree[] nodos = new NodoTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			nodos[i] = new NodoTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("IDRama").toString()),
										termArrayToInfix(soluciones[i].get("Rotulo").toTermArray())
									);
		}	
		
		q.close();
		
		return nodos;
	}
	
	public RamaTree[] getRamasActuales() {	
		Query q = new Query(String.format("arbol('%s', rama(ID, %d, NodoP, NodoH, _))", namePU, fotogramaActual));
			
		Map<String, Term>[] soluciones = q.allSolutions();
		RamaTree[] ramas = new RamaTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			ramas[i] = new RamaTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("NodoP").toString()),
										Integer.parseInt(soluciones[i].get("NodoH").toString())
									);
		}	
		
		q.close();
		
		return ramas;
	}
	
	public RamaTree[] getRamasCutActuales() {	
		Query q = new Query(String.format("arbol('%s', rama(ID, _, NodoP, NodoH, %d))", namePU, fotogramaActual));
		
		
		Map<String, Term>[] soluciones = q.allSolutions();
		RamaTree[] ramas = new RamaTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			ramas[i] = new RamaTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("NodoP").toString()),
										Integer.parseInt(soluciones[i].get("NodoH").toString())
									);
		}	
		
		q.close();
		
		return ramas;
	}
	
//	public void eliminarArbol() {
//		Query q = new Query(String.format("eliminarArbol('%s')", namePU));
//		q.allSolutions();
//		q.close();
//	}
//	
	
}
