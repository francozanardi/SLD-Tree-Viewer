package com.gmail.francozanardi97.app.treeSLD;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Util;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;
import org.springframework.stereotype.Component;

import com.gmail.francozanardi97.app.dto.NodoTree;
import com.gmail.francozanardi97.app.dto.ProgramaUsuario;
import com.gmail.francozanardi97.app.dto.RamaTree;

public class ArbolSLD {
	
	private int fotogramaActual;
	private String pathPU;
	private String namePU;
	private ProgramaUsuario programaUsuario;
	
	private Map<Integer, NodoTree[]> mapNodos;
	private Map<Integer, RamaTree[]> mapRamas;
	private Map<Integer, RamaTree[]> mapRamasCut;
	
	private Map<String, Term>[] valoresVariables;
	private int solucionActual;
	private Map<String, String> repVars;
	
	private final static int TIMEOUT_QUERY = 9;
	


	public ArbolSLD(String namePU, String pathPU, ProgramaUsuario pu) throws Throwable {
		this.fotogramaActual = 0;
		
		this.pathPU = pathPU;
		this.namePU = namePU;
		this.programaUsuario = pu;
		
		mapNodos = new Hashtable<>();
		mapRamas = new Hashtable<>();
		mapRamasCut = new Hashtable<>();
		
		this.solucionActual = 0;
		
		init();
		
	}
	

	
	private void init() throws Throwable {
		try {
			crearArbolSLD();
			//initVariables();
			Map<String, Term> solFot = Query.oneSolution(String.format("datos('%s', fotogramaActual(FotMax))", namePU));
			int fotogramaMax = Integer.parseInt(solFot.get("FotMax").toString());
			
			
			int fot = 0;
			NodoTree[] nodos = {_getRaiz()};
			RamaTree[] ramas = {};
			RamaTree[] ramasCut = {};
			
			initRepresentacionVars(nodos[0]);
			replaceRepVars(nodos[0]);
		
			
			while(fot < fotogramaMax) {
				mapNodos.put(fot, nodos);
				mapRamas.put(fot, ramas);
				mapRamasCut.put(fot, ramasCut);
				
				fot++;
				nodos = getNodos(fot);
				ramas = getRamas(fot);
				ramasCut = getRamasCut(fot);

			}
			
		} catch(PrologException e) {
			System.out.println("Prolog Error -> " + e.getMessage());
			e.printStackTrace();
			throw e;
		} finally {
			close();
		}

		
		
		
	}
	
//	private void initVariables() {
//		try {
//			deleteDynamicPredicates();
//			valoresVariables = Query.allSolutions(String.format("'%s':(%s)", namePU, programaUsuario.getQueryProlog()));
//		} catch(PrologException e) {
//			System.out.println("No se pudieron inicializar las variables debido a una excepción.");
//			System.out.println("-> " + e.getMessage());
//		}
//
//	}
	
	private void crearArbolSLD() throws Throwable {
		cargarUserProgram();
		createQuerySLD();
	}
	
	private void createQuerySLD() throws Throwable {
		Query q = new Query(String.format("crearSLD((%s), '%s', '%s')", programaUsuario.getQueryProlog(), namePU, pathPU.replace('\\', '/')));
		boolean timeout = false;
		
		List<Map<String, Term>> sols = new ArrayList<>();
		
		long tiempoInicialms = System.currentTimeMillis();
		while(q.hasMoreSolutions() && !timeout) {
			sols.add(q.nextSolution());
			timeout = (tiempoInicialms+(TIMEOUT_QUERY*1000)) <= System.currentTimeMillis();
		}

		
		if(timeout) {
			close();
			throw new JPLException("Timeout");
		} else {
			valoresVariables = sols.toArray((Map<String, Term>[]) new Map[sols.size()]);
		}
		
		q.close();
	}
	
	private void cargarUserProgram() {
		File f = new File(pathPU);
		

		try {
			FileWriter w = new FileWriter(f);
			w.append(String.format(":- module('%s', []). \n\n", namePU));
			w.append(programaUsuario.getSourceCode());
			w.flush();
			w.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void deleteDynamicPredicates() {
		Query.allSolutions(String.format("predicate_property('%s':X, dynamic), retractall('%s':X), compile_predicates(['%s':X])", namePU, namePU, namePU));
	}
	
	private void close() {		
		Query.allSolutions(String.format("unload_file('%s')", pathPU.replace('\\', '/')));
		Query.allSolutions(String.format("eliminarArbol('%s')", namePU));
		deleteDynamicPredicates();
		
		File f = new File(pathPU);
		f.delete();
	}
	
	public ProgramaUsuario getProgramaUsuario() {
		return programaUsuario;
	}

	public void siguienteFotograma() {
		fotogramaActual++;
	}
	
	public int getFotograma() {
		return fotogramaActual;
	}
	
	public NodoTree getRaiz() {
		NodoTree[] r = mapNodos.get(0);
		
		if(r != null && r.length > 0) {
			return r[0];
		}
		
		return new NodoTree(-1, -1, "[fail]");
	}
	
	public NodoTree[] getNodosActuales() {	
		return mapNodos.get(fotogramaActual);
	}
	
	public RamaTree[] getRamasActuales() {
		return mapRamas.get(fotogramaActual);
	}
	
	public RamaTree[] getRamasCutActuales() {
		return mapRamasCut.get(fotogramaActual);
	}
	
	public NodoTree getSolucion() {
		if(valoresVariables != null && solucionActual < valoresVariables.length) {
			Map<String, Term> solActual = valoresVariables[solucionActual++];
			
			String[] rot = new String[solActual.size()];
			int i = 0;
			for(Entry<String, Term> e: solActual.entrySet()) {
				rot[i++] = e.getKey() + ":" + e.getValue().toString();
			}
			
			return new NodoTree(-1, -1, Arrays.toString(rot));
		}
		
		return new NodoTree(-1, -1, "");

	}
		
	
	private String toInfix_(Term term) {
		Query q;
		boolean hasSolution;
		
		if(term.type() != Prolog.COMPOUND) {
			if(term.type() == Prolog.ATOM) {
				return term.name();
			}
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
				String arg1 = toInfix_(term.arg(1));
				String arg2 = toInfix_(term.arg(2));
				
				if(term.name().equals("+") && arg2.charAt(0) == '-') {
					return "(" + arg1 + " - " + arg2.substring(1) + ")";
				}
				
				return "(" + arg1 + " " + term.name()  + " " + arg2 + ")";
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

	
	
	private void initRepresentacionVars(NodoTree raiz){
		String consulta = programaUsuario.getQueryProlog();
		String rotRaiz = raiz.getRotulo().substring(1, raiz.getRotulo().length()-1);
		
		repVars = new Hashtable<>();
		
		int c = 0;
		int r = 0;
		
		//System.out.println("consulta: " + consulta);
		//System.out.println("raiz: " + rotRaiz);
		
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

				
				//System.out.println("varRep: " + varRep);
				//System.out.println("varName: " + varName);
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
	
	private void replaceRepVars(NodoTree nodo) {
		String newRot = "";
		String rot = nodo.getRotulo();
		String rep;
		
		int i = 0;
		while(i < rot.length()) {
			if(rot.charAt(i) == '_' && !estaEntreComillas(i, rot)) {
				rep = "_";
				i++;
				while(i < rot.length() && Character.isDigit(rot.charAt(i))) {
					rep += rot.charAt(i++);
				}
				
				String varName = repVars.get(rep);
				if(varName != null) {
					newRot += varName;
				} else {
					newRot += rep;
				}
				
			} else {
				newRot += rot.charAt(i);
				i++;
			}
			
			
		}
		
		//System.out.println("Rot: " + rot);
		//System.out.println("newRot" + newRot);
		
		nodo.setRotulo(newRot);
	}
	
//	private void putNodos() {
//		List<NodoTree> nodos;
//		//necesitamos hacerlo así, para que las variables manejen la misma representación en prolog y quede más visible para el usuario
//		Map<String, Term>[] soluciones = Query.allSolutions(String.format("arbol('%s', nodo(IDRaiz, _, 0, RotuloRaiz)); (arbol('%s', nodo(ID, IDPadre, Fotograma, Rotulo)), arbol('%s', rama(IDRama, _, IDPadre, ID, _)))", namePU, namePU, namePU));
//		int fot;
//		NodoTree nodo;
//		Term varFot;
//		
//		
//		for(int s = 0; s < soluciones.length; s++) {
//			varFot = soluciones[s].get("Fotograma");
//			
//			if(!varFot.isVariable()) { // es decir, fue instanciado.
//				fot = Integer.parseInt(varFot.toString());
//				nodo = new NodoTree(
//									Integer.parseInt(soluciones[s].get("ID").toString()),
//									Integer.parseInt(soluciones[s].get("IDRama").toString()),
//									soluciones[s].get("Rotulo").name()
//								);
//				
//				nodos = mapNodos.get(fot);
//				if(nodos != null) {
//					nodos.add(nodo);
//				} else {
//					nodos = new ArrayList<>();
//					nodos.add(nodo);
//					mapNodos.put(fot, nodos);
//				}
//				
//				
//				
//			} else {
//				
//				
//				nodo = new NodoTree(
//						Integer.parseInt(soluciones[s].get("IDRaiz").toString()),
//						-1,
//						soluciones[s].get("RotuloRaiz").name()
//					);
//				
//				getRepresentacionVariables(programaUsuario.getQueryProlog(), nodo.getRotulo().substring(1, nodo.getRotulo().length()-1));
//				
//				nodos = new ArrayList<>();
//				nodos.add(nodo);
//				
//				mapNodos.put(0, nodos);
//			}
//			
//			System.out.println("ROT NODO: " + nodo.getRotulo());
//
//		}
//
//	}
	
//	public void putRamas(int fotMax) {
//		Map<String, Term>[] soluciones;
//		RamaTree[] ramas;
//		
//		for(int f = 0; f < fotMax; f++) {
//			soluciones = Query.allSolutions(String.format("arbol('%s', rama(ID, %d, NodoP, NodoH, _))", namePU, f));
//			ramas = new RamaTree[soluciones.length];
//			
//			for(int i = 0; i < soluciones.length; i++) {
//				ramas[i] = new RamaTree(
//											Integer.parseInt(soluciones[i].get("ID").toString()),
//											Integer.parseInt(soluciones[i].get("NodoP").toString()),
//											Integer.parseInt(soluciones[i].get("NodoH").toString())
//										);
//			}
//			
//			mapRamas.put(f, ramas);
//		}
//
//	}
//	
//	public void putRamasCut(int fotMax) {		
//		Map<String, Term>[] soluciones;
//		RamaTree[] ramas;
//		
//		for(int f = 0; f < fotMax; f++) {
//			soluciones = Query.allSolutions(String.format("arbol('%s', rama(ID, _, NodoP, NodoH, %d))", namePU, f));
//			ramas = new RamaTree[soluciones.length];
//			
//			for(int i = 0; i < soluciones.length; i++) {
//				ramas[i] = new RamaTree(
//											Integer.parseInt(soluciones[i].get("ID").toString()),
//											Integer.parseInt(soluciones[i].get("NodoP").toString()),
//											Integer.parseInt(soluciones[i].get("NodoH").toString())
//										);
//			}
//			
//			mapRamasCut.put(f, ramas);
//		}
//	}
	
	
	private NodoTree _getRaiz() {	
		Map<String, Term> solucion = Query.oneSolution(String.format("arbol('%s', nodo(ID, -1, %d, _, Rotulo))", namePU, 0));
		
		NodoTree raiz;

		if(solucion != null && solucion.size() > 0) {
			raiz = new NodoTree(
					Integer.parseInt(solucion.get("ID").toString()),
					-1,
					termArrayToInfix(solucion.get("Rotulo").toTermArray())
				);
		} else {
			raiz = new NodoTree(-1, -1, "[fail]");
		}
		
		return raiz;
	}
	
	private NodoTree[] getNodos(int fotograma) {
		//obtenemos todo nodo que no sea raiz

		Map<String, Term>[] soluciones = Query.allSolutions(String.format("arbol('%s', nodo(ID, IDPadre, %d, _, Rotulo)), arbol('%s', rama(IDRama, _, IDPadre, ID, _))", namePU, fotograma, namePU));
		NodoTree[] nodos = new NodoTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			nodos[i] = new NodoTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("IDRama").toString()),
										termArrayToInfix(soluciones[i].get("Rotulo").toTermArray())
									);
			
			
			replaceRepVars(nodos[i]);
		}	

		return nodos;
	}
	
	private RamaTree[] getRamas(int fotograma) {	
		Map<String, Term>[] soluciones = Query.allSolutions(String.format("arbol('%s', rama(ID, %d, NodoP, NodoH, _))", namePU, fotograma));
		RamaTree[] ramas = new RamaTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			ramas[i] = new RamaTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("NodoP").toString()),
										Integer.parseInt(soluciones[i].get("NodoH").toString())
									);
		}
		
		return ramas;
	}
	
	private RamaTree[] getRamasCut(int fotograma) {	
		Map<String, Term>[] soluciones = Query.allSolutions(String.format("arbol('%s', rama(ID, _, NodoP, NodoH, %d))", namePU, fotograma));
		RamaTree[] ramas = new RamaTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			ramas[i] = new RamaTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("NodoP").toString()),
										Integer.parseInt(soluciones[i].get("NodoH").toString())
									);
		}	

		return ramas;
	}
		
}
