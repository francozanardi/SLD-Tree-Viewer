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
	
	private final static int TIMEOUT_QUERY = 9;
	
	public ArbolSLD(String namePU, String pathPU, ProgramaUsuario pu) throws Throwable {
		this.fotogramaActual = 0;
		
		this.pathPU = pathPU;
		this.namePU = namePU;
		this.programaUsuario = pu;
		
		mapNodos = new Hashtable<Integer, NodoTree[]>();
		mapRamas = new Hashtable<Integer, RamaTree[]>();
		mapRamasCut = new Hashtable<Integer, RamaTree[]>();
		
		this.solucionActual = 0;
		
		init();
		
	}
	

	
	private void init() throws Throwable {
		try {
			crearArbolSLD();
			initVariables();
			
			int fot = 0;
			NodoTree[] nodos = {_getRaiz()};
			RamaTree[] ramas = {};
			RamaTree[] ramasCut = {};
		
			
			while(nodos.length > 0 || ramas.length > 0 || ramasCut.length > 0) {
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
			throw e;
		} finally {
			close();
		}

		
		
		
	}
	
	private void initVariables() {
		try {
			Query q = new Query(programaUsuario.getQueryProlog());
			valoresVariables = q.allSolutions();
			q.close();
		} catch(PrologException e) {
			
		}

	}
	
	private void crearArbolSLD() throws Throwable {
		cargarUserProgram();
		createQuerySLD();
	}
	
	private void createQuerySLD() throws Throwable {
		Query q = new Query(String.format("crearSLD((%s), '%s', '%s')", programaUsuario.getQueryProlog(), namePU, pathPU.replace('\\', '/')));

				
		ExecutorService executor = Executors.newCachedThreadPool();
		Callable<Void> task = new Callable<Void>() {
			
			@Override
			public Void call() {
				q.allSolutions();
				return null;
			}
		};
		Future<Void> future = executor.submit(task);
		try {
		   future.get(TIMEOUT_QUERY, TimeUnit.SECONDS); 
		} catch (TimeoutException ex) {
			close();
		   throw new JPLException("Timeout");
		} catch (InterruptedException e) {
			close();
			throw new JPLException("Execution interrumped");
		} catch (ExecutionException e) {
			close();
			throw e.getCause();
		} finally {
		   future.cancel(true); // may or may not desire this
		}
		
		q.close();
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
	
	private void close() {
		Query qUnload = new Query(String.format("unload_file('%s')", pathPU.replace('\\', '/')));
		qUnload.allSolutions();
		qUnload.close();

		Query q1 = new Query(String.format("eliminarArbol('%s')", namePU));
		q1.allSolutions();
		q1.close();
		

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
	
	private NodoTree _getRaiz() {	
		Query q = new Query(String.format("arbol('%s', nodo(ID, -1, %d, Rotulo))", namePU, 0));

		Map<String, Term> solucion = q.oneSolution();
		
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

		
		q.close();
		
		return raiz;
	}
	
	private NodoTree[] getNodos(int fotograma) {	
		Query q = new Query(String.format("arbol('%s', nodo(ID, IDPadre, %d, Rotulo)), arbol('%s', rama(IDRama, _, IDPadre, ID, _))", namePU, fotograma, namePU));
		
		
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
	
	private RamaTree[] getRamas(int fotograma) {	
		Query q = new Query(String.format("arbol('%s', rama(ID, %d, NodoP, NodoH, _))", namePU, fotograma));
			
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
	
	private RamaTree[] getRamasCut(int fotograma) {	
		Query q = new Query(String.format("arbol('%s', rama(ID, _, NodoP, NodoH, %d))", namePU, fotograma));
		
		
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
		
}
