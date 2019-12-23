package com.gmail.francozanardi97.app.treeSLD;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Query;
import org.jpl7.Term;

import com.gmail.francozanardi97.app.dto.*;

public class ArbolSLD {
	
	private int fotogramaActual;
	private String pathPU;
	private String namePU;
	private ProgramaUsuario programaUsuario;
	
	private Map<Integer, NodoTree[]> mapNodos;
	private Map<Integer, RamaTree[]> mapRamas;
	private Map<Integer, RamaTree[]> mapRamasCut;
	private Map<Integer, SustitutionTree[]> mapSustitutions;
	
	private Map<String, Term>[] valoresVariables;
	private int solucionActual;
	
	private RotuloParser rotuloParser;

	
	private final static int TIMEOUT_QUERY = 9;
	


	public ArbolSLD(String namePU, String pathPU, ProgramaUsuario pu) throws Throwable {
		this.fotogramaActual = 0;
		
		this.pathPU = pathPU;
		this.namePU = namePU;
		this.programaUsuario = pu;
		
		mapNodos = new Hashtable<>();
		mapRamas = new Hashtable<>();
		mapRamasCut = new Hashtable<>();
		mapSustitutions = new Hashtable<>();
		
		this.solucionActual = 0;
		this.rotuloParser = new RotuloParser();
		init();
		
	}
	

	
	private void init() throws Throwable {
		try {
			crearArbolSLD();
			Map<String, Term> solFot = Query.oneSolution(String.format("datos('%s', fotogramaActual(FotMax))", namePU));
			int fotogramaMax = Integer.parseInt(solFot.get("FotMax").toString());
			
			
			int fot = 0;
			NodoTree[] nodos = {_getRaiz()};
			RamaTree[] ramas = {};
			RamaTree[] ramasCut = {};
			SustitutionTree[] susts = {};
			
			rotuloParser.init(nodos[0], programaUsuario);
			nodos[0].setRotulo(rotuloParser.replaceRepVars(nodos[0].getRotulo()));

			
			while(fot < fotogramaMax) {
				mapNodos.put(fot, nodos);
				mapRamas.put(fot, ramas);
				mapRamasCut.put(fot, ramasCut);
				mapSustitutions.put(fot, susts);
				
				fot++;
				nodos = getNodos(fot);
				ramas = getRamas(fot);
				ramasCut = getRamasCut(fot);
				susts = getSustitutions(fot);

			}
			
		} catch(PrologException e) {
			System.out.println("Prolog Error -> " + e.getMessage());
			e.printStackTrace();
			throw e;
		} finally {
			close();
		}
	
	}

	
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
	
	public SustitutionTree[] getSustitucionesActuales() {
		return mapSustitutions.get(fotogramaActual);
	}
	
	public NodoTree getSolucion() {
		if(valoresVariables != null && solucionActual < valoresVariables.length) {
			Map<String, Term> solActual = valoresVariables[solucionActual++];
			
			String[] rot = new String[solActual.size()];
			int i = 0;
			for(Entry<String, Term> e: solActual.entrySet()) {
				rot[i++] = e.getKey() + " = " + e.getValue().toString();
			}
			
			return new NodoTree(-1, -1, Arrays.toString(rot));
		}
		
		return new NodoTree(-1, -1, "");

	}
		

	private NodoTree _getRaiz() {	
		Map<String, Term> solucion = Query.oneSolution(String.format("arbol('%s', nodo(ID, -1, %d, _, Rotulo))", namePU, 0));
		
		NodoTree raiz;

		if(solucion != null && solucion.size() > 0) {
			raiz = new NodoTree(
					Integer.parseInt(solucion.get("ID").toString()),
					-1,
					rotuloParser.termArrayToInfix(solucion.get("Rotulo").toTermArray())
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
										rotuloParser.termArrayToInfix(soluciones[i].get("Rotulo").toTermArray())
									);
			
			
			nodos[i].setRotulo(rotuloParser.replaceRepVars(nodos[i].getRotulo()));
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
	
	private SustitutionTree[] getSustitutions(int fotograma) {	
		Map<String, Term>[] soluciones = Query.allSolutions(String.format("arbol('%s', sustitucion(ID, %d, IDRama, Sust)), arbol('%s', rama(IDRama, _, _, IDNodo, _))", namePU, fotograma, namePU));
		SustitutionTree[] sustitutions = new SustitutionTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			sustitutions[i] = new SustitutionTree(
										Integer.parseInt(soluciones[i].get("ID").toString()),
										Integer.parseInt(soluciones[i].get("IDNodo").toString()),
										rotuloParser.termArrayToInfix(soluciones[i].get("Sust").toTermArray())
									);
			
			sustitutions[i].setSustitution(rotuloParser.parseSustitution(sustitutions[i].getSustitution()));
		}	

		return sustitutions;
	}
		
}
