package com.gmail.francozanardi97.app.treeSLD;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
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
		queryCrearSLD = 
		        new Query( 
			            "crearSLD", 
			            new Term[] {new Atom(programaUsuario.getQueryProlog()), new Atom(namePU), new Atom(pathPU)} 
			        );
		
		queryCrearSLD.oneSolution();
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
	
	public NodoTree[] getNodosActuales() {	
		Query q = new Query(String.format("arbol('%s', nodo(ID, _, %d, Rotulo))", namePU, fotogramaActual));
		
		
		Map<String, Term>[] soluciones = q.allSolutions();
		NodoTree[] nodos = new NodoTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			nodos[i] = new NodoTree(Integer.parseInt(soluciones[i].get("ID").toString()), soluciones[i].get("Rotulo").toString());
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
		Query q = new Query(String.format("arbol('%s', rama(ID, %d, NodoP, NodoH, Cut)), Cut \\= -1", namePU, fotogramaActual));
		
		
		Map<String, Term>[] soluciones = q.allSolutions();
		RamaTree[] ramas = new RamaTree[soluciones.length];
		
		for(int i = 0; i < soluciones.length; i++) {
			if(!soluciones[i].get("Cut").toString().equals("-1")) {
				ramas[i] = new RamaTree(
											Integer.parseInt(soluciones[i].get("ID").toString()),
											Integer.parseInt(soluciones[i].get("NodoP").toString()),
											Integer.parseInt(soluciones[i].get("NodoH").toString())
										);
			}
		}	
		
		q.close();
		
		return ramas;
	}
	
	
}
