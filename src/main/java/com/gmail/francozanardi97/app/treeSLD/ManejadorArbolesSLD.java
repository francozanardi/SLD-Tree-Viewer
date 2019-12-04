package com.gmail.francozanardi97.app.treeSLD;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gmail.francozanardi97.app.model.ProgramaUsuario;

@Component
public class ManejadorArbolesSLD {
	
	private ServletContext servletContext;
	
	private Map<String, ArbolSLD> arboles;
	
	private final static String PATH_PROLOG_FILES = "/prolog";
	private final static String PATH_METAINTERPRETE = PATH_PROLOG_FILES + "/sld_tree.pl";
	
	@Autowired
	public ManejadorArbolesSLD(ServletContext sc) {
		arboles = new HashMap<String, ArbolSLD>();
		
		servletContext = sc;
		cargarMetainterprete(servletContext.getRealPath(PATH_METAINTERPRETE));
	}
	
	
	private void cargarMetainterprete(String path) {
		System.out.println("Path servlet: " + path);
		Query q = 
		        new Query( 
		            "consult", 
		            new Term[] {new Atom(path)} 
		        );
		
		q.oneSolution();
		
//		System.out.println("q.hasSolution(): " + q.hasSolution());
		//esto no solo verifica si existe una solución, sino que también abre la solución encontrada.
		//por lo tanto es necesario hacerlo.
		q.close();
		
	}
	
	public ArbolSLD agregarArbolSLD(String name, ProgramaUsuario p) {
		ArbolSLD arbol = new ArbolSLD(name, servletContext.getRealPath(PATH_PROLOG_FILES + "/" + name + ".pl"), p);
		arboles.put(name, arbol);
		return arbol;
	}
	
	public ArbolSLD getArbolSLD(String name) {
		return arboles.get(name);
	}
	
	public void eliminarArbol(String name) {
		Query q = new Query(String.format("eliminarArbol('%s')", name));
		q.allSolutions();
		q.close();
		
		arboles.remove(name);
	}
	

}
