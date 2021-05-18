package com.gmail.francozanardi97.app.treeSLD;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.DigestUtils;

import com.gmail.francozanardi97.app.dto.ProgramaUsuario;

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
		//con JPL > 7.4 tendríamos que usar:
		//Query q = new Query(new Compound("consult", new Term[] {new Atom(path)}));
		
		Query q = 
		        new Query( 
		            "consult", 
		            new Term[] {new Atom(path)} 
		        );
		
		q.oneSolution();
		
		q.close();
		
	}
	
	private String generateNewTreeID() throws IOException {
		String time = System.nanoTime() + "";
		InputStream stream = new ByteArrayInputStream(time.getBytes(StandardCharsets.UTF_8));

		return DigestUtils.md5DigestAsHex(stream);
		
	}
	
	public String agregarArbolSLD(ProgramaUsuario p) throws Throwable {
		String name = generateNewTreeID();
		
		ArbolSLD arbol = new ArbolSLD	(
											name,
											servletContext.getRealPath(PATH_PROLOG_FILES + "/" + name + ".pl"),
											p
										);
		
		arboles.put(name, arbol);

		return name;
	}
		
	public ArbolSLD getArbolSLD(String name) {
		return arboles.get(name);
	}
	
	public void eliminarArbol(String name) {
		arboles.remove(name);
	}
	

}
