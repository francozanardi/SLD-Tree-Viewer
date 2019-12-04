package com.gmail.francozanardi97.app.controller;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.ResourceUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.ContextLoader;
import org.springframework.web.context.request.RequestContextHolder;

import com.gmail.francozanardi97.app.model.NodoTree;
import com.gmail.francozanardi97.app.model.ProgramaUsuario;
import com.gmail.francozanardi97.app.model.RamaTree;
import com.gmail.francozanardi97.app.treeSLD.ArbolSLD;
import com.gmail.francozanardi97.app.treeSLD.ManejadorArbolesSLD;

import treeSLD.TreeSLD;

@Controller
//@RequestMapping(value="/")
public class InicioController {
	
//	@Autowired mejor no lo hacemos un bean, creemos que no es correcto.
//	@Autowired
//	private ServletContext servletContext;
	
	@Autowired
	private ManejadorArbolesSLD manejadorArbol;

	@RequestMapping(value="/", method=RequestMethod.GET)
	public String prepararForm(Model model) {
		
		System.out.println("session: " + RequestContextHolder.currentRequestAttributes().getSessionId());

//		System.out.println("servletContext: " + servletContext);
	

		return "inicio";
	}
	
	
	//funciona
	@RequestMapping(value="/p", method=RequestMethod.POST)
	public String testPost(@ModelAttribute Test test) {
		System.out.println("test: " + test.getStr());
		System.out.println("Hola :D");
		return "inicio";
	}
	
	//funciona con: $.post('q', 'str=hola :v', function(obj){console.log('-> ', obj);});
	@RequestMapping(value="/q", method=RequestMethod.POST)
	public @ResponseBody Test testPost2(@ModelAttribute Test tRecibido) {
		Test test = new Test();
		test.setStr("jeje " + tRecibido.getStr());
		System.out.println("Hola XD " + tRecibido.getStr());
		return test;
	}
	
	@RequestMapping(value="/q", method=RequestMethod.GET)
	public String testGet2() {
		return "q";
	}
	
	@RequestMapping(value="/", method=RequestMethod.POST)
	public @ResponseBody NodoTree crearSLD(@ModelAttribute ProgramaUsuario p) {
		String sessionID = RequestContextHolder.currentRequestAttributes().getSessionId();
		ArbolSLD arbol;
		
		if(p != null && !p.getQueryProlog().isEmpty() && !p.getSourceCode().isEmpty()) {
			arbol = manejadorArbol.agregarArbolSLD(sessionID, p);
			return arbol.getRaiz();
		}
		
		//acá debería obtener los nodos en el fot actual y enviarlo, podría ser en un array. hasta ahí todo piola.
		//el problema sería cuando haga un next, ya que ahi debería obtener tanto los nodos, como las ramas y las ramas cuts.
		
		return null;
	}
	
	@RequestMapping("/avanzarFotograma")
	public @ResponseBody Integer avanzarFotograma() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		ArbolSLD arbol = manejadorArbol.getArbolSLD(session);
		arbol.siguienteFotograma();
		
		return arbol.getFotograma();
	}
	
	@RequestMapping("/getNodos")
	public @ResponseBody NodoTree[] getNodos() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		return manejadorArbol.getArbolSLD(session).getNodosActuales();
	}
	
	@RequestMapping("/getRamas")
	public @ResponseBody RamaTree[] getRamas() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		return manejadorArbol.getArbolSLD(session).getRamasActuales();
	}
	
	@RequestMapping("/getRamasCut")
	public @ResponseBody RamaTree[] getRamasCut() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		return manejadorArbol.getArbolSLD(session).getRamasCutActuales();
	}
	
	
	@RequestMapping("/eliminarArbol")
	public @ResponseBody void eliminarArbol() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		manejadorArbol.eliminarArbol(session);
	}
	
	@RequestMapping("/nextSolution")
	public @ResponseBody void nextSolution() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		ArbolSLD arbol = manejadorArbol.getArbolSLD(session);
		//arbol.nextSolution();
		arbol.siguienteFotograma();
	}
	
	@RequestMapping("/hasMoreSolutions")
	public @ResponseBody boolean hasMoreSolutions() {
		String session = RequestContextHolder.currentRequestAttributes().getSessionId();
		return manejadorArbol.getArbolSLD(session).hasMoreSolutions();
	}
	
	
	
	/*@RequestMapping(value="/", method=RequestMethod.POST)
	public String crearSLD(@ModelAttribute ProgramaUsuario p, BindingResult result, HttpServletRequest req) {
		// Por ahora la validación de errores la hacemos acá, pero se puede separar y usar otra clase, creo que sería más correcto. Leer spring.pdf.
		String sessionID = RequestContextHolder.currentRequestAttributes().getSessionId();
		
		System.out.println("Hola :v");
		
		System.out.println("p.sourceCode(): " + p.getSourceCode());
		System.out.println("p.queryProlog(): " + p.getQueryProlog());
		
		if(p.getSourceCode() == "") {
			result.rejectValue("sourceCode", "codeEmpty");
		}
		
		if(p.getQueryProlog() == "") {
			result.rejectValue("queryProlog", "consultaVacia");
		}
		
		
		if(!result.hasErrors()) {		
			String path = req.getServletContext().getRealPath("/prolog/" + sessionID + ".pl");
			// La idea es que haya una clase que gestione los nombres de los archivos, podría ser siemplemente un contador que vaya incrementado 
			// a medida que le pedimos reservar un nombre.
			
			System.out.println("path: " + path);
			
			String pathMI = req.getServletContext().getRealPath("/prolog/sld_tree.pl");
			
			System.out.println("pathMI: " + pathMI);
			
			cargarSourceCode(path, p);
			cargarMetainterprete(pathMI);
			
			Query queryCrearSLD  = 
			        new Query( 
				            "crearSLD", 
				            new Term[] {new Atom(p.getQueryProlog()), new Atom(sessionID), new Atom(path)} 
				        );
			
			queryCrearSLD.allSolutions();
			
			queryCrearSLD.close();
		
			
			Query queryArbol = new Query("arbol", new Term[] {new Atom(sessionID), new Variable("X")});
			System.out.println("q3 tiene solucion? " + queryArbol.hasSolution());
			for(Map<String, Term> s : queryArbol.allSolutions()) {
				System.out.println("X: " + s.get("X").toString());
				String name = s.get("X").name();
				
				if(name.equals("nodo")) {
					Term[] t = s.get("X").arg(4).toTermArray();
					for(Term ta: t) {
						System.out.println("Aux: " + ta.toString());
					}
				}
			}
			queryArbol.close();
			
			Query queryEliminarArbol = new Query("eliminarArbol", new Term[] {new Atom(sessionID)});
			queryEliminarArbol.allSolutions();
			queryEliminarArbol.close();
		}
		
		
		return "inicio";
	}*/
	
	
//	@RequestMapping("/nextStep")
//	public @ResponseBody String nextStep(@RequestParam("param1") String param) {
//		return "Exitoso " + param;
//	}
	
}
