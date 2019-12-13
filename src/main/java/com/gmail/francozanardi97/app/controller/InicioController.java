package com.gmail.francozanardi97.app.controller;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.jpl7.Atom;
import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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

import com.gmail.francozanardi97.app.domain.NotificacionError;
import com.gmail.francozanardi97.app.dto.NodoTree;
import com.gmail.francozanardi97.app.dto.ProgramaUsuario;
import com.gmail.francozanardi97.app.dto.RamaTree;
import com.gmail.francozanardi97.app.service.ServiceNotificacionError;
import com.gmail.francozanardi97.app.treeSLD.ArbolSLD;
import com.gmail.francozanardi97.app.treeSLD.ManejadorArbolesSLD;


@Controller
public class InicioController {

	@Autowired
	private ServiceNotificacionError serviceNotifError;
	
	@Autowired
	private ManejadorArbolesSLD manejadorArbol;

	@RequestMapping(value="/", method=RequestMethod.GET)
	public String prepararForm(Model model) {
		
		System.out.println("session: " + RequestContextHolder.currentRequestAttributes().getSessionId());

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
	public @ResponseBody ResponseEntity<String> crearSLD(@ModelAttribute ProgramaUsuario p) {
		String error = "";
		if(p.getQueryProlog().endsWith(".")) {
			p.setQueryProlog(p.getQueryProlog().substring(0, p.getQueryProlog().length()-1));
		}
		
		try {
			if(p != null && !p.getQueryProlog().isEmpty()) {
				return new ResponseEntity<>(manejadorArbol.agregarArbolSLD(p), HttpStatus.OK);
			}	
		} catch (PrologException e) {
			error = e.term().arg(1).name();
		} catch (JPLException e) {
			error = e.getMessage();
		} catch (Throwable e) {
			error = "Fallo inesperado";
		} 
		
		return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
	}
	
	@RequestMapping(value="/notificarError", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<Void> notificarError(@RequestParam("id") String id, @RequestParam("descripcion_error") String descripcionError) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
		ProgramaUsuario pu;
		NotificacionError ne;
		
		if(arbol != null) {
			pu = arbol.getProgramaUsuario();
			ne = new NotificacionError(pu, descripcionError);
			try {
				serviceNotifError.guardarNotificacion(ne);
				
				return new ResponseEntity<>(HttpStatus.OK);
			} catch (SQLException e) {
				System.out.println(e.getMessage());
			}
		}
		
		return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
	}
	
	@RequestMapping(value="/getSolucion", method=RequestMethod.POST)
	public @ResponseBody NodoTree getSolucion(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
				
		return arbol.getSolucion();
	}
	
	@RequestMapping(value="/getRaiz", method=RequestMethod.POST)
	public @ResponseBody NodoTree getRaiz(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
				
		return arbol.getRaiz();
	}
	
	@RequestMapping("/getModulos")
	public @ResponseBody void getModulos() {
		Query q = new Query("arbol(M, _)");
		
		for(Map<String, Term> m: q.allSolutions()) {
			System.out.println("-> M = " + m.get("M").toString());
		}
		
		q.close();
	}
	
	@RequestMapping(value="/avanzarFotograma", method=RequestMethod.POST)
	public @ResponseBody Integer avanzarFotograma(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
		arbol.siguienteFotograma();
		
		return arbol.getFotograma();
	}
	
	@RequestMapping(value="/getNodos", method=RequestMethod.POST)
	public @ResponseBody NodoTree[] getNodos(@RequestParam("id") String id) {
		return manejadorArbol.getArbolSLD(id).getNodosActuales();
	}
	
	@RequestMapping(value="/getRamas", method=RequestMethod.POST)
	public @ResponseBody RamaTree[] getRamas(@RequestParam("id") String id) {
		return manejadorArbol.getArbolSLD(id).getRamasActuales();
	}
	
	@RequestMapping(value="/getRamasCut", method=RequestMethod.POST)
	public @ResponseBody RamaTree[] getRamasCut(@RequestParam("id") String id) {
		return manejadorArbol.getArbolSLD(id).getRamasCutActuales();
	}
	
	@RequestMapping(value="/eliminarArbol", method=RequestMethod.POST)
	public @ResponseBody void eliminarArbol(@RequestParam("id") String id) {
		manejadorArbol.eliminarArbol(id);
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
