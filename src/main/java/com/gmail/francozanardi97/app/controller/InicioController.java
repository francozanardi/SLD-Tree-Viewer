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
	
	@RequestMapping("/getModulos")
	public @ResponseBody void getModulos() {
		Query q = new Query("arbol(M, _)");
		
		for(Map<String, Term> m: q.allSolutions()) {
			System.out.println("-> M = " + m.get("M").toString());
		}
		
		q.close();
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
			e.printStackTrace();
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
		} else {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		
		return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
	}
	
	@RequestMapping(value="/getSolucion", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<NodoTree> getSolucion(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
		if(arbol != null) {
			return new ResponseEntity<>(arbol.getSolucion(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
	}
	
	@RequestMapping(value="/getRaiz", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<NodoTree> getRaiz(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
			
		if(arbol != null) {
			return new ResponseEntity<>(arbol.getRaiz(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
	}
	
	
	@RequestMapping(value="/avanzarFotograma", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<Integer> avanzarFotograma(@RequestParam("id") String id) {
		ArbolSLD arbol = manejadorArbol.getArbolSLD(id);
		
		if(arbol != null) {
			arbol.siguienteFotograma();
			
			return new ResponseEntity<>(arbol.getFotograma(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(-1, HttpStatus.BAD_REQUEST);

	}
	
	@RequestMapping(value="/getNodos", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<NodoTree[]> getNodos(@RequestParam("id") String id) {
		ArbolSLD arbol =  manejadorArbol.getArbolSLD(id);
		
		if(arbol != null) {
			return new ResponseEntity<>(arbol.getNodosActuales(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(new NodoTree[] {}, HttpStatus.BAD_REQUEST);		
	}
	
	@RequestMapping(value="/getRamas", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<RamaTree[]> getRamas(@RequestParam("id") String id) {
		ArbolSLD arbol =  manejadorArbol.getArbolSLD(id);
		
		if(arbol != null) {
			return new ResponseEntity<>(arbol.getRamasActuales(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(new RamaTree[] {}, HttpStatus.BAD_REQUEST);	
	}
	
	@RequestMapping(value="/getRamasCut", method=RequestMethod.POST)
	public @ResponseBody ResponseEntity<RamaTree[]> getRamasCut(@RequestParam("id") String id) {
		ArbolSLD arbol =  manejadorArbol.getArbolSLD(id);
		
		if(arbol != null) {
			return new ResponseEntity<>(arbol.getRamasCutActuales(), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(new RamaTree[] {}, HttpStatus.BAD_REQUEST);	
	}
	
	@RequestMapping(value="/eliminarArbol", method=RequestMethod.POST)
	public @ResponseBody void eliminarArbol(@RequestParam("id") String id) {
		manejadorArbol.eliminarArbol(id);
	}
	
}
