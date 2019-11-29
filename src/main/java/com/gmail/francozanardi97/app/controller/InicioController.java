package com.gmail.francozanardi97.app.controller;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
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

import form.ProgramaUsuario;
import treeSLD.TreeSLD;

@Controller
//@RequestMapping(value="/")
public class InicioController {
	
//	@Autowired mejor no lo hacemos un bean, creemos que no es correcto.

	@RequestMapping(value="/", method=RequestMethod.GET)
	public String prepararForm(Model model) {
		
		System.out.println("session2: " + RequestContextHolder.currentRequestAttributes().getSessionId());

		return "inicio";
	}
	
//	@RequestMapping(value="/", method=RequestMethod.POST)
//	public String crearSLD(@RequestBody String test) {
//		System.out.println("Hola :V.");
//		return "inicio";
//	}
	
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
			
			System.out.println("p.getQueryProlog(): " + p.getQueryProlog());
			try {
//				Query q2 = new Query("crearSLD(" + p.getQueryProlog() + ", sourceUser).");
				System.out.println("print0.");
//				Query q2 = new Query("crearSLD(p).");

				Query q2  = 
				        new Query( 
					            "crearSLD", 
					            new Term[] {new Atom(p.getQueryProlog()), new Atom(sessionID), new Atom(path)} 
					        );
				
				q2.allSolutions();
				
//				Query q2  = 
//				        new Query( 
//					            "tst", 
//					            new Term[] {new Atom("p"), new Atom("sourceUser")} 
//					        );
//				System.out.println("q2.hasSolution(): " + q2.hasSolution());
//				System.out.println("print0.5.");
				
				
				q2.close();
				System.out.println("print2.");
			} catch (Exception e) {
				System.out.println("message: " + e.getMessage());
				e.printStackTrace();
			}
		
			
			Query q3 = new Query("arbol", new Term[] {new Atom(sessionID), new Variable("X")});
			System.out.println("q3 tiene solucion? " + q3.hasSolution());
			for(Map<String, Term> s : q3.allSolutions()) {
				System.out.println("X: " + s.get("X").toString());
				String name = s.get("X").name();
				
				if(name.equals("nodo")) {
					Term[] t = s.get("X").arg(4).toTermArray();
					for(Term ta: t) {
						System.out.println("Aux: " + ta.toString());
					}
				}
			}
			q3.close();
			
			Query q4 = new Query("eliminarArbol", new Term[] {new Atom(sessionID)});
			q4.allSolutions();
			q4.close();
		}
		
		
		return "inicio";
	}
	
/*	@RequestMapping(params="nextStep")
	public String avanzarSLD(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//		model.addAttribute("hayNext", new Boolean(true));
		System.out.println("Next presionado1");
		
		
		return "inicio";
	}*/
	
	public void cargarSourceCode(String path, ProgramaUsuario programa) {
		File f = new File(path);
		try {
			FileWriter w = new FileWriter(f);
			w.append(programa.getSourceCode());
			w.flush();
			w.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void cargarMetainterprete(String path) {
		Query q = 
		        new Query( 
		            "consult", 
		            new Term[] {new Atom(path)} 
		        );
		
		System.out.println("q.hasSolution(): " + q.hasSolution()); //esto no solo verifica si existe una solución, sino que también abre la solución encontrada.
		//por lo tanto es necesario hacerlo.
		q.close();
		
	}
	
	@RequestMapping("/nextStep")
	public @ResponseBody String nextStep(@RequestParam("param1") String param) {
		return "Exitoso " + param;
	}
	
//	@RequestMapping(params="options", method=RequestMethod.POST)
//	public String abrirOpciones(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//
//		return "inicio";
//	}
}
