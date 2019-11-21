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
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.ResourceUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.ContextLoader;

import form.ProgramaUsuario;
import treeSLD.TreeSLD;

@Controller
@RequestMapping(value="/")
public class InicioController {
	
//	@Autowired mejor no lo hacemos un bean, creemos que no es correcto.
	private TreeSLD generadorTree;

	@RequestMapping(method=RequestMethod.GET)
	public String prepararForm(Model model) {
		ProgramaUsuario p = new ProgramaUsuario();
		generadorTree = new TreeSLD(p);
		model.addAttribute("programaUsuario", p);
		model.addAttribute("svgTreeSLD", null);
		model.addAttribute("hayPrev", new Boolean(false));
		model.addAttribute("hayNext", new Boolean(false));
		
		System.out.println("Se creó el generador!");

		return "inicio";
	}
	
	@RequestMapping(params="createSLD", method=RequestMethod.POST)
	public String crearSLD(@ModelAttribute("programaUsuario") ProgramaUsuario p, HttpSession session, BindingResult result, Model model, HttpServletRequest req) {
		// Por ahora la validación de errores la hacemos acá, pero se puede separar y usar otra clase, creo que sería más correcto. Leer spring.pdf.
		
		if(p.getSourceCode() == "") {
			result.rejectValue("sourceCode", "codeEmpty");
		}
		
		if(p.getQueryProlog() == "") {
			result.rejectValue("queryProlog", "consultaVacia");
		}
		
		
		if(!result.hasErrors()) {
			System.out.println("session: " + session.getId());
			model.addAttribute("svgTreeSLD", generadorTree.generarSVG());
			
			String path = req.getServletContext().getRealPath("/prolog/" + session.getId() + ".pl");
			
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
					            new Term[] {new Atom(p.getQueryProlog()), new Atom(session.getId()), new Atom(path)} 
					        );
				
//				Query q2  = 
//				        new Query( 
//					            "tst", 
//					            new Term[] {new Atom("p"), new Atom("sourceUser")} 
//					        );
//				System.out.println("q2.hasSolution(): " + q2.hasSolution());
//				System.out.println("print0.5.");
				q2.allSolutions();
				System.out.println("print1.");
				q2.close();
				System.out.println("print2.");
			} catch (Exception e) {
				System.out.println("message: " + e.getMessage());
				e.printStackTrace();
			}
			
			Query q3 = new Query("arbol(X).");
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
		}
		
		model.addAttribute("hayPrev", new Boolean(false));
		model.addAttribute("hayNext", new Boolean(true));
		
		return "inicio";
	}
	
	@RequestMapping(params="nextStep")
	public String avanzarSLD(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//		model.addAttribute("hayNext", new Boolean(true));
		System.out.println("Next presionado1");
		model.addAttribute("hayPrev", new Boolean(true));
		model.addAttribute("hayNext", new Boolean(true));
		
		
		model.addAttribute("svgTreeSLD", generadorTree.generarSVG());
		
		return "inicio";
	}
	
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
	
//	@RequestMapping(params="options", method=RequestMethod.POST)
//	public String abrirOpciones(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//
//		return "inicio";
//	}
}
