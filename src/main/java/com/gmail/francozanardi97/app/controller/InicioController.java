package com.gmail.francozanardi97.app.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import form.ProgramaUsuario;
import treeSLD.TreeSLD;

@Controller
@RequestMapping(value="/")
public class InicioController {
	
//	@Autowired mejor no lo hacemos un bean, creemos que no es correcto.
	private TreeSLD generadorTree;

	@RequestMapping(method=RequestMethod.GET)
	public String prepararForm(Model model) {
		model.addAttribute("programaUsuario", new ProgramaUsuario());
		
		return "inicio";
	}
	
	@RequestMapping(method=RequestMethod.POST)
	public String procesarForm(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
		// Por ahora la validación de errores la hacemos acá, pero se puede separar y usar otra clase, creo que sería más correcto. Leer spring.pdf.
		
		if(p.getSourceCode() == "") {
			result.rejectValue("sourceCode", "codeEmpty");
		}
		
		if(p.getQueryProlog() == "") {
			result.rejectValue("queryProlog", "consultaVacia");
		}
		
		
		if(!result.hasErrors()) {
			generadorTree = new TreeSLD(p);
			model.addAttribute("svgTreeSLD", generadorTree.generarSVG());
		}
		
		return "inicio";
	}
}
