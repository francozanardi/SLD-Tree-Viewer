// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

/* Example definition of a simple mode that understands a subset of
 * JavaScript:
 * Sacado de acá: https://codemirror.net/demo/simplemode.html
 * Para entender expresiones regulares en js: https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions#special-word-boundary
 */

// En token ponemos el nombre del estilo que va a tener.
// el estilo está definido en prolog.css, 

var builtinCuerpo = "(as|is|true|false|fail|repeat|forall|findall|member|length|last|nth0|nth1|not|concat_atom|" +
					"flatten|min|max|reverse|maplist|sublist|append|assertz|retract|retractall|asserta|assert|" +
					"write|writeln|nl|between|read|clause|functor|arg|nonvar|var|sleep)";

var builtinStart = "(dynamic|use_module|module|if)";


CodeMirror.defineSimpleMode("prolog", {
	
	//estado inicial. No se encuentra en ningún estado en particular, por lo que está fuera de un parámetro, predicado, string, comentario, etc.
	start: [
		{regex: /\%.*$/, token: "comment"},
	    {regex: /\/\*/, token: "comment", push: "comment"},
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
	    
	    
		{regex: new RegExp("(\\w)"+builtinStart+"|(?:"+builtinStart+"\\b)"), token: [null, null, "builtin"], push: "parametros"},	
		{regex: /\:\-|\-\-\>/, token: "keyword"},
		
	    {regex: /[a-z]{1}\w*/, token: "predicate", push: "parametros"},


	],
	
	//parámetros en la definición de un predicado o parámetros en el llamado a un builtin desde fuera del cuerpo de un predicado.
	parametros: [
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
		{regex: /\d+(\.\d+){0,1}/, token: "number"},
	    
		{regex: /\./, token: "keyword", pop: true},
		{regex: /\(/, token: null, push: "parametros"},
		{regex: /\)/, token: null, pop: true},
		{regex: /(\W){1}([A-Z_]\w*)|([^A-Z_][A-Z_]\w*)|(^[A-Z_]\w*)/, token: [null, "variable", null, "variable"]},
		{regex: /\:\-|\-\-\>/, token: "keyword", indent: true, next: "cuerpo"},
	],
	
	//cuerpo de un predicado.
	cuerpo: [
		{regex: /\%.*$/, token: "comment"},
	    {regex: /\/\*/, token: "comment", push: "comment"}, 
	    {regex: /\\"/, token: null},
	    {regex: /"/, token: "string", push: "string1"},
	    {regex: /\\'/, token: null},
	    {regex: /'/, token: "string", push: "string2"},
	    {regex: /\d+(\.\d+){0,1}/, token: "number"},
	    
		{regex: /\(/, token: null, push: "cuerpo"},
		{regex: /\)/, token: null, pop: true},
		{regex: /(\W){1}([A-Z_]\w*)|([^A-Z_][A-Z_]\w*)|(^[A-Z_]\w*)/, token: [null, "variable", null, "variable"]},
		{regex: /\\\+|\-\>|\;|\,|\!|\=\.\.|\|/, token: "keyword"},
		{regex: /\./, token: "keyword", dedent: true, pop: true},

		//evitar repetir código trae el problema de que no reconoce la palabra clave si esta es el primer caracter del estado cuerpo.
		{regex: new RegExp("(\\w)"+builtinCuerpo+"|(?:"+builtinCuerpo+"\\b)"), token: [null, null, "builtin"]},
		{regex: /\\\+|\-\>|\;|\,|\!/, token: "keyword"},

	],
	
	//string con comilla doble ".
	string1: [
		{regex: /[^\\"]"/, token: "string", pop: true},
		{regex: /\\"/, token: "string"},
		{regex: /[^"]/, token: "string"},
		{regex: /^"/, token: "string", pop: true},
	],
	
	//string con comilla simple '.
	string2: [
		{regex: /[^\\']'/, token: "string", pop: true},
		{regex: /\\'/, token: "string"},
		{regex: /[^']/, token: "string"},
		{regex: /^'/, token: "string", pop: true},
	],
	
	//comentario en bloque.
	comment: [
		{regex: /.*?\*\//, token: "comment", pop: true}, 
		{regex: /.*/, token: "comment"}
	]

	
	
});
