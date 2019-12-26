import './controles.js';
import './graficador.js';
import './fullscreen.js';
import './report.js';
import './alerts.js';

window.editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});