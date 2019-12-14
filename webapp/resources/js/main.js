var myID = null;
var treant = null;
var mapNodos = new Map(); //mapeo desde id nodo a objeto TreeNode
var mapRamasDisponibles = new Map();
//mapeo que contiene solo las ramas sin nodos hijos,
//es decir el mapeo va desde id rama a objeto TreeNode, el cual es un nodo invisible.

const SPEED_ANIMATION = 500;

var configTreeDefault = {
	chart: {
		container: "#sldtree",
		scrollbar: "fancy",
		//animateOnInit: true,
		
		animation: {
			nodeAnimation: "easeOutBounce",
			nodeSpeed: SPEED_ANIMATION,
			connectorsAnimation: "bounce",
			connectorsSpeed: SPEED_ANIMATION
		},
		
		callback: {		
			onCreateNode: function(newNode, newNodeDOM) {
				var redraw = false;
				var esperando = false;
				
				$(newNodeDOM.querySelector('p.node-name')).hover(function () {
					var self = this;
					esperando = true;	
					
					setTimeout(function() {
						if(esperando){
							var wOrig = $(self).outerWidth();
							
							$(self).css({'overflow' : 'visible'});
							$(self).css({'max-width' : 'fit-content'});
							
							if($(self).width() > wOrig){
								newNode.width = $(self).outerWidth();
								newNode.getTree().redraw();
								redraw = true;
							}
							
							esperando = false;
						}
					}, Math.max(newNode.getTree().CONFIG.animation.nodeSpeed, newNode.getTree().CONFIG.animation.connectorsSpeed));
						

				}, function () {
					if(!esperando){
						$(this).css({'overflow' : 'hidden'});
						$(this).css({'max-width' : '100px'});
						
						if(redraw){
							newNode.width = $(this).outerWidth();
							newNode.getTree().redraw();
							redraw = false;
						}
						
						hayCambios = false;
					}
					
					esperando = false;
				});
			},
	
		},
		
		connectors: {
			type: 'straight'
		},
		
		node: {
			collapsable: false
		},
		
	},
	
	nodeStructure: {
	}
	
};

var editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});



window.onbeforeunload = function(event) {
	if(myID){
		$.post('eliminarArbol', 'id='+myID);
		
		event.returnValue = "";
	}
}



$('#optionsButton').click(() => {
	$('#modalOptions').modal('show');
});

$('#stopButton').click(() => {
	$.post('eliminarArbol', 'id='+myID)
	.always(() => {
		document.getElementById("createButton").disabled = false;
		document.getElementById("createButton").style.display = "block";
		document.getElementById("optionsButton").style.display = "block";
		document.getElementById("nextButton").style.display = "none";
		document.getElementById("stopButton").style.display = "none";

		document.getElementById("nextStepButton").style.display = "none";
		document.getElementById("skipButton").style.display = "none";
		
		mapRamasDisponibles.clear();
		mapNodos.clear();
		
		myID = null;

		configTreeDefault.nodeStructure = {HTMLclass: "nodo_invisible"};
		
		treant.tree.reload();
		configTreeDefault.nodeStructure = {};

		// configTreeDefault.nodeStructure.HTMLclass = ""; //volvemos a dejarlo por defecto visible.
	});
});

$('#nextButton').click(() => {

	$('#nextButton').attr("disabled", true);
	
	$.post('avanzarFotograma', 'id='+myID)
	.then(fotActual => {
		return $.post('getNodos', 'id='+myID);
	})
	.then(nodos => {
		graficarNodos(nodos);
		
		if(nodos[0].rotulo === "[]"){ //es la cláusula vacía
			if(mapRamasDisponibles.size > 0){
				$('#nextButton').attr("disabled", false);
			}
			desactivarControlesNext();
			agregarSolucion(nodos[0]);
		} else if(nodos[0].rotulo === "[fail]" && mapRamasDisponibles.size == 0){
			desactivarControlesNext();
		} else {
			activarControlesNext();
		}
	});
	
	
});

$('#skipButton').click(() => {
	treant.tree.CONFIG.animation.connectorsSpeed = 0;
	treant.tree.CONFIG.animation.nodeSpeed = 0;

	nextFotograma((nodos) => {
		treant.tree.CONFIG.animation.connectorsSpeed = SPEED_ANIMATION;
		treant.tree.CONFIG.animation.nodeSpeed = SPEED_ANIMATION;

		if(nodos.length == 1 && nodos[0].rotulo === "[]"){ //es la cláusula vacía
			desactivarControlesNext();
			agregarSolucion(nodos[0]);
			
			if(mapRamasDisponibles.size > 0){
				document.getElementById("nextButton").disabled = false;
			}
		} else if(nodos.length == 1 && nodos[0].rotulo === "[fail]" && mapRamasDisponibles.size == 0){
			desactivarControlesNext();
		}
	});

});



$('#nextStepButton').click(() => {
	desactivarControlesNext();

	nextFotograma((nodos) => {
		var interval = setInterval(function() {
			let seDesactivaronControles = false;

			if(treant.tree.animacionesEnCurso == 0){
				if(nodos.length == 1 && nodos[0].rotulo === "[]"){ //es la cláusula vacía
					desactivarControlesNext();
					seDesactivaronControles = true;
					agregarSolucion(nodos[0]);
					
					if(mapRamasDisponibles.size > 0){
						document.getElementById("nextButton").disabled = false;
					}
				} else if(nodos.length == 1 && nodos[0].rotulo === "[fail]" && mapRamasDisponibles.size == 0){
					desactivarControlesNext();
					seDesactivaronControles = true;
				}

				if(!seDesactivaronControles){
					activarControlesNext();
				}
				
				clearInterval(interval);
			}


		}, 100);
	});
});


$('#program').submit(
		function(evento) {

			var s = "sourceCode=" + encodeURIComponent(editor.getValue()) + "&queryProlog="
					+ encodeURIComponent($('#queryProlog').val());

			document.getElementById("createButton").disabled = true;

			$.post('', s)
				.then(id => {
					myID = id;
					
					return $.post('getRaiz', 'id='+myID);
				})
				.then(raiz => {
					crearArbol(raiz);

					document.getElementById("createButton").style.display = "none";
					document.getElementById("optionsButton").style.display = "none";
					document.getElementById("nextButton").style.display = "block";
					document.getElementById("stopButton").style.display = "block";
		
					document.getElementById("nextStepButton").style.display = "block";
					document.getElementById("skipButton").style.display = "block";
					
					document.getElementById("nextButton").disabled = true;
					document.getElementById("nextStepButton").disabled = false;
					document.getElementById("skipButton").disabled = false;


				})
				.catch((error) => {
					console.log('error: ', error);
					showAlertError('¡Se ha producido un fallo interno!', error.responseText.length < 60 ? error.responseText : (error.responseText.substring(0, 60) + '...'));
					document.getElementById("createButton").disabled = false;
				})

			evento.preventDefault();
		}
);


function agregarSolucion(nodo){
	$.post('getSolucion', 'id='+myID, (solucion) => {
		
		if(solucion.rotulo !== "" && solucion.rotulo !== "[]"){
			var nodoSolucion = {
					text: {
						name: solucion.rotulo
					}
			};
			
			treant.tree.addNode(mapNodos.get(nodo.id), nodoSolucion);
		}

	});
	

}


function nextFotograma(callback){
	$.post('avanzarFotograma', 'id='+myID)
	.then(() => {
		return $.post('getRamas', 'id='+myID);
	})
	.then(ramas => {
		console.log('ramas: ', ramas);
		
		graficarRamas(ramas);
		return $.post('getRamasCut', 'id='+myID);
	})
	.then(ramasCut => {
		console.log('ramasCut: ', ramasCut);
		
		graficarRamasCut(ramasCut);
		return $.post('getNodos', 'id='+myID);
	})
	.then(nodos => {
		console.log('nodos: ', nodos);
		
		graficarNodos(nodos);

		if(callback && typeof(callback) === "function"){
			callback(nodos);
		}
	});


}

function crearArbol(nodo){
	console.log('nodo.rotulo: ', nodo.rotulo);
	
	configTreeDefault.nodeStructure.text = {
		name: nodo.rotulo
	};

	configTreeDefault.nodeStructure.HTMLid = 'nodo_' + nodo.id;
	
	if(treant){
		treant.tree.reload();
	} else {
		treant = new Treant(configTreeDefault, null, $);
	}

	mapNodos.set(0, treant.tree.nodeDB.db[0]);
	
}

function graficarRamas(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		let nodeInvisible = {
				text: {
					name: "______"
				},
				HTMLclass: "nodo_invisible",
				HTMLid: "rama_" + rama.id
		};
				
		let nuevoNodoInvisible = treant.tree.addNode(mapNodos.get(rama.padre), nodeInvisible);
		
		mapRamasDisponibles.set(rama.id, nuevoNodoInvisible);
	}
}

function graficarNodos(nodos){
	for(let i in nodos){
		let nodo = nodos[i];
		
		let nodoInvisible = mapRamasDisponibles.get(nodo.idRama);
		
		nodoInvisible.nodeDOM.id = "nodo_" + nodo.id;
		$(nodoInvisible.nodeDOM).removeClass('nodo_invisible');
		/*
		 * La siguiente instrucción se debe a que el quitarle la clase 'nodo_invisible' puede modificar la altura del 'div'
		 * (si se agregase borde al remover esta clase, por ejemplo).
		 * El hecho de que se llame antes de setTextName, es que sabemos que este método ejecutará un redraw(), por lo tanto evitamos
		 * ejecutarlos nosotros para actualizar el cambio en la altura.
		 */
		
		nodoInvisible.height = nodoInvisible.nodeDOM.offsetHeight+1;
		nodoInvisible.setTextName(nodo.rotulo);
		
		mapRamasDisponibles.delete(nodo.idRama);
		mapNodos.set(nodo.id, nodoInvisible);
	}
}

function graficarRamasCut(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		mapRamasDisponibles.get(rama.id).cutRama();
		mapRamasDisponibles.delete(rama.id);
	}
}

function configButtons_fullScreen(){
	
	//si todos son nulls entonces no está en fullscreen.
	function isFullScreenOn() {
		return document.fullScreenElement || document.webkitFullscreenElement || document.msFullscreenElement || document.mozFullScreenElement;
	}
	
	function acomodarTree() {
		setTimeout(function() {
			treant.tree.redraw();
		}, 500);
	}
	
	function configExitFullScreen(){
		
		function exitFullScreen() {
			if(!isFullScreenOn()){ 
				btnFullscreen.style.cssText = "display: block;";
				btnreport.style.cssText = "display: block;";
				viewTree.style.cssText = "height: 100%!important;";
				acomodarTree();
			}
		} 
		
		/* Standard syntax */
		document.addEventListener("fullscreenchange", exitFullScreen);
		/* Firefox */
		document.addEventListener("mozfullscreenchange", exitFullScreen);
		/* Chrome, Safari and Opera */
		document.addEventListener("webkitfullscreenchange",  exitFullScreen);
		/* IE / Edge */
		document.addEventListener("msfullscreenchange",  exitFullScreen);
		
	}
	
	function addListnerToFullscreen(){
		btnFullscreen.addEventListener("click", () => {
			btnFullscreen.style.cssText = "display: none;";
			btnreport.style.cssText = "display: none;";
			viewTree.style.cssText = "height: 90%!important;";

			var controlsAndView = document.getElementById('controlsAndViewTree');
			
			if (controlsAndView.requestFullscreen) {
				controlsAndView.requestFullscreen();
			} else if (controlsAndView.mozRequestFullScreen) { /* Firefox */
				controlsAndView.mozRequestFullScreen();
			} else if (controlsAndView.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
				controlsAndView.webkitRequestFullscreen();
			} else if (controlsAndView.msRequestFullscreen) { /* IE/Edge */
				controlsAndView.msRequestFullscreen();
			}
			
			acomodarTree();
		});
	}

	function configHover(){
		$(viewTree).hover(() => {
			if(!isFullScreenOn() && mapNodos.size > 0){
				btnFullscreen.style.cssText = "display: block;";
				btnreport.style.cssText = "display: block;";
			}
		}, () => {
			btnFullscreen.style.cssText = "display: none;";
			btnreport.style.cssText = "display: none;";
		});
	}
	
	var btnFullscreen, btnreport, viewTree;

	btnFullscreen = document.getElementById('fullScreenButton');
	btnreport = document.getElementById('reportButton');
	viewTree = document.getElementById('viewTree');

	
	configHover();
	addListnerToFullscreen();
	configExitFullScreen();	
}

function activarControlesNext(){
	$('#nextStepButton').attr("disabled", false);
	$('#skipButton').attr("disabled", false);
}

function desactivarControlesNext(){
	$('#nextStepButton').attr("disabled", true);
	$('#skipButton').attr("disabled", true);
}

$("#reportSend").click(() => {
	$.post('notificarError', 'id='+myID+"&descripcion_error="+encodeURIComponent($("#reportDescription").val()))
	.done(() => {
		showAlertSuccess('¡Error enviado!', 'Gracias por reportar el error.');
	}).fail(() => {
		showAlertError('¡Se ha producido un fallo interno!', 'Inténtelo más tarde.');
	}).always(() => {
		$('#reportModal').modal('hide');
		$('#reportDescription').val("");
		
	})

});

$("#closeAlert").click(() => {
	$("#alert").hide();
})

function showAlertError(title, body){
	$('#alert').removeClass('alert-success');
	$('#alert').addClass('alert-danger');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}

function showAlertSuccess(title, body){
	$('#alert').removeClass('alert-danger');
	$('#alert').addClass('alert-success');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}


configButtons_fullScreen(); //esto no tendría que crearlo cada vez que creo un árbol
//además cuando hago stop tendría que desactivar la función del hover en el div sldtree.

//t.tree.addNode(nodoPadre, nuevoNodo);

/*
 * obtener ubicacion de rama -> sea N el nodo hijo de la rama:
 * M estará en la posición: n.parent().connectorPoint(true);
 * L estará en la posición: n.connectorPoint(false);
 * 
 * Siendo M y L elemento del path en SVG html.

*/

// t.tree.connectionStore[3].attr("stroke", "red"); permite cambiar el color de una rama a rojo. 
// Esto lo podemos usar a la hora de hacer un cut a una rama y querer cambiar su color a gris.

/*
 * Formas de hacer un nodo visible:
 * 	- Sabiendo su HTMLid, lo buscamos en el DOM y seteamos className.
 *  - Guardando una referencia al TreeNode, de esta manera si n fuera el TreeNode haríamos:
 *  	n.nodeDOM.className = ...
 *  
 *  Para cambiar el texto necesitamos una referencia al nodo, o nuevamente un id para buscarlo en el tree.
 *  En este último caso usariamos: t.tree.nodeDB.db[id], si el id es igual al indice del arreglo o buscaríamos elemento a elemento.
 * 
 *  Para agregar o remover las css clases mejor usar jquery:  $(this).addClass('className'),  $(this).removeClass('className');
 * 
*/
