<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://www.springframework.org/tags" prefix="spring"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<!DOCTYPE html>
<html>
<head>
<spring:url value="/resources" var="urlPublic"></spring:url>

<link rel="stylesheet" href="${urlPublic}/codemirror/lib/codemirror.css">
<link rel="stylesheet" href="${urlPublic}/codemirror/mode/prolog/prolog.css">

<link rel="stylesheet" href="${urlPublic}/treant/Treant.css">

<link rel="stylesheet" href="${urlPublic}/css/style.css">


<!-- Evaluar la posiblidad de usar require.js para organizar las depedencias e intentar acelerar la velocidad de carga.
Adem�s minimizar los archivos js utilizados, especialmente codemirror.js. Mirar como swish utiliza requirejs. -->
<script src="${urlPublic}/codemirror/lib/codemirror.js"></script>
<script src="${urlPublic}/codemirror/addon/edit/matchbrackets.js"></script>
<script src="${urlPublic}/codemirror/addon/mode/simple.js"></script>
<script src="${urlPublic}/codemirror/mode/prolog/prolog.js"></script>

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jquery.perfect-scrollbar/1.4.0/css/perfect-scrollbar.min.css">


<link rel="stylesheet"
	href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	crossorigin="anonymous"> <!-- Al ponerlo abajo del todo pareciera que la p�gina carga m�s ordenada. -->
	
<meta charset="ISO-8859-1">
<title>SLD Tree Generator</title>
</head>

<body>
	<div id="loadDiv" style="display: block; background-color:white; position:absolute; top:0px; left:0px; width:100%; height:100%; z-index:2000;"></div>
	<div class="container h-100">

		<h1>Bienvenidos.</h1>
		<div id="alert" class="alert alert-dismissible collapse">
				<a id="closeAlert" href="#" class="close" aria-label="close">&times;</a>
				<span></span>
		</div>
		<div class="row h-100">
			<div class="form-group col-lg-6 h-100" style="margin-bottom: 10rem;">
				<label for="program">Source code</label>
				<form id='program' class="h-100" autocomplete="off" spellcheck="false">
					<textarea id="sourceCode"></textarea>
					<%-- <form:errors path="sourceCode" cssClass="errorCode" /> --%>

					<div class="input-group mb-2">
						<div class="input-group-prepend" style="margin-right: -9px;">
							<div class="input-group-text">?-</div>
						</div>
						<input id="queryProlog"
							placeholder="example(X)." class="form-control"
							style="font-style: italic;" /> 
					</div>

					<%--  <form:errors path="queryProlog" cssClass="errorCode" /> --%>

					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<input class="btn btn-primary" type="submit" value="Create SLD" id="createButton"/>
						<button class="btn btn-primary" type="button" id="nextButton" style="display: none;">Next Solution</button>
						<button class="btn btn-primary" type="button" id="stopButton" style="display: none;">Stop</button>
						<button class="btn btn-primary" type="button" id="optionsButton" style="display: block;">Options</button>
					</div>
					


				<!-- Modal -->
				<div class="modal fade" id="modalOptions" tabindex="-1" role="dialog" aria-hidden="true">
					<div class="modal-dialog modal-dialog-centered modal-sm" role="document">
						<div class="modal-content">
							<div class="modal-header">
								<h5 class="modal-title">Opciones</h5>
								<button type="button" class="close" data-dismiss="modal" aria-label="Close">
									<span aria-hidden="true">&times;</span>
								</button>
							</div>
							<div class="modal-body">
								<div class="form-check">
									<input type="checkbox" id="occurs_check"/>
									<label class="form-check-label" for="occurs_check">
									  Occurs check
									</label>
								</div>
								<div class="form-check">
									<input type="checkbox" id="show_substitutions"/>
									<label class="form-check-label" for="show_substitutions">
									  View substitutions
									</label>
								</div>
							</div>
						</div>
					</div>
				</div>
				
				</form>



				<script>

				</script>
			</div>

			<div class="form-group col-lg-6 h-100" id="controlsAndViewTree">
				<label for="viewTree">SLDTree</label>

				<div class="contenedorGrafico h-100" id="viewTree">
					<div class="chart h-100" id="sldtree">

					</div>

					<button class="buttonInViewer" id="fullScreenButton">
						<img src="resources/img/fullscreen.svg">
					</button>

					<button class="buttonInViewer" id="reportButton" data-toggle="modal" data-target="#reportModal">
						<img src="resources/img/report.svg">
					</button>

				</div>

				<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
					<button class="btn btn-primary" type="button" name="status" id="statusButton">Status</button>
					<button class="btn btn-primary" type="button" name="nextStep" id="nextStepButton">Next step</button>
					<button class="btn btn-primary" type="button" name="skip" id="skipButton">Skip</button>
					<button class="btn btn-primary" type="button" name="skipAll" id="skipAllButton">Skip all</button>
				</div>

				<!-- Modal -->
				<div class="modal fade" id="reportModal" tabindex="-1" role="dialog" aria-hidden="true">
					<div class="modal-dialog" role="document">
					<div class="modal-content">
						<div class="modal-header">
						<h5 class="modal-title">Report error</h5>
						<button type="button" class="close" data-dismiss="modal" aria-label="Close">
							<span aria-hidden="true">&times;</span>
						</button>
						</div>
						<div class="modal-body">
							<label for="reportDescription">Description</label>
							<textarea class="form-control" id="reportDescription"></textarea>
						</div>
						<div class="modal-footer">
						<button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
						<button type="button" class="btn btn-primary" id="reportSend">Send</button>
						</div>
					</div>
					</div>
				</div>

				<!-- Modal -->
				<div class="modal fade" id="statusModal" tabindex="-1" role="dialog" aria-hidden="true">
					<div class="modal-dialog modal-dialog-centered modal-sm" role="document">
					<div class="modal-content">
						<div class="modal-header">
						<h5 class="modal-title">Current status</h5>
						<button type="button" class="close" data-dismiss="modal" aria-label="Close">
							<span aria-hidden="true">&times;</span>
						</button>
						</div>
						<div class="modal-body">
							<dl class="row">
								<dt class="col-sm-6">Status</dt>
								<dd id="estadoArbol" class="col-sm-6 text-monospace">Running</dd>
								
								<dt class="col-sm-6">Solutions</dt>
								<dd id="cantidadSoluciones" class="col-sm-6 text-monospace">0</dd>
								
								<dt class="col-sm-6 text-truncate">Nodes</dt>
								<dd id="cantidadNodos" class="col-sm-6 text-monospace">0</dd>
								
							</dl>
						</div>
					</div>
					</div>
				</div>

				<script>
					document.getElementById("nextStepButton").style.display = "none";
					document.getElementById("statusButton").style.display = "none";
					document.getElementById("skipAllButton").style.display = "none";
					document.getElementById("skipButton").style.display = "none";
				</script>
					
					
			</div>
			

		</div>

	</div>


	<script src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
	<script src="${urlPublic}/treant/vendor/jquery.easing.js"></script>
	<script src="${urlPublic}/treant/vendor/raphael.js"></script>
	<script src="${urlPublic}/treant/Treant.js"></script>
	
	<script src="${urlPublic}/treant/vendor/jquery.mousewheel.js"></script>
	<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery.perfect-scrollbar/1.4.0/perfect-scrollbar.min.js"></script>

	<script
		src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
		integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
		crossorigin="anonymous"></script>
		
	<script type="text/javascript">
		window.onload = function(){
			document.getElementById("loadDiv").style.display = "none";
		}
	</script>
	
	<script src="${urlPublic}/js/main.js"></script>

</body>
</html>