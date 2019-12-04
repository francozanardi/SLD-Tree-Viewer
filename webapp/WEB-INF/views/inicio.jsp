<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://www.springframework.org/tags" prefix="spring"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<!DOCTYPE html>
<html>
<head>
<spring:url value="/resources" var="urlPublic"></spring:url>

<link rel="stylesheet" href="${urlPublic}/codemirror/lib/codemirror.css">
<link rel="stylesheet" href="${urlPublic}/codemirror/mode/prolog/prolog.css">

<link rel="stylesheet" href="${urlPublic}/treant/Treant.css">
<link rel="stylesheet" href="${urlPublic}/treant/vendor/perfect-scrollbar/perfect-scrollbar.css">

<link rel="stylesheet" href="${urlPublic}/css/style.css">


<!-- Evaluar la posiblidad de usar require.js para organizar las depedencias e intentar acelerar la velocidad de carga.
Además minimizar los archivos js utilizados, especialmente codemirror.js. Mirar como swish utiliza requirejs. -->
<script src="${urlPublic}/codemirror/lib/codemirror.js"></script>
<script src="${urlPublic}/codemirror/addon/edit/matchbrackets.js"></script>
<script src="${urlPublic}/codemirror/addon/mode/simple.js"></script>
<script src="${urlPublic}/codemirror/mode/prolog/prolog.js"></script>




<link rel="stylesheet"
	href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	crossorigin="anonymous"> <!-- Al ponerlo abajo del todo pareciera que la página carga más ordenada. -->
	
<meta charset="ISO-8859-1">
<title>SLD Tree Generator</title>
</head>

<body>
	<div id="loadDiv" style="display: block; background-color:white; position:absolute; top:0px; left:0px; width:100%; height:100%; z-index:2000;"></div>
	<div class="container h-100">
		<h1>Bienvenidos.</h1>
		<div class="row h-100">
			<div class="form-group col-lg-6 h-100" style="margin-bottom: 10rem;">
				<label>Source code</label>
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
<!-- 							<div class="modal-footer">
								<button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
								<button type="button" class="btn btn-primary">Save changes</button>
							</div> -->
						</div>
					</div>
				</div>
				
				</form>



				<script>

				</script>
			</div>

			<div class="form-group col-lg-6 h-100">
				<label>SLDTree</label>
				<div class="chart h-100" id="sldtree">
				
				</div>
				
					<div class="btn-group col-lg-12" style="margin-bottom: 15px; padding-left: 0px; padding-right: 0px;" role="group">
						<button class="btn btn-primary" type="button" name="prevStep" id="prevStepButton">Prev step</button>
						<button class="btn btn-primary" type="button" name="nextStep" id="nextStepButton">Next step</button>
						<button class="btn btn-primary" type="button" name="skip" id="skipButton">Skip</button>
					</div>
						
					<script>				
						document.getElementById("prevStepButton").style.display = "none";
						document.getElementById("nextStepButton").style.display = "none";
						document.getElementById("skipButton").style.display = "none";
					</script>
					
				<span id="mySpan"></span>	
			</div>
			

		</div>

	</div>


	<script src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
	<script src="${urlPublic}/treant/vendor/jquery.easing.js"></script>
	<script src="${urlPublic}/treant/vendor/raphael.js"></script>
	<script src="${urlPublic}/treant/Treant.js"></script>
	
	<script src="${urlPublic}/treant/vendor/jquery.mousewheel.js"></script>
	<script src="${urlPublic}/treant/vendor/perfect-scrollbar/perfect-scrollbar.js"></script>

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