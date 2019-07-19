<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://www.springframework.org/tags" prefix="spring"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet"
	href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	crossorigin="anonymous">

<spring:url value="/resources" var="urlPublic"></spring:url>

<link rel="stylesheet" href="${urlPublic}/css/style.css">
<link rel="stylesheet" href="${urlPublic}/codemirror/lib/codemirror.css">
<link rel="stylesheet" href="${urlPublic}/codemirror/mode/prolog/prolog.css">

<script src="${urlPublic}/codemirror/lib/codemirror.js"></script>
<script src="${urlPublic}/codemirror/addon/edit/matchbrackets.js"></script>
<script src="${urlPublic}/codemirror/addon/mode/simple.js"></script>
<script src="${urlPublic}/codemirror/mode/prolog/prolog.js"></script>

<meta charset="ISO-8859-1">
<title>SLD Tree Generator</title>
</head>

<body>
	<div class="container">
		<h1>Bienvenidos.</h1>
		<div class="row">
			<div class="form-group col-lg-6">
				<label>Source code</label>
				<form:form modelAttribute="programaUsuario">
					<form:textarea path="sourceCode" id="sourceCode"
						style="white-space: pre; margin-top: 0px; margin-bottom: 0px; height: 280px;"
						cssClass="form-control rounded-10" />
					<form:errors path="sourceCode" cssClass="errorCode" />
					</br>
					<input class="btn btn-primary" type="submit" value="Crear SLD" />
				</form:form>
				<script>
					var editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
						lineNumbers : true,
						matchBrackets: true,
						theme: "prolog"
					});
				</script>
			</div>

			<div class="form-group col-lg-6">
				<label>SLDTree</label>
				<div class="form-control rounded-10" id="sldtree"
					style="margin-top: 0px; margin-bottom: 0px; height: 280px;">
					${svgTreeSLD}</div>
			</div>
		</div>

	</div>

</body>
</html>