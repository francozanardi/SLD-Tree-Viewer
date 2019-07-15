<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>

<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet"
	href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	crossorigin="anonymous">

<spring:url value="/resources" var="urlPublic"></spring:url>

<link rel="stylesheet" href="${urlPublic}/css/style.css">
<meta charset="ISO-8859-1">
<title>SLD Tree Generator</title>
</head>

<body>
	<div class="container">
		<h1>Bienvenidos.</h1>
		<div class="row">
			<div class="form-group col-lg-6">
				<label>Source code</label>
				<textarea class="form-control rounded-10" id="sourceCode"
					style="margin-top: 0px; margin-bottom: 0px; height: 280px;"></textarea>
			</div>

			<div class="form-control rounded-10" id="sldtree"
				style="margin-top: 0px; margin-bottom: 0px; height: 280px;">
				<canvas id="canvas" width="400" height="250"></canvas>
			</div>
		</div>

		<div class="row">
			<p>
				<a class="btn btn-primary" data-toggle="collapse"
					href="#collapseExample" role="button" aria-expanded="false"
					aria-controls="collapseExample"> Link with href </a>
				<button class="btn btn-primary" type="button" data-toggle="collapse"
					data-target="#collapseExample" aria-expanded="false"
					aria-controls="collapseExample">Button with data-target</button>
			</p>
			<div class="collapse" id="collapseExample">
				<div class="card card-body">Anim pariatur cliche
					reprehenderit, enim eiusmod high life accusamus terry richardson ad
					squid. Nihil anim keffiyeh helvetica, craft beer labore wes
					anderson cred nesciunt sapiente ea proident.</div>
			</div>
		</div>

	</div>

</body>
</html>