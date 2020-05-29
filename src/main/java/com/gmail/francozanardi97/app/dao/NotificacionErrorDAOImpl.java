package com.gmail.francozanardi97.app.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gmail.francozanardi97.app.domain.NotificacionError;

@Repository
public class NotificacionErrorDAOImpl implements NotificacionErrorDAO {

	@Autowired
	private DataSource datasource;
	
	@Override
	public NotificacionError get(long id) {
		return null;
	}

	@Override
	public List<NotificacionError> getAll() {
		return null;
	}

	@Override
	public void save(NotificacionError not) throws SQLException {
		String sql = "INSERT INTO notificacion_error(query, source_code, descripcion) VALUES(?, ?, ?)";
		Connection conn = datasource.getConnection();
		PreparedStatement ps = conn.prepareStatement(sql);
		ps.setString(1, not.getProgramaUsuario().getQueryProlog());
		ps.setString(2, not.getProgramaUsuario().getSourceCode());
		ps.setString(3, not.getDescripcion());
		ps.executeUpdate();
		ps.close();			
	}

	@Override
	public void update(NotificacionError not) {

	}

	@Override
	public void delete(int id) {
		
	}

}
