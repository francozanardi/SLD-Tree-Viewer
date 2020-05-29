package com.gmail.francozanardi97.app.dao;

import java.sql.SQLException;
import java.util.List;

import com.gmail.francozanardi97.app.domain.NotificacionError;

public interface NotificacionErrorDAO {
	NotificacionError get(long id);
    
    List<NotificacionError> getAll();
     
    void save(NotificacionError not) throws SQLException;
     
    void update(NotificacionError not);
     
    void delete(int id);
}
