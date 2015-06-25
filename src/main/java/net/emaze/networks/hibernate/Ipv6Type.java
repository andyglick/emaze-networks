package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import net.emaze.networks.ipv6.Ipv6;
import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.usertype.UserType;

public class Ipv6Type implements UserType {

    public static final int STORED_SQL_TYPE = Types.VARCHAR;

    @Override
    public int[] sqlTypes() {
        return new int[]{STORED_SQL_TYPE};
    }

    @Override
    public Class returnedClass() {
        return Ipv6.class;
    }

    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        return x == null ? y == null : x.equals(y);
    }

    @Override
    public int hashCode(Object x) throws HibernateException {
        return x == null ? 0 : x.hashCode();
    }

    @Override
    public Object nullSafeGet(ResultSet rs, String[] names, SessionImplementor si, Object owner) throws HibernateException, SQLException {
        final String stored = rs.getString(names[0]);
        if (rs.wasNull()) {
            return null;
        }
        return Ipv6.parse(stored);
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index, SessionImplementor si) throws HibernateException, SQLException {
        if (value == null) {
            st.setNull(index, STORED_SQL_TYPE);
        } else {
            st.setString(index, value.toString());
        }
    }

    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    @Override
    public boolean isMutable() {
        return false;
    }

    @Override
    public Serializable disassemble(Object value) throws HibernateException {
        return value.toString();
    }

    @Override
    public Object assemble(Serializable cached, Object owner) throws HibernateException {
        return Ipv6.parse((String) cached);
    }

    @Override
    public Object replace(Object original, Object target, Object owner) throws HibernateException {
        return original;
    }
}
