package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv4;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.annotations.Type;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate3.HibernateCallback;
import org.springframework.orm.hibernate3.HibernateOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration(classes = InMemoryHibernateConfiguration.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class Ipv4TypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final IpContainer container = new IpContainer();
        container.setIp(Ipv4.parse("127.0.0.1"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<IpContainer>() {
            @Override
            public IpContainer doInHibernate(Session session) throws HibernateException, SQLException {
                IpContainer got = (IpContainer) session.get(IpContainer.class, id);
                Assert.assertEquals(Ipv4.parse("127.0.0.1"), got.getIp());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "ip_container")
    public static class IpContainer {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.Ipv4Type")
        private Ipv4 ip;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv4 getIp() {
            return ip;
        }

        public void setIp(Ipv4 ip) {
            this.ip = ip;
        }
    }
}
