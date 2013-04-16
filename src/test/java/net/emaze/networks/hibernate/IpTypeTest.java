package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ip;
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
public class IpTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final IpContainer container = new IpContainer();
        container.setIp(Ip.parse("127.0.0.1"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<IpContainer>() {
            @Override
            public IpContainer doInHibernate(Session session) throws HibernateException, SQLException {
                IpContainer got = (IpContainer) session.get(IpContainer.class, id);
                Assert.assertEquals(Ip.parse("127.0.0.1"), got.getIp());
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
        @Type(type = "net.emaze.networks.hibernate.IpType")
        private Ip ip;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ip getIp() {
            return ip;
        }

        public void setIp(Ip ip) {
            this.ip = ip;
        }
    }
}
