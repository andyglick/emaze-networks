package net.emaze.networks.hibernate;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv4;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate4.HibernateCallback;
import org.springframework.orm.hibernate4.HibernateOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration(classes = InMemoryHibernateConfiguration.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class Ipv4TypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeAIpv4() {
        final Ipv4Container container = new Ipv4Container();
        container.setIp(Ipv4.parse("127.0.0.1"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv4Container>() {
            @Override
            public Ipv4Container doInHibernate(Session session) throws HibernateException {
                Ipv4Container got = (Ipv4Container) session.get(Ipv4Container.class, id);
                Assert.assertEquals(Ipv4.parse("127.0.0.1"), got.getIp());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "ipv4_container")
    public static class Ipv4Container {

        @Id
        @GeneratedValue
        private Integer id;
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
