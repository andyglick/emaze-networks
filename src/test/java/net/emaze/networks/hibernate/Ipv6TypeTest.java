package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv6;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.annotations.Type;
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
public class Ipv6TypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeAIpv6() {
        final Ipv6Container container = new Ipv6Container();
        container.setIp(Ipv6.parse("2001:0DB8:0000:CD31::"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv6Container>() {
            
            @Override
            public Ipv6Container doInHibernate(Session session) throws HibernateException {
                Ipv6Container got = (Ipv6Container) session.get(Ipv6Container.class, id);
                Assert.assertEquals(Ipv6.parse("2001:0DB8:0000:CD31::"), got.getIp());
                return got;
            }
        });
    }
   
    @Entity
    @Table(name = "ipv6_container")
    public static class Ipv6Container {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.Ipv6Type")
        private Ipv6 ip;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv6 getIp() {
            return ip;
        }

        public void setIp(Ipv6 ip) {
            this.ip = ip;
        }
    }
}
