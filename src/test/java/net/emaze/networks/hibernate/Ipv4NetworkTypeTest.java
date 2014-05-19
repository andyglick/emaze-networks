package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv4Network;
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
public class Ipv4NetworkTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final Ipv4NetworkContainer container = new Ipv4NetworkContainer();
        container.setNetwork(Ipv4Network.fromCidrNotation("0.0.0.0/0"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv4NetworkContainer>() {
            @Override
            public Ipv4NetworkContainer doInHibernate(Session session) throws HibernateException {
                Ipv4NetworkContainer got = (Ipv4NetworkContainer) session.get(Ipv4NetworkContainer.class, id);
                Assert.assertEquals(Ipv4Network.fromCidrNotation("0.0.0.0/0"), got.getNetwork());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "networkv4_container")
    public static class Ipv4NetworkContainer {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.Ipv4NetworkType")
        private Ipv4Network network;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv4Network getNetwork() {
            return network;
        }

        public void setNetwork(Ipv4Network network) {
            this.network = network;
        }
    }
}
