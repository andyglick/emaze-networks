package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv6Network;
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
public class Ipv6NetworkTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeAIpv6Network() {
        final Ipv6NetworkContainer container = new Ipv6NetworkContainer();
        container.setNetwork(Ipv6Network.fromCidrNotation("::/0"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv6NetworkContainer>() {
            @Override
            public Ipv6NetworkContainer doInHibernate(Session session) throws HibernateException {
                Ipv6NetworkContainer got = (Ipv6NetworkContainer) session.get(Ipv6NetworkContainer.class, id);
                Assert.assertEquals(Ipv6Network.fromCidrNotation("::/0"), got.getNetwork());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "networkv6_container")
    public static class Ipv6NetworkContainer {

        @Id
        @GeneratedValue
        private Integer id;
        private Ipv6Network network;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv6Network getNetwork() {
            return network;
        }

        public void setNetwork(Ipv6Network network) {
            this.network = network;
        }
    }
}
