package net.emaze.networks.hibernate;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.ipv4.Ipv4Network;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate4.HibernateOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.support.TransactionTemplate;

@ContextConfiguration(classes = InMemoryHibernateConfiguration.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class Ipv4NetworkTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;
    @Autowired
    private TransactionTemplate tx;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final Ipv4NetworkContainer container = new Ipv4NetworkContainer();
        container.setNetwork(Ipv4Network.fromCidrNotation("0.0.0.0/0"));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(session -> {
            Ipv4NetworkContainer got = (Ipv4NetworkContainer) session.get(Ipv4NetworkContainer.class, id);
            Assert.assertEquals(Ipv4Network.fromCidrNotation("0.0.0.0/0"), got.getNetwork());
            return got;
        });
    }

    @Test
    public void canUseNetworkAsPrimaryKey() {
        final Ipv4NetworkKeyContainer container = new Ipv4NetworkKeyContainer();
        container.setId(Ipv4Network.fromCidrNotation("0.0.0.0/0"));
        final Serializable id = tx.execute((status) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(session -> {
            Ipv4NetworkKeyContainer got = (Ipv4NetworkKeyContainer) session.get(Ipv4NetworkKeyContainer.class, id);
            Assert.assertEquals(Ipv4Network.fromCidrNotation("0.0.0.0/0"), got.getId());
            return got;
        });
    }

    @Entity
    @Table(name = "networkv4_container")
    public static class Ipv4NetworkContainer {

        @Id
        @GeneratedValue
        private Integer id;
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

    @Entity
    @Table(name = "networkv4_key_container")
    public static class Ipv4NetworkKeyContainer {

        @Id
        private Ipv4Network id;

        public Ipv4Network getId() {
            return id;
        }

        public void setId(Ipv4Network id) {
            this.id = id;
        }
    }
}
