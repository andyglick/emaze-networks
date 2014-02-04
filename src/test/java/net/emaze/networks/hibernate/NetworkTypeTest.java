package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Network;
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
public class NetworkTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final NetworkContainer container = new NetworkContainer();
        container.setNetwork(Network.fromCidrNotation("0.0.0.0/0"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<NetworkContainer>() {
            @Override
            public NetworkContainer doInHibernate(Session session) throws HibernateException, SQLException {
                NetworkContainer got = (NetworkContainer) session.get(NetworkContainer.class, id);
                Assert.assertEquals(Network.fromCidrNotation("0.0.0.0/0"), got.getNetwork());
                return got;
            }
        });
    }

    @Test
    public void canSerializeAndDeserializeAIpv6Network() {
        final NetworkContainer container = new NetworkContainer();
        container.setNetwork(Network.fromCidrNotation("::/0"));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<NetworkContainer>() {
            @Override
            public NetworkContainer doInHibernate(Session session) throws HibernateException, SQLException {
                NetworkContainer got = (NetworkContainer) session.get(NetworkContainer.class, id);
                Assert.assertEquals(Network.fromCidrNotation("::/0"), got.getNetwork());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "network_container")
    public static class NetworkContainer {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.NetworkType")
        private Network network;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Network getNetwork() {
            return network;
        }

        public void setNetwork(Network network) {
            this.network = network;
        }
    }
}
