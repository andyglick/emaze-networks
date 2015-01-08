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
import org.springframework.transaction.support.TransactionTemplate;

@ContextConfiguration(classes = InMemoryHibernateConfiguration.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class Ipv4TypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;
    @Autowired
    private TransactionTemplate tx;

    @Test
    public void canSerializeAndDeserializeAIpv4() {
        final Ipv4Container container = new Ipv4Container();
        container.setIp(Ipv4.parse("127.0.0.1"));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(new HibernateCallback<Ipv4Container>() {
            @Override
            public Ipv4Container doInHibernate(Session session) throws HibernateException {
                Ipv4Container got = (Ipv4Container) session.get(Ipv4Container.class, id);
                Assert.assertEquals(Ipv4.parse("127.0.0.1"), got.getIp());
                return got;
            }
        });
    }

    @Test
    public void canUseIpAsPrimaryKey() {
        final Ipv4KeyContainer container = new Ipv4KeyContainer();
        container.setIp(Ipv4.parse("127.0.0.1"));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(session -> {
            Ipv4KeyContainer got = (Ipv4KeyContainer) session.get(Ipv4KeyContainer.class, id);
            Assert.assertEquals(Ipv4.parse("127.0.0.1"), got.getIp());
            return got;
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

    @Entity
    @Table(name = "ipv4_key_container")
    public static class Ipv4KeyContainer {

        @Id
        private Ipv4 ip;

        public Ipv4 getIp() {
            return ip;
        }

        public void setIp(Ipv4 ip) {
            this.ip = ip;
        }
    }

}
