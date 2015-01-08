package net.emaze.networks.hibernate;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv6;
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
public class Ipv6TypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;
    @Autowired
    private TransactionTemplate tx;

    @Test
    public void canSerializeAndDeserializeAIpv6() {
        final Ipv6Container container = new Ipv6Container();
        container.setIp(Ipv6.parse("2001:0DB8:0000:CD31::"));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });

        hibernateOperations.execute(new HibernateCallback<Ipv6Container>() {

            @Override
            public Ipv6Container doInHibernate(Session session) throws HibernateException {
                Ipv6Container got = (Ipv6Container) session.get(Ipv6Container.class, id);
                Assert.assertEquals(Ipv6.parse("2001:0DB8:0000:CD31::"), got.getIp());
                return got;
            }
        });
    }

    @Test
    public void canUseIpAsPrimaryKey() {
        final Ipv6KeyContainer container = new Ipv6KeyContainer();
        container.setIp(Ipv6.parse("2001:0DB8:0000:CD31::"));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });

        hibernateOperations.execute(new HibernateCallback<Ipv6KeyContainer>() {

            @Override
            public Ipv6KeyContainer doInHibernate(Session session) throws HibernateException {
                Ipv6KeyContainer got = (Ipv6KeyContainer) session.get(Ipv6KeyContainer.class, id);
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

    @Entity
    @Table(name = "ipv6_key_container")
    public static class Ipv6KeyContainer {

        @Id
        private Ipv6 ip;

        public Ipv6 getIp() {
            return ip;
        }

        public void setIp(Ipv6 ip) {
            this.ip = ip;
        }
    }
}
