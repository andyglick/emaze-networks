package net.emaze.networks.hibernate;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv4Mask;
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
public class Ipv4MaskTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;
    @Autowired
    private TransactionTemplate tx;

    @Test
    public void canSerializeAndDeserializeAIpv4Network() {
        final Ipv4MaskContainer container = new Ipv4MaskContainer();
        container.setMask(Ipv4Mask.net(24));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(new HibernateCallback<Ipv4MaskContainer>() {
            @Override
            public Ipv4MaskContainer doInHibernate(Session session) throws HibernateException {
                Ipv4MaskContainer got = (Ipv4MaskContainer) session.get(Ipv4MaskContainer.class, id);
                Assert.assertEquals(Ipv4Mask.net(24), got.getMask());
                return got;
            }
        });
    }

    @Test
    public void canUseMaskAsPrimaryKey() {
        final Ipv4MaskKeyContainer container = new Ipv4MaskKeyContainer();
        container.setId(Ipv4Mask.net(24));
        final Serializable id = tx.execute((status) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(new HibernateCallback<Ipv4MaskKeyContainer>() {
            @Override
            public Ipv4MaskKeyContainer doInHibernate(Session session) throws HibernateException {
                Ipv4MaskKeyContainer got = (Ipv4MaskKeyContainer) session.get(Ipv4MaskKeyContainer.class, id);
                Assert.assertEquals(Ipv4Mask.net(24), got.getId());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "maskv4_container")
    public static class Ipv4MaskContainer {

        @Id
        @GeneratedValue
        private Integer id;
        private Ipv4Mask mask;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv4Mask getMask() {
            return mask;
        }

        public void setMask(Ipv4Mask mask) {
            this.mask = mask;
        }
    }

    @Entity
    @Table(name = "maskv4_key_container")
    public static class Ipv4MaskKeyContainer {

        @Id
        private Ipv4Mask id;

        public Ipv4Mask getId() {
            return id;
        }

        public void setId(Ipv4Mask id) {
            this.id = id;
        }
    }
}
