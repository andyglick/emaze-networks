package net.emaze.networks.hibernate;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.ipv6.Ipv6Mask;
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
public class Ipv6MaskTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;
    @Autowired
    private TransactionTemplate tx;

    @Test
    public void canSerializeAndDeserializeAIpv6Network() {
        final Ipv6MaskContainer container = new Ipv6MaskContainer();
        container.setMask(Ipv6Mask.net(100));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(session -> {
            Ipv6MaskContainer got = (Ipv6MaskContainer) session.get(Ipv6MaskContainer.class, id);
            Assert.assertEquals(Ipv6Mask.net(100), got.getMask());
            return got;
        });
    }

    @Test
    public void canUseMaskAsPrimaryKey() {
        final Ipv6MaskKeyContainer container = new Ipv6MaskKeyContainer();
        container.setMask(Ipv6Mask.net(100));
        final Serializable id = tx.execute((state) -> {
            return hibernateOperations.save(container);
        });
        hibernateOperations.execute(session -> {
            Ipv6MaskKeyContainer got = (Ipv6MaskKeyContainer) session.get(Ipv6MaskKeyContainer.class, id);
            Assert.assertEquals(Ipv6Mask.net(100), got.getMask());
            return got;
        });
    }

    @Entity
    @Table(name = "maskv6_container")
    public static class Ipv6MaskContainer {

        @Id
        @GeneratedValue
        private Integer id;
        private Ipv6Mask mask;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Ipv6Mask getMask() {
            return mask;
        }

        public void setMask(Ipv6Mask mask) {
            this.mask = mask;
        }
    }

    @Entity
    @Table(name = "maskv6_key_container")
    public static class Ipv6MaskKeyContainer {

        @Id
        private Ipv6Mask mask;

        public Ipv6Mask getMask() {
            return mask;
        }

        public void setMask(Ipv6Mask mask) {
            this.mask = mask;
        }
    }
}
