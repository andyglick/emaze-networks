package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv6Mask;
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
public class Ipv6MaskTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final Ipv6MaskContainer container = new Ipv6MaskContainer();
        container.setMask(Ipv6Mask.net(24));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv6MaskContainer>() {
            @Override
            public Ipv6MaskContainer doInHibernate(Session session) throws HibernateException, SQLException {
                Ipv6MaskContainer got = (Ipv6MaskContainer) session.get(Ipv6MaskContainer.class, id);
                Assert.assertEquals(Ipv6Mask.net(24), got.getMask());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "mask_container")
    public static class Ipv6MaskContainer {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.Ipv6MaskType")
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
}
