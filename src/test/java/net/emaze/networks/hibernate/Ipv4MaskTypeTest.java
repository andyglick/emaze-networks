package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Ipv4Mask;
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
public class Ipv4MaskTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeAIpv4Network() {
        final Ipv4MaskContainer container = new Ipv4MaskContainer();
        container.setMask(Ipv4Mask.net(24));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<Ipv4MaskContainer>() {
            @Override
            public Ipv4MaskContainer doInHibernate(Session session) throws HibernateException {
                Ipv4MaskContainer got = (Ipv4MaskContainer) session.get(Ipv4MaskContainer.class, id);
                Assert.assertEquals(Ipv4Mask.net(24), got.getMask());
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
        @Type(type = "net.emaze.networks.hibernate.Ipv4MaskType")
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
}
