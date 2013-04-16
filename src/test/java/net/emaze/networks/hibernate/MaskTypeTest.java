package net.emaze.networks.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import net.emaze.networks.Mask;
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
public class MaskTypeTest {

    @Autowired
    private HibernateOperations hibernateOperations;

    @Test
    public void canSerializeAndDeserializeANetwork() {
        final MaskContainer container = new MaskContainer();
        container.setMask(Mask.net(24));
        final Serializable id = hibernateOperations.save(container);

        hibernateOperations.execute(new HibernateCallback<MaskContainer>() {
            @Override
            public MaskContainer doInHibernate(Session session) throws HibernateException, SQLException {
                MaskContainer got = (MaskContainer) session.get(MaskContainer.class, id);
                Assert.assertEquals(Mask.net(24), got.getMask());
                return got;
            }
        });
    }

    @Entity
    @Table(name = "mask_container")
    public static class MaskContainer {

        @Id
        @GeneratedValue
        private Integer id;
        @Type(type = "net.emaze.networks.hibernate.MaskType")
        private Mask mask;

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Mask getMask() {
            return mask;
        }

        public void setMask(Mask mask) {
            this.mask = mask;
        }
    }
}
