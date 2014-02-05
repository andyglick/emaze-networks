package net.emaze.networks.dozer;

import net.emaze.networks.Ipv4;
import net.emaze.networks.Ipv4Mask;
import net.emaze.networks.Ipv4Network;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class ImmutableBeanFactoryTest {

    private static final DozerBeanMapper mapper = new DozerBeanMapper();

    @BeforeClass
    public static void setup() {
        mapper.addMapping(new NetworksBeanMappings());
    }

    @Test
    public void canMapIPv4() {
        final Ipv4 got = mapper.map(Ipv4.parse("127.0.0.1"), Ipv4.class);
        Assert.assertEquals(Ipv4.parse("127.0.0.1"), got);
    }

    @Test
    public void canMapContainedIPv4() {
        final Ipv4Container got = mapper.map(Ipv4Container.of(Ipv4.parse("127.0.0.1")), Ipv4Container.class);
        Assert.assertEquals(Ipv4.parse("127.0.0.1"), got.getValue());
    }

    @Test
    public void canMapIPv4Ipv4Mask() {
        final Ipv4Mask got = mapper.map(Ipv4Mask.net("255.255.255.0"), Ipv4Mask.class);
        Assert.assertEquals(Ipv4Mask.net(24), got);
    }

    @Test
    public void canMapContainedIPv4Ipv4Mask() {
        final Ipv4MaskContainer got = mapper.map(Ipv4MaskContainer.of(Ipv4Mask.net("255.255.255.0")), Ipv4MaskContainer.class);
        Assert.assertEquals(Ipv4Mask.net(24), got.getValue());
    }

    @Test
    public void canMapIPv4Ipv4Network() {
        final Ipv4Network got = mapper.map(Ipv4Network.fromCidrNotation("127.0.0.0/16"), Ipv4Network.class);
        Assert.assertEquals(Ipv4Network.fromCidrNotation("127.0.0.0/16"), got);
    }

    @Test
    public void canMapContainedIPv4Ipv4Network() {
        final Ipv4NetworkContainer got = mapper.map(Ipv4NetworkContainer.of(Ipv4Network.fromCidrNotation("127.0.0.0/16")), Ipv4NetworkContainer.class);
        Assert.assertEquals(Ipv4Network.fromCidrNotation("127.0.0.0/16"), got.getValue());
    }

    @Test
    public void canMapIPv6() {
        final Ipv6 got = mapper.map(Ipv6.parse("::1"), Ipv6.class);
        Assert.assertEquals(Ipv6.parse("::1"), got);
    }

    @Test
    public void canMapContainedIPv6() {
        final Ipv6Container got = mapper.map(Ipv6Container.of(Ipv6.parse("::1")), Ipv6Container.class);
        Assert.assertEquals(Ipv6.parse("::1"), got.getValue());
    }

    @Test
    public void canMapIPv6Ipv6Mask() {
        final Ipv6Mask got = mapper.map(Ipv6Mask.net(64), Ipv6Mask.class);
        Assert.assertEquals(Ipv6Mask.net(64), got);
    }

    @Test
    public void canMapContainedIPv6Ipv6Mask() {
        final Ipv6MaskContainer got = mapper.map(Ipv6MaskContainer.of(Ipv6Mask.net(64)), Ipv6MaskContainer.class);
        Assert.assertEquals(Ipv6Mask.net(64), got.getValue());
    }

    @Test
    public void canMapIPv6Ipv6Network() {
        final Ipv6Network got = mapper.map(Ipv6Network.fromCidrNotation("::/64"), Ipv6Network.class);
        Assert.assertEquals(Ipv6Network.fromCidrNotation("::/64"), got);
    }

    @Test
    public void canMapContainedIPv6Ipv6Network() {
        final Ipv6NetworkContainer got = mapper.map(Ipv6NetworkContainer.of(Ipv6Network.fromCidrNotation("::/64")), Ipv6NetworkContainer.class);
        Assert.assertEquals(Ipv6Network.fromCidrNotation("::/64"), got.getValue());
    }

    public static class Ipv4Container extends Container<Ipv4> {
    };

    public static class Ipv4MaskContainer extends Container<Ipv4Mask> {
    };

    public static class Ipv4NetworkContainer extends Container<Ipv4Network> {
    };

    public static class Ipv6Container extends Container<Ipv6> {
    };

    public static class Ipv6MaskContainer extends Container<Ipv6Mask> {
    };

    public static class Ipv6NetworkContainer extends Container<Ipv6Network> {
    };

    public static class Container<T> {

        private T value;

        public static <T> Container of(T value) {
            final Container<T> c = new Container<>();
            c.setValue(value);
            return c;
        }

        public T getValue() {
            return value;
        }

        public void setValue(T value) {
            this.value = value;
        }
    }
}
