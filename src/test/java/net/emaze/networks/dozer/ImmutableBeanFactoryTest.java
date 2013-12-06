package net.emaze.networks.dozer;

import net.emaze.networks.Ip;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import net.emaze.networks.Mask;
import net.emaze.networks.Network;
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
    public void canMapIp() {
        final Ip got = mapper.map(Ip.parse("127.0.0.1"), Ip.class);
        Assert.assertEquals(Ip.parse("127.0.0.1"), got);
    }

    @Test
    public void canMapContainedIp() {
        final IpContainer got = mapper.map(IpContainer.of(Ip.parse("127.0.0.1")), IpContainer.class);
        Assert.assertEquals(Ip.parse("127.0.0.1"), got.getValue());
    }

    @Test
    public void canMapMask() {
        final Mask got = mapper.map(Mask.parse("255.255.255.0"), Mask.class);
        Assert.assertEquals(Mask.net(24), got);
    }

    @Test
    public void canMapContainedMask() {
        final MaskContainer got = mapper.map(MaskContainer.of(Mask.parse("255.255.255.0")), MaskContainer.class);
        Assert.assertEquals(Mask.net(24), got.getValue());
    }

    @Test
    public void canMapNetwork() {
        final Network got = mapper.map(Network.fromCidrNotation("127.0.0.0/16"), Network.class);
        Assert.assertEquals(Network.fromCidrNotation("127.0.0.0/16"), got);
    }

    @Test
    public void canMapContainedNetwork() {
        final NetworkContainer got = mapper.map(NetworkContainer.of(Network.fromCidrNotation("127.0.0.0/16")), NetworkContainer.class);
        Assert.assertEquals(Network.fromCidrNotation("127.0.0.0/16"), got.getValue());
    }

    @Test
    public void canMapIpv6() {
        final Ipv6 got = mapper.map(Ipv6.parse("::1"), Ipv6.class);
        Assert.assertEquals(Ipv6.parse("::1"), got);
    }

    @Test
    public void canMapContainedIpv6() {
        final Ipv6Container got = mapper.map(Ipv6Container.of(Ipv6.parse("::1")), Ipv6Container.class);
        Assert.assertEquals(Ipv6.parse("::1"), got.getValue());
    }

    @Test
    public void canMapIpv6Mask() {
        final Ipv6Mask got = mapper.map(Ipv6Mask.net(64), Ipv6Mask.class);
        Assert.assertEquals(Ipv6Mask.net(64), got);
    }

    @Test
    public void canMapContainedIpv6Mask() {
        final Ipv6MaskContainer got = mapper.map(Ipv6MaskContainer.of(Ipv6Mask.net(64)), Ipv6MaskContainer.class);
        Assert.assertEquals(Ipv6Mask.net(64), got.getValue());
    }

    @Test
    public void canMapIpv6Network() {
        final Ipv6Network got = mapper.map(Ipv6Network.fromCidrNotation("::/64"), Ipv6Network.class);
        Assert.assertEquals(Ipv6Network.fromCidrNotation("::/64"), got);
    }

    @Test
    public void canMapContainedIpv6Network() {
        final Ipv6NetworkContainer got = mapper.map(Ipv6NetworkContainer.of(Ipv6Network.fromCidrNotation("::/64")), Ipv6NetworkContainer.class);
        Assert.assertEquals(Ipv6Network.fromCidrNotation("::/64"), got.getValue());
    }

    public static class IpContainer extends Container<Ip> {
    };

    public static class MaskContainer extends Container<Mask> {
    };

    public static class NetworkContainer extends Container<Network> {
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
