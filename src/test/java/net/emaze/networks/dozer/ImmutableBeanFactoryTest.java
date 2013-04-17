package net.emaze.networks.dozer;

import net.emaze.networks.Ip;
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
        mapper.addMapping(ImmutableBeanFactoryTest.class.getResourceAsStream("/net/emaze/networks/dozer/network-mappings.xml"));
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
    

    public static class IpContainer extends Container<Ip>{};
    public static class MaskContainer extends Container<Mask>{};
    public static class NetworkContainer extends Container<Network>{};
    
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
