package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Mask;
import org.junit.Test;

public class Ipv6MaskToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'netmask':24}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Mask bwn = new BeanWithIpv6Mask();
        bwn.setNetmask(Ipv6Mask.net(24));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithIpv6Mask {

        private Ipv6Mask netmask;

        public Ipv6Mask getNetmask() {
            return netmask;
        }

        public void setNetmask(Ipv6Mask netmask) {
            this.netmask = netmask;
        }
    }
}
