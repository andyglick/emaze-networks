package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Mask;
import org.junit.Test;

public class IpMaskToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'netmask':'255.255.255.0'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask bwn = new BeanWithNetmask();
        bwn.setNetmask(Mask.netV4(24));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void testIPv6MaskSerialization() throws IOException {
        final String expected = "{'netmask':'115'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask bwn = new BeanWithNetmask();
        bwn.setNetmask(Mask.netV6(115));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithNetmask {

        private Mask netmask;

        public Mask getNetmask() {
            return netmask;
        }

        public void setNetmask(Mask netmask) {
            this.netmask = netmask;
        }
    }
}
