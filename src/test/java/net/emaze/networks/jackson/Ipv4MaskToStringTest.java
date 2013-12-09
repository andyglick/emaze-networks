package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv4Mask;
import org.junit.Test;

public class Ipv4MaskToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'netmask':24}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask bwn = new BeanWithNetmask();
        bwn.setNetmask(Ipv4Mask.net(24));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithNetmask {

        private Ipv4Mask netmask;

        public Ipv4Mask getNetmask() {
            return netmask;
        }

        public void setNetmask(Ipv4Mask netmask) {
            this.netmask = netmask;
        }
    }
}
