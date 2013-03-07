package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Netmask;
import org.junit.Test;

public class NetmaskToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'netmask':24}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask bwn = new BeanWithNetmask();
        bwn.setNetmask(Netmask.fromBits(24));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithNetmask {

        private Netmask netmask;

        public Netmask getNetmask() {
            return netmask;
        }

        public void setNetmask(Netmask netmask) {
            this.netmask = netmask;
        }
    }
}
