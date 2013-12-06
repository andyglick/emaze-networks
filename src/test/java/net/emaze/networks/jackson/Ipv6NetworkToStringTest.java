package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Network;
import org.junit.Test;

public class Ipv6NetworkToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'cidr':'0000:0000:0000:0000:0000:0000:0000:0000/64'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr bwc = new BeanWithCidr();
        bwc.setCidr(Ipv6Network.fromCidrNotation("::/64"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithCidr {

        private Ipv6Network cidr;

        public Ipv6Network getCidr() {
            return cidr;
        }

        public void setCidr(Ipv6Network cidr) {
            this.cidr = cidr;
        }
    }
}
