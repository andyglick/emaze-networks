package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv4Network;
import org.junit.Test;

public class Ipv4NetworkToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'cidr':'10.0.0.0/8'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr bwc = new BeanWithCidr();
        bwc.setCidr(Ipv4Network.fromCidrNotation("10.0.0.0/8"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithCidr {

        private Ipv4Network cidr;

        public Ipv4Network getCidr() {
            return cidr;
        }

        public void setCidr(Ipv4Network cidr) {
            this.cidr = cidr;
        }
    }
}
