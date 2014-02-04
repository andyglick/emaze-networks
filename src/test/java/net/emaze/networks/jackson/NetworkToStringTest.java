package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Network;
import org.junit.Test;

public class NetworkToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'cidr':'10.0.0.0/8'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr bwc = new BeanWithCidr();
        bwc.setCidr(Network.fromCidrNotation("10.0.0.0/8"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void testIPv6Serialization() throws IOException {
        final String expected = "{'cidr':'1234:0000:0000:0000:0000:0000:0000:0000/16'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr bwc = new BeanWithCidr();
        bwc.setCidr(Network.fromCidrNotation("1234::/16"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithCidr {

        private Network cidr;

        public Network getCidr() {
            return cidr;
        }

        public void setCidr(Network cidr) {
            this.cidr = cidr;
        }
    }
}
