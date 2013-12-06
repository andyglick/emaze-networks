package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6;
import org.junit.Test;

public class Ipv6ToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'ipv6':'0000:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6 bwi = new BeanWithIpv6();
        bwi.setIpv6(Ipv6.parse("::1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithIpv6 {

        private Ipv6 ipv6;

        public Ipv6 getIpv6() {
            return ipv6;
        }

        public void setIpv6(Ipv6 ipv6) {
            this.ipv6 = ipv6;
        }
    }
}
