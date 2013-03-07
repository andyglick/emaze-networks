package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv4;
import org.junit.Test;

public class Ipv4ToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'ipv4':'127.0.0.1'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4 bwi = new BeanWithIpv4();
        bwi.setIpv4(Ipv4.parse("127.0.0.1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithIpv4 {

        private Ipv4 ipv4;

        public Ipv4 getIpv4() {
            return ipv4;
        }

        public void setIpv4(Ipv4 ipv4) {
            this.ipv4 = ipv4;
        }
    }
}
