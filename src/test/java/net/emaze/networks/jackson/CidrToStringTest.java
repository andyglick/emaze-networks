package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Cidr;
import org.junit.Test;

public class CidrToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'cidr':'10.0.0.0/8'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithCidr bwc = new BeanWithCidr();
        bwc.setCidr(Cidr.parse("10.0.0.0/8"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithCidr {

        private Cidr cidr;

        public Cidr getCidr() {
            return cidr;
        }

        public void setCidr(Cidr cidr) {
            this.cidr = cidr;
        }
    }
}
