package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ip;
import org.junit.Test;

public class IpToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'ip':'127.0.0.1'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIp bwi = new BeanWithIp();
        bwi.setIp(Ip.parse("127.0.0.1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }

    @Test
    public void testIPv6Serialization() throws IOException {
        final String expected = "{'ip':'1234:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIp bwi = new BeanWithIp();
        bwi.setIp(Ip.parse("1234::1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }

    public static class BeanWithIp {

        private Ip ip;

        public Ip getIp() {
            return ip;
        }

        public void setIp(Ip ip) {
            this.ip = ip;
        }
    }
}
